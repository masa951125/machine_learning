rm(list=ls())

library(tidyverse)
library(dslabs)
data("movielens")

movielens %>% as_tibble()

movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)

tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")


library(caret)
set.seed(755, sample.kind = "Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#1 simple model

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(test_set)) #predict rate is 3
RMSE(test_set$rating, predictions)

results <- tibble(method = "Just the average", RMSE = naive_rmse)

#2 b_i 

#fit <- lm(rating ~ as.factor(movieId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

qplot(b_i,data=movie_avgs, bins=10, color = I("black"))

predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by="movieId")%>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings,test_set$rating)

results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

#b_u
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
                     
results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
results %>% knitr::kable()

#comprehension check
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

#Q1
movielens %>% 
  group_by(movieId)%>%
  summarize(n=n(), year = as.character(first(year))) %>%
  ggplot(aes(year, sqrt(n), group=year)) + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Q2
recent_movies <- movielens %>% 
  filter(year >=1993) %>%
  group_by(movieId) %>%
  summarize(title= title[1], n = n(), 
            mean= mean(rating),avg =n/(2018-first(year))) %>%
  top_n(25, avg) %>%
  arrange(-avg) 

#Q3
recent_movies <- movielens %>% 
  filter(year >=1993) %>%
  group_by(movieId) %>%
  summarize(title= title[1], n = n(), 
            mean= mean(rating),avg =n/(2018-first(year)))%>%
  ggplot(aes(avg, mean)) +  geom_point() +geom_smooth()

#Q4
mean(recent_movies$mean)

#Q5 Q6
movielens <- movielens %>% mutate(date =as_datetime(timestamp))

movielens %>%
  mutate (week =round_date(movielens$date,unit = "week")) %>%
  group_by(week) %>%
  summarize(ave = mean(rating)) %>%
  ggplot(aes(week, ave)) +geom_point() +geom_smooth()

#Q8
unique(movielens$genres)

dat <- movielens %>%
  group_by(genres) %>% mutate(n =n()) %>%
  filter(n >1000) %>%
  summarize(genres= first(genres), ave= mean(rating), se= sd(rating)/sqrt(n())) %>%
  ggplot(aes(x=genres, y=ave, ymin =ave-2*se, ymax= ave+2*se)) +
  geom_point()+  geom_errorbar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
