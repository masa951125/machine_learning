library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  pull(title)

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()


#Here are the 10 best movies according to our estimate:
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

#here are the 10 worst:
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)

#Look at how often they are rated.
train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)

#regularized estimates of b_i using λ=3
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())


#make a plot of the regularized estimates versus the least squares estimates.
tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

#The top 10 best movies based on the penalized estimates
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  pull(title)

tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
predicted_ratings[is.na(predicted_ratings)]<-3

RMSE(predicted_ratings, test_set$rating)

lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  predicted_ratings[is.na(predicted_ratings)]<-3
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses) 

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  predicted_ratings[is.na(predicted_ratings)]<-3
  return(RMSE(predicted_ratings, test_set$rating))
})
lambdas[which.min(rmses)]

#comprehension check
rm(list=ls())
options(digits=7)

set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))
qplot(n)
median(n)
qplot(rt(1000, 5))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1
schools[which.max(schools$score),]
schools %>% top_n(10, score) %>% arrange(-score)

#Q2
median(schools$size)
topschools <- schools %>% top_n(10, score) %>% arrange(-score)
median(topschools$size)

schools %>% top_n(10, score) %>%.$size %>% median()

#Q3
schools %>% top_n(-10, score) %>% .$size %>% median()

#Q4
schools %>% top_n(10, quality) %>% ggplot(aes(size, score)) +
  geom_point() +geom_smooth()

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha=0.5) +geom_point(data=filter(schools,rank <=10), col="red")

schools %>% top_n(10, score) %>% arrange(desc(score))

#Q5
overall <- mean(sapply(scores, mean))

schools <-schools %>% mutate(reg_score = overall + 
                               sapply(sapply(scores, function(scores){
  scores-overall}),sum)/(schools$size+25))

schools %>% top_n(10, reg_score)%>% arrange(-reg_score)

#answer
alpha <- 25

reg_score <- sapply(scores, function(x)  
  overall + sum(x-overall)/(length(x)+alpha))

schools %>% mutate(reg_score = reg_score) %>%
  top_n(10, reg_score) %>% arrange(desc(reg_score))

schools %>% ggplot(aes(size,reg_score)) +geom_point()

#Q6
alpha <- seq(10:250)

reg_score <- sapply(scores, function(x)  
  overall + sum(x-overall)/(length(x)+alpha))


RMSE_schools <- sapply(alpha, function(x){
  reg_score <- sapply(scores, function(y)overall + sum(y-overall)/(length(y)+x))
  sqrt(mean((reg_score-schools$quality)^2))
}) 
which.min(RMSE_schools)
min(RMSE_schools)

#answer
RMSE <- sapply(alphas, function(alpha){
  reg_score <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((reg_score - schools$quality)^2))
})

#Q7
reg_score <- sapply(scores, function(x)  
  overall + sum(x-overall)/(length(x)+135))

schools %>% mutate(reg_score = reg_score) %>%  
  top_n(10, reg_score) %>% arrange(desc(reg_score))

#Q8
RMSE_schools <- sapply(alpha, function(x){
  reg_score <- sapply(scores, function(y) overall + sum(y-overall)/(length(y)+x))
  sqrt(mean((reg_score-schools$quality)^2))
}) 
which.min(RMSE)
min(RMSE_schools)

alphas <- seq(10,250)
RMSE <- sapply(alphas, function(alpha){
  reg_score <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((reg_score - schools$quality)^2))
})

min(RMSE)
plot(alphas, RMSE)
alphas[which.min(RMSE)]