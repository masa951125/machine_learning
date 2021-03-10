library(tidyverse)
install.packages("e1071")
install.packages("caret")
library(caret)
library(dslabs)
library(purr)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#comprehension check
data(mnist)
read_mnist()

mnist <- read_mnist()
ncol(mnist$train$images)

# tabulate each combination of prediction and actual value
table(predicted= y_hat, actual= test_set$sex)

#calculating accuracy
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")
prev
confusionMatrix(data= y_hat,reference = test_set$sex)

# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

F_meas(data= y_hat,reference = test_set$sex)

p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

install.packages("ggrepel")
library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#comprehension check part1

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

sum(dat$sex=="Female" & dat$type=="inclass")/sum(dat$type=="inclass")
sum(dat$sex=="Female" & dat$type=="online")/sum(dat$type=="online")

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

#Find accuracy
y_hat <- ifelse(x=="online", "Male", "Female") %>%
factor(levels= levels(y))
mean(y_hat==y)

y_hat <- ifelse(dat$type=="inclass", "Female", "Male") %>%
  factor(levels=levels(y))
mean(y_hat==dat$sex)

confusionMatrix(y_hat,y)
table(y_hat,y)
sensitivity(y_hat,y)
specificity(y_hat,y)

mean(dat$sex=="Female")

#comprehension check part2
library(caret)
library(purrr)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")
class(y)

#train$Sepal.Length
range(train$Sepal.Length)

cutoff_SL <-seq(5.0,7.9, by=0.1)
accuracy <- map_dbl(cutoff_SL, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor")
  factor(levels= levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

#train$Sepal.Width
range(train$Sepal.Width)

cutoff_SW <-seq(2.0,3.8, by=0.1)
accuracy <- map_dbl(cutoff_SW, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica","versicolor")
  factor(levels= levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

#train$Petal.Length
range(train$Petal.Length)

cutoff_PL <-seq(3.0,6.9, by=0.1)
accuracy <- map_dbl(cutoff_PL, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica","versicolor")
  factor(levels= levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)


#train$Petal.Width
range(train$Petal.Width)

cutoff_PW <-seq(1.0,2.5, by=0.1)
accuracy <- map_dbl(cutoff_PW, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor")
  factor(levels= levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

#simple code
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

#calculating overall accuracy

best_cutoff_PL <- cutoff_PL[which.max(accuracy)]
best_cutoff_PL

y_hat <- ifelse(test$Petal.Length > best_cutoff_PL, "virginica","versicolor")%>%
  factor(levels= levels(test$Species))
mean(y_hat == test$Species)
