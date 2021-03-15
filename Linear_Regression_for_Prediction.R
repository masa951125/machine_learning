rm(list=ls())
library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

# residual sum of squares
mean((avg - test_set$son)^2)

fit <- lm(son ~ father, data = train_set)
fit$coef

#regression line and residual sum of squares (squared loss)
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

?predict.lm

#comprehension test
library(tidyverse)
library(caret)

#Q1
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")

RMSE_100 <- replicate(n,{
  test_index <- createDataPartition(dat$y, p= 0.5, list = F,times = 1)
  train_set <- dat %>% slice(-test_index)
  test_set <-dat %>% slice(test_index)
  fit <- lm(y~x, data=train_set)
  y_hat <-predict(fit, test_set)
  RMSE(test_set$y, y_hat)
})

mean(RMSE_100)
sd(RMSE_100)

#Q2

fun <- function(num){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(num, c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y"))
  results <- replicate(100,{
    test_index <- createDataPartition(dat$y, p= 0.5, list = F,times = 1)
    train_set <- dat %>% slice(-test_index)
    test_set <-dat %>% slice(test_index)
    fit <- lm(y~x, data=train_set)
    y_hat <-predict(fit, test_set)
    RMSE(test_set$y, y_hat)
  })
  my_list <- list("mean"= mean(results), "sd"=sd(results), "num"=num)
  return(my_list)
}
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1, sample.kind="Rounding")
sapply(n, fun)


rmse <-function(n){
  replicate(100,{
  test_index <- createDataPartition(dat$y, p= 0.5, list = F,times = 1)
  train_set <- dat %>% slice(-test_index)
  test_set <-dat %>% slice(test_index)
  fit <- lm(y~x, data=train_set)
  y_hat <-predict(fit, test_set)
  RMSE(test_set$y, y_hat)
})
}


set.seed(1, sample.kind="Rounding")
sapply(n, rmse)

#Q3
set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1, sample.kind="Rounding")

RMSE_100 <- replicate(n,{
  test_index <- createDataPartition(dat$y, p= 0.5, list = F,times = 1)
  train_set <- dat %>% slice(-test_index)
  test_set <-dat %>% slice(test_index)
  fit <- lm(y~x, data=train_set)
  y_hat <-predict(fit, test_set)
  RMSE(test_set$y, y_hat)
})

mean(RMSE_100)
sd(RMSE_100)

#Q6
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


#answers method    

  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x_1, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  fit <- lm(y ~ x_1 + x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
#Q8
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x_1, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  fit <- lm(y ~ x_1 + x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2)) 
