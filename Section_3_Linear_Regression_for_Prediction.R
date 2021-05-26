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
  
#Regression for a Categorical Outcome 

  library(dslabs)
  data("heights")
  y <- heights$height  
  set.seed(2, sample.kind = "Rounding") 
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- heights %>% slice(-test_index)
  test_set <- heights %>% slice(test_index)
  
  train_set %>% 
    filter(round(height)==66) %>%
    summarize(y_hat = mean(sex=="Female"))
  
  heights %>% 
    mutate(x = round(height)) %>%
    group_by(x) %>%
    filter(n() >= 10) %>%
    summarize(prop = mean(sex == "Female")) %>%
    ggplot(aes(x, prop)) +
    geom_point()

  lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
  p_hat <- predict(lm_fit, test_set)
  y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
  confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]
  
  library(caret)
  
  heights %>% 
    mutate(x = round(height)) %>%
    group_by(x) %>%
    filter(n() >= 10) %>%
    summarize(prop = mean(sex == "Female")) %>%
    ggplot(aes(x, prop)) +
    geom_point() + 
    geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])
  
  range(p_hat)
  
  # fit logistic regression model
  glm_fit <- train_set %>% 
    mutate(y = as.numeric(sex == "Female")) %>%
    glm(y ~ height, data=., family = "binomial") 
  glm_fit
  
  p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
  
  tmp <- heights %>% 
    mutate(x = round(height)) %>%
    group_by(x) %>%
    filter(n() >= 10) %>%
    summarize(prop = mean(sex == "Female")) 
  
  logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
    mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
  tmp %>% 
    ggplot(aes(x, prop)) +
    geom_point() +
    geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)
  
  y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
  confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]
  
#2 or 7 problem
library(dslabs)

    mnist <- read_mnist()
  is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
  titles <- c("smallest","largest")
  tmp <- lapply(1:2, function(i){
    expand.grid(Row=1:28, Column=1:28) %>%
      mutate(label=titles[i],
             value = mnist$train$images[is[i],])
  }) 

  library(dslabs)    
  data("mnist_27")
  mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
  
  

#Q1
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()



set.seed(1, sample.kind="Rounding")
mu_1 <- seq(0, 3, len=25)


fit_glm <-dat$train %>% glm(y~x, family="binomial",data=.)
p_hat_glm <- predict(fit_glm, dat$test)
y_hat_glm <- factor(ifelse(p_hat_glm>0.5, 1,0))
confusionMatrix(data = y_hat_glm, reference = dat$test$y)$overall["Accuracy"]

###
set.seed(1, sample.kind="Rounding")
delta <- seq(0, 3, len=25)

res <- sapply(delta, function(d){
  dat <- make_data(mu_1 =d)
  fit_glm <-  glm(y~x, family="binomial",data=dat$train)
  p_hat_glm <- predict(fit_glm, dat$test)
  y_hat_glm <- ifelse(p_hat_glm>0.5, 1, 0) %>% factor(c(1,0))
  mean(y_hat_glm== dat$test$y)
})

qplot(mu_1, res)