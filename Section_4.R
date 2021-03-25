library(tidyverse)
library(dslabs)

mnist <- read_mnist()
set.seed(0) # if using R 3.5 or earlier
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))

#comprehension check

library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)

d <-dist(tissue_gene_expression$x)
d[1]
d[2]
d[39]
d[40]
d[73]
d[74]

ind <- c(1,2,39,40,73,74)
as.matrix(d)[ind,ind]
image(as.matrix(d))

#knn

library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k= 3)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#fit knn with k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

accuracy %>%mutate(k=ks) %>%
  gather(set, accuracy, test, train) %>%
  ggplot(aes(k, accuracy, color=set))+
  geom_point()+ 
  geom_line()
  
#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)

#comprehension check Q1

set.seed(1, sample.kind="Rounding")
data("heights")

class(heights$sex)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(heights$sex, p=0.5, list = FALSE, times=1) 
train_set <-heights %>%slice(-test_index)
test_set <- heights%>%slice(test_index)

ks <- seq(1, 101, 3)

F_1 <- sapply(ks, function(k){
  fit_knn <- knn3(sex ~ height, data = train_set, k = k)
  y_hat_knn <- predict(fit_knn, test_set, type = "class")
  F_meas(data= y_hat_knn, reference = factor(test_set$sex))
})

max(F_1)
ks[which.max(F_1)]

#comprehension check Q2

library(dslabs)
library(caret)
data("tissue_gene_expression")



x <- as.data.frame(tissue_gene_expression$x)

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(tissue_gene_expression$y, p=0.5, list = FALSE, times=1) 

train_set <-x [-test_index,]
test_set <- x[test_index,]
train_set_y <- tissue_gene_expression$y[-test_index]
test_set_y <-tissue_gene_expression$y[test_index]

ks = seq(1, 11, 2)
Accuracy <- sapply(ks, function(k){
  fit <- knn3(train_set,train_set_y ,k=k)
  y_hat <- predict(fit, test_set, type = "class")
  mean(y_hat==test_set_y)
})
Accuracy

