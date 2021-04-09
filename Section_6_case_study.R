rm(list=ls())

#1
library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123, sample.kind = "Rounding")
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#2
library(matrixStats)
library(tidyverse)

sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

#put a name on columns
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

#k-fold cross validation
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)

#test in small numbers
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[ ,col_index], y,  k = 3)
ggplot(train_knn)

y_hat_knn <- predict(fit_knn,x_test[, col_index],type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]

#random forest
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)

fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

install.packages("rafalib")
rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}
train_rf$bestTune

#variable importance
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp

?importance

image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

#ensemble
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)

#comprehension check

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
fits

#Q2
pred <-sapply(fits, function(model){
  y_pred <-predict(model, mnist_27$test)
})
dim(pred)

#Q3
accuracy <-sapply(fits, function(model){
  y_pred <-predict(model, mnist_27$test)
  mean(y_pred==mnist_27$test$y)
})

mean(accuracy)

#Q4
y_en <-ifelse(rowSums(pred[1:200,]=="7")>5,"7","2")%>%factor
mean(y_en==mnist_27$test$y)

#answer
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5
which(accuracy >0.81)

#Q6
sum(fits$glm$results$Accuracy
     +fits$lda$results$Accuracy
     +min(fits$naive_bayes$results$Accuracy)
     +fits$svmLinear$results$Accuracy
     +min(fits$knn$results$Accuracy)
     +fits$gamLoess$results$Accuracy
     +min(fits$multinom$results$Accuracy)
     +fits$qda$results$Accuracy
     +fits$rf$results$Accuracy
     +min(fits$adaboost$results$Accuracy))/10

acc_hat <- sapply(fits, function(fit)min(fit$results$Accuracy))

#Q7
which(acc_hat>=0.8)

ind <-which(acc_hat>=0.8)
selected_models <-models[ind]
selected_fits <-fits[ind]

selected_pred <-sapply(selected_fits, function(model){
  y_pred <-predict(model, mnist_27$test)
})

pred[ind]

y_en <-ifelse(rowMeans(pred[,ind]=="7")>0.5,"7","2")%>%factor
mean(y_en==mnist_27$test$y)

#answer
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)