rm(list=ls())

library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
train_gda <- train(y ~ ., method = "qda", data = mnist_27$train)
train_rpart <- train(y ~ ., method = "rpart", data = mnist_27$train)
plot(train_knn)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

getModelInfo("knn")
modelLookup("loess")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

train_knn$bestTune
train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

train_knn$results$AccuracySD

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

#comprehension check
rm(list=ls())
data("tissue_gene_expression")
library(rpart)
modelLookup("rpart")

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
set.seed(1991, sample.kind="Rounding")

train_rpart <- train(x,y, method="rpart", 
                     tuneGrid= data.frame(cp=seq(0, 0.1, 0.01)) )
ggplot(train_rpart)


train_rpart <- train(x,y, method="rpart", 
                     tuneGrid= data.frame(cp=seq(0, 0.1, 0.01)),
                     control = rpart.control(minsplit = 0) )

train_rpart$results$Accuracy

ggplot(train_rpart)
confusionMatrix(train_rpart)

plot(train_rpart$finalModel,margin=0.1)
text(train_rpart$finalModel, cex=0.6)

set.seed(1991, sample.kind="Rounding")
train_rf <- train(x,y, method="rf", 
                     tuneGrid= data.frame(mtry =seq(50, 200, 25)),nodesize = 1)
train_rf$bestTune
fit <-train_rf
ggplot(fit)
imp <-varImp(fit)

fit_rpart <- train_rpart
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

class(imp)
dat <-data.frame(imp$importance) %>%
  mutate(rank = rank(-imp$importance))
dat["CFHR4",]

dat <-dat %>% arrange(-dat)
sum(dat$Overall> dat["CFHR4",])



tibble(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)