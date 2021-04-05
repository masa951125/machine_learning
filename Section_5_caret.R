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


#Titanic
rm(list=ls())

install.packages("titanic")
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42, sample.kind="Rounding")
y <-titanic_clean$Survived
ind <- createDataPartition(y,times = 1, p = 0.2, list = FALSE )
train_set <- titanic_clean %>% slice(-ind)
test_set <- titanic_clean %>% slice(ind)

nrow(train_set)
nrow(test_set)
mean(train_set$Survived ==1)

#Q2
set.seed(3, sample.kind="Rounding")
y_hat <-sample(c(0,1), length(ind),replace=T)
mean(y_hat==test_set$Survived)

#Q3
set.seed(42, sample.kind="Rounding")
mean(train_set$Sex=="female" & train_set$Survived==1)
mean(train_set$Sex=="male" & train_set$Survived==1)

#Q4
female <- train_set %>% filter(Sex=="female")
mean(female$Survived==1)
male <-train_set %>% filter(Sex=="male")

train_set %>%
  group_by(Sex) %>%
  summarize(Survived =mean(Survived==1))%>%
  filter(Sex=="female") %>%
  pull(Survived)

#Q5
test_set %>%
  group_by(Sex) %>%
  summarize(Survived =mean(Survived==1)) 

y_hat <- test_set %>%
  mutate(prediction = ifelse(test_set$Sex=="female",1,0))
y <- test_set$Survived

mean(y_hat$prediction == test_set$Survived)

#answer
# predict Survived=1 if female, 0 if male
sex_model <- ifelse(test_set$Sex == "female", 1, 0)   
# calculate accuracy
mean(sex_model == test_set$Survived)  

#Q4
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived =mean(Survived==1))

class_model <- ifelse(test_set$Pclass==1,1,0)
mean(class_model== test_set$Survived)


combi_model_tibble <-train_set %>%
  group_by(Sex, Pclass) %>% 
  summarize(Survived =mean(Survived==1)) 


combi_model_tibble %>% spread(Sex, Survived)

combi_model <- ifelse((!test_set$Pclass==3) &
                         (test_set$Sex=="female"),1,0)

mean(combi_model==test_set$Survived)

sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)
sex_class_model <- factor(sex_class_model)

#Q5
sex_model <-factor(sex_model)
class_model <- factor(class_model)
combi_model <- factor(combi_model)
sex <- confusionMatrix(data=sex_model, reference=test_set$Survived)
class <- confusionMatrix(data=class_model, reference = test_set$Survived)
combi <-confusionMatrix(data=combi_model, reference = test_set$Survived)

identical(combi_model, sex_model)

sex
class
combi

#Q6
F_meas(data=sex_model, reference=test_set$Survived)
F_meas(data=class_model, reference = test_set$Survived)
F_meas(data=combi_model, reference = test_set$Survived)

#Q7
set.seed(1, sample.kind="Rounding")

y <- train_set$Survived
x <- train_set$Fare
dat <- data.frame(x,y)
dat_test <- data.frame(x= test_set$Fare, y= test_set$Survived)

fit_lda <- train(y~x, method="lda", data = dat)
y_hat_lda <- predict(fit_lda, newdata= dat_test)
mean(y_hat_lda== test_set$Survived)

fit_qda <- train(y~x, method="qda", data = dat)
y_hat_qda <- predict(fit_qda, newdata= dat_test)

#answer
train_lda <- train(Survived~Fare, method="lda", data=train_set)
lda_pred <- predict(train_lda, test_set)
mean(lda_pred== test_set$Survived)

train_qda <- train(Survived~Fare, method="qda", data=train_set)
qda_pred <- predict(train_qda, test_set)
mean(qda_pred== test_set$Survived)

#Q8
train_glm <- train(Survived~Age, method="glm", data=train_set)
glm_pred <- predict(train_glm, test_set)
mean(glm_pred== test_set$Survived)

train_glm <- train(Survived~Age + Fare + Pclass + Sex, method="glm", data=train_set)
glm_pred <- predict(train_glm, test_set)
mean(glm_pred== test_set$Survived)

train_glm <- train(Survived~ ., method="glm", data=train_set)
glm_pred <- predict(train_glm, test_set)
mean(glm_pred== test_set$Survived)

#Q9
set.seed(6, sample.kind="Rounding")
train_knn <- train(Survived~., method="knn",
                   tuneGrid=data.frame(k =seq(3, 51, 2)), data=train_set)

train_knn$bestTune
plot(train_knn)

knn_pred <- predict(train_knn, test_set)
mean(knn_pred== test_set$Survived)

#Q10
set.seed(8, sample.kind="Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(Survived ~ ., method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)
knn_cv_pred <- predict(train_knn_cv, test_set)
mean(knn_cv_pred==test_set$Survived)

#Q11
set.seed(10, sample.kind="Rounding")
train_rpart <- train(Survived~., method="rpart",
                   tuneGrid=data.frame(cp=seq(0, 0.05, 0.002)), data=train_set)

train_rpart$bestTune
rpart_pred <- predict(train_rpart, test_set)
mean(rpart_pred==test_set$Survived)

#Q12
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex=0.7)

#Q13
set.seed(14, sample.kind="Rounding")
train_rf <- train(Survived~., method="rf",
                  tuneGrid=data.frame(mtry = seq(1:7)), 
                  ntree=100, 
                  data=train_set)
train_rf$bestTune

rf_pred <- predict(train_rf, test_set)
mean(rf_pred==test_set$Survived)
varImp(train_rf)