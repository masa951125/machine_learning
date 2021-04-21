options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

names(brca$x[1,])
?brca

#Q1
nrow(brca$x)
ncol(brca$x)
mean(brca$y=="M")
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

#Q2
x_scaled <- sweep(sweep(brca$x,2,colMeans(brca$x)),2,colSds(brca$x), FUN="/")
sd(x_scaled[,1])
median(x_scaled[,1])

#Q3
d <-dist(x_scaled)
d_mat <- as.matrix(d)

mean(d_mat[brca$y=="B",1])
mean(d_mat[brca$y=="M",1])

#Q4
h_1 <- hclust(dist(x_scaled))
h_2 <- hclust(dist(t(x_scaled)))

d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), col = RColorBrewer::brewer.pal(11, "Spectral"),
        labRow = NA, labCol = NA)


heatmap(x_scaled, col = RColorBrewer::brewer.pal(11, "Spectral"))

#Q5
h_1 <- hclust(dist(x_scaled))
h_2 <- hclust(dist(t(x_scaled)))
groups <-cutree(h_2, k=5)
names(groups)[groups==5]
split(names(groups), groups)

#Q6
pca <-prcomp(x_scaled)
summary(pca)

#Q7
data.frame(PC1 =x_scaled[,1],PC2 =x_scaled[,2],tumor_type =brca$y) %>%
  ggplot(aes(PC1, PC2, col=tumor_type))+
  geom_point()

#Q8
for(i in 1:10){
  boxplot(pca$x[,i]~brca$y, main = paste("PC", i))
}

#answer
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()


data.frame(pca$x[,1:10] ,tumor_type =brca$y) %>%
  ggplot(aes(PC1:PC10,col=tumor_type))+
  geom_boxplot()

#Q9
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

mean(test_y=="B")
mean(train_y=="B")

#Q10
k <- kmeans(train_x,centers =2 )

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
pred_y <-predict_kmeans(test_x,k)

mean(pred_y==as.numeric(test_y))
as.numeric(test_y)

#answer
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

test_y[test_y=="B"] 
mean(kmeans_preds[test_y=="B"] =="B")
mean(kmeans_preds[test_y=="M"]=="M")

#answer
sensitivity(factor(kmeans_preds), test_y, positive = "B")

#Q11
glm <-train(train_x,train_y, method = "glm")
y_hat_glm <- predict(glm, test_x)

mean(test_y==y_hat_glm)

#Q12
lda <-train(train_x,train_y, method = "lda")
y_hat_lda <- predict(lda, test_x)
mean(test_y==y_hat_lda)

qda <-train(train_x,train_y, method = "qda")
y_hat_qda <- predict(qda, test_x)
mean(test_y==y_hat_qda)

#Q13
library(gam)
set.seed(5, sample.kind = "Rounding")

train_loess <-train(train_x,train_y, method = "gamLoess")
y_hat_loess <- predict(train_loess, test_x)
mean(test_y==y_hat_loess)

#Q14
set.seed(7, sample.kind = "Rounding")
train_knn <- train(train_x, train_y, method = "knn",
                   tuneGrid = data.frame(k = seq(3,21, 2)))

y_hat_knn <- predict(train_knn, test_x)
mean(test_y==y_hat_knn)
plot(train_knn)

#Q15
set.seed(9, sample.kind = "Rounding")
train_rf <- train(train_x, train_y, method = "rf",
                   tuneGrid = data.frame(mtry= c(3, 5, 7, 9)),
                   importance = TRUE)

y_hat_rf <- predict(train_rf, test_x)
mean(test_y==y_hat_rf)

#Q16

train_rf$results
d <- varImp(train_rf)

#Q17
models <- c("glm", "lda", "knn", "gamLoess", "qda", "rf", "kmeans")

train_kmeans <-train(train_x,train_y, method = "kmeans")
y_hat_glm <- predict(glm, test_x)

y_hat_kmeans <- factor(kmeans_preds)
ensemble <- cbind(y_hat_glm, 
          y_hat_kmeans, 
          y_hat_knn, 
          y_hat_lda, 
          y_hat_loess,
          y_hat_qda, 
          y_hat_rf) 

y_hat_ensemble <- ifelse(rowMeans(ensemble)>1.5,2,1)
mean(y_hat_ensemble==as.numeric(test_y))

#answer
ensemble <- cbind(glm = y_hat_glm == "B", lda = y_hat_lda == "B", qda = y_hat_qda == "B", loess = y_hat_loess == "B", rf = y_hat_rf == "B", knn = y_hat_knn == "B", kmeans = y_hat_kmeans == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

#Q17
ensemble <-mean(ensemble_preds==test_y)
glm <-mean(y_hat_glm ==test_y)
kmeans <-mean(y_hat_kmeans==test_y)
knn <-mean(y_hat_knn==test_y)
lda <-mean(y_hat_lda==test_y)
qda <-mean(y_hat_qda==test_y)
loess <-mean(y_hat_loess==test_y)
rf <-mean(y_hat_rf==test_y)

models <- c("Ensemble", "Logistic regression","K means","K nearest neighbors", "LDA", "QDA", "Loess", "Random forest")
accuracy <- c(mean(ensemble_preds==test_y),
              mean(y_hat_glm ==test_y),
              mean(y_hat_kmeans==test_y),
              mean(y_hat_knn==test_y),
              mean(y_hat_lda==test_y),
              mean(y_hat_qda==test_y),
              mean(y_hat_loess==test_y),
              mean(y_hat_rf==test_y))
data.frame(Model = models, Accuracy = accuracy)