rm(list=ls())

library(tidyverse)
library(dslabs)
install.packages("matrixStats")
library(matrixStats)

mnist <- read_mnist()
if(!exists("mnist")) mnist <- read_mnist()
class(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

head(mnist)
length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
m <-cbind(x_1, x_2)
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)
dim(m)

class(y)
y

rowSums(x)
rowMeans(x)

sums <- rowSums(x)
avg <- rowMeans(x)
mean(avg)
y
as.factor(y)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)

identical(avgs, avg)

#Filtering Columns Based on Summaries
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

#extract columns and rows
x[ ,c(351,352)]
x[c(2,3),]
new_x <- x[ ,colSds(x) > 60,drop=F]
new_sds<- colSds(new_x)
image(1:28, 1:28, matrix(new_sds, 17, 17)[, 28:1])

dim(new_x)
class(x[,1])
dim(x[1,])

#preserve the matrix class
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

#index with matrices
mat <- matrix(1:15, 5, 3)
as.vector(mat)
qplot(as.vector(x), bins = 30, color = I("black"))
new_x <- x
new_x[new_x < 50] <- 0

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

#binarize the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2)*1

#scale each row of a matrix
(x - rowMeans(x)) / rowSds(x)

#scale each column
t(t(x) - colMeans(x))

#take each entry of a vector and subtracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))

#divide by the standard deviation
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

#comprehension check
#scale each row of a matrix
(x - rowMeans(x)) / rowSds(x)

#scale each column
t(t(x) - colMeans(x))

#take each entry of a vector and subtracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))

#divide by the standard deviation
x <- matrix(rnorm(100*10), 100, 10)

dim(x)
ncol(x)

a <-x+seq(nrow(x))
b <-sweep(x,1,1:nrow(x),FUN="+")
c <-sweep(x,2,1:nrow(x),FUN = "+")

-x
identical(a,b)

range(x)
x<- mnist$train$images
grey <-ifelse(x>50 & x<205,1,0)
mean(grey)

y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom="boxplot")
mean(y)
