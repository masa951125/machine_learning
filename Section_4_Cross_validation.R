library(tidyverse)
library(caret)

set.seed(1996, sample.kind="Rounding") 
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset,y, method="glm")
fit

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

ind <- which(tt$p.value <0.01)
length(ind)

x_subset <- x[,ind]
x_subset

fit <- train(x_subset,y, method="glm")
fit

fit <- train(x_subset, y, method= "knn", 
             tuneGrid =data.frame(k = seq(101, 301, 25)))

indexes <- createDataPartition(y, times = 5, p = 0.2)
dat <- data.frame(y=y, data.frame(x))
res <- sapply(indexes, function(test_index){
  
  train_set <- slice(dat, -test_index)
  test_set <- slice(dat, test_index)
  
  pvals <- colttests(as.matrix(train_set[,-1]), train_set$y)$p.value
  
  ind <- c(TRUE, pvals <= 0.01)
  train_set <-   train_set[, ind]
  
  fit <- glm(y ~ ., data = train_set, family = "binomial")
  y_hat <- ifelse(predict(fit, newdata = test_set[, ind], type = "response") > 0.5, 1, 0) %>%
    factor()
  mean(y_hat == test_set$y)
})
res
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, 
             method="knn", tuneGrid =data.frame(k = seq(1,7,2)))

max(fit$results$Accuracy)

#bootstrap

n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1, sample.kind="Rounding") 
N <- 250
X <- sample(income, N)
M<- median(X)
M

library(gridExtra)
B <- 10^5
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)
p1

B <- 10^5
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()
quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

#comprehension check
library(dslabs)
library(caret)
data(mnist_27)

set.seed(1995, sample.kind="Rounding") 
indexes <- createResample(mnist_27$train$y, 10)
sum(indexes$Resample01 == 4)

sum(indexes ==3)


sum(indexes[[1]]==3) +
  sum(indexes[[2]]==3)+
  sum(indexes[[3]]==3)+
  sum(indexes[[4]]==3)+
  sum(indexes[[5]]==3)+
  sum(indexes[[6]]==3)+
  sum(indexes[[7]]==3)+
  sum(indexes[[8]]==3)+
  sum(indexes[[9]]==3)+
  sum(indexes[[10]]==3)

x <- sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)

set.seed(1, sample.kind="Rounding") 

dat <- replicate(10000,{
  y <-rnorm(100,0,1)
  quantile(y, 0.75)
})
  
mean(dat)
sd(dat)

#Q4
set.seed(NULL)

set.seed(1, sample.kind = "Rounding")
q <- rnorm(100,0,1)
set.seed(1, sample.kind = "Rounding")
ind <- createResample(q, 10)

ind.data <-as.data.frame(ind)

N <- c(1,10)
q_75 <-sapply(N, function(k){
  quantile(q[ind.data[,k]],0.75)
})

mean(q_75)
sd(q_75)

#model method
q_75_answer <- sapply(ind, function(k){
  q_star <- y[k]
  quantile(q_star, 0.75)
})


#Q5
set.seed(1, sample.kind = "Rounding")
ind_10000 <- createResample(q,10000)

ind_10000_data <-as.data.frame(ind_10000)

N <- c(1:10000)
q_75 <-sapply(N, function(k){
  quantile(q[ind_10000_data[,k]],0.75)
})

mean(q_75)
sd(q_75)
