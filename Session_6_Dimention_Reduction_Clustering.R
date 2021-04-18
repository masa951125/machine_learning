#dimention reduction

set.seed(1988,sample.kind="Rounding")
library(MASS)
n <- 100
Sigma <- matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)
x <- rbind(mvrnorm(n / 2, c(69, 69), Sigma),
           mvrnorm(n / 2, c(55, 55), Sigma))

plot(x)

d <- dist(x)
as.matrix(d)[99,1]
z <-x[,1]

dist(z)
plot(dist(x),dist(z)/sqrt(2))
plot
z  <- cbind((x[,2] + x[,1])/2,  x[,2] - x[,1])

#orthogonality
z[,1] <- (x[,1] + x[,2]) / sqrt(2)
z[,2] <- (x[,2] - x[,1]) / sqrt(2)

z

library(tidyverse)
qplot(z[,1], bins = 20, color = I("black"))

#principal component analysis
colMeans(x^2) 

sum(colMeans(x^2))
sum(colMeans(z^2))

v <- colMeans(z^2)
v/sum(v)

#first principal component
pca <- prcomp(x)
pca$rotation

names(iris)
iris$Species


#clustering
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$train %>% qplot(x_1, x_2, data = .)

data("movielens")
top <- movielens %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(50, n) %>%
  pull(movieId)

x <- movielens %>% 
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>% 
  dplyr::select(title, userId, rating) %>%
  spread(userId, rating)

row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix()
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

#hierarchical cluster
d <- dist(x)
h <- hclust(d)
plot(h, cex = 0.65, main = "", xlab = "")
groups <- cutree(h, k = 10)
names(groups)[groups==4]
names(groups)[groups==9]

h_2 <- dist(t(x)) %>% hclust()

#k_means
x_0 <- x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0, centers = 10)
groups <- k$cluster

k$cluster
k <- kmeans(x_0, centers = 10, nstart = 25)

#heatmaps
data("tissue_gene_expression")
x <- sweep(tissue_gene_expression$x, 2, colMeans(tissue_gene_expression$x))
h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x)))
image(x[h_1$order, h_2$order])
heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral"))

#comprehension check
data("tissue_gene_expression")
qplot(tissue_gene_expression$x[,1:2], ) 

#Q1
pca <- prcomp(tissue_gene_expression$x)
pca$rotation

data.frame(pca$x[,1:2], tissue = tissue_gene_expression$y) %>% 
  ggplot(aes(PC1,PC2, color = tissue))+
  geom_point()

#Q2
avg <- rowMeans(tissue_gene_expression$x)

data.frame(pc1 =pca$x[,1], avg) %>%
  ggplot(aes(pc1, avg, color=tissue_gene_expression$y)) +
  geom_point()
cor(pca$x[,1], avg)

#Q3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q4
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

boxplot(pc$x[,7]~ tissue_gene_expression$y)

#Q5
summary(pc)

#Q1
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
h <- hclust(d)
plot(h, cex = 0.5, main = "", xlab = "")
plot(h)

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), 
        col = RColorBrewer::brewer.pal(11, "RdBu"),
        ColSideColors= colors,
        scale="row")
