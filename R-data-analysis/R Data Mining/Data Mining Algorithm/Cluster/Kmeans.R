data(iris)
head(iris)#提取前 6 行的数据
library(fpc)#安装程序包 fpc 并调用
#将数据标准化
norm <- function(x){
    (x-mean(x))/(sqrt(var(x)))
}
raw.data <- iris[,1:4]
head(raw.data)
norm.data <- data.frame(sl = norm(raw.data[,1]),
                        sw = (raw.data[,2]),
                        pl = (raw.data[,3]),
                        pw = (raw.data[,4]))
head(norm.data)
# k 取 2 到 10，评估 K
K <- 2:10
round <- 40 # 每次迭代 40 次，避免局部最优
rst <- sapply(K, function(i){
    print(paste("K=",i))
    mean(sapply(1:round,function(r){
        print(paste("Round",r))
        result <- kmeans(norm.data, i)
        stats <- cluster.stats(dist(norm.data), result$cluster)
        stats$avg.silwidth }))
})
plot(K,rst,type='l',main='轮廓系数与 K 的关系', ylab='轮廓系数')
#当 k 取 2 时，有最大的轮廓系数，所以选取聚类种类为 2 #
#将聚类结果通过多维定标(Multidimensional scaling)降至 2 维，进而查看聚类结果#
old.par <- par(mfrow = c(1,2))
k = 2 # 根据上面的评估 k=2 最优
clu <- kmeans(norm.data,k)
mds = cmdscale(dist(norm.data,method="euclidean"))
plot(mds, col=clu$cluster, main='kmeans 聚类 k=2', pch = 19)
plot(mds, col=iris$Species, main='原始聚类', pch = 19)
par(old.par)
##由结果可知，由于原始数据集分

hclust()

agnes()


daisy()


data(ruspini)
pr4 <- pam(ruspini, 4)
str(si <- silhouette(pr4))
(ssi <- summary(si))
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring

si2 <- silhouette(pr4$clustering, dist(ruspini, "canberra"))
summary(si2) # has small values: "canberra"'s fault
plot(si2, nmax= 80, cex.names=0.6)

op <- par(mfrow= c(3,2), oma= c(0,0, 3, 0),
          mgp= c(1.6,.8,0), mar= .1+c(4,2,2,2))
for(k in 2:6)
  plot(silhouette(pam(ruspini, k=k)), main = paste("k = ",k), do.n.k=FALSE)
mtext("PAM(Ruspini) as in Kaufman & Rousseeuw, p.101",
      outer = TRUE, font = par("font.main"), cex = par("cex.main")); frame()

## the same with cluster-wise colours:
c6 <- c("tomato", "forest green", "dark blue", "purple2", "goldenrod4", "gray20")
for(k in 2:6)
  plot(silhouette(pam(ruspini, k=k)), main = paste("k = ",k), do.n.k=FALSE,
       col = c6[1:k])
par(op)

## clara(): standard silhouette is just for the best random subset
data(xclara)
set.seed(7)
str(xc1k <- xclara[sample(nrow(xclara), size = 1000) ,])
cl3 <- clara(xc1k, 3)
plot(silhouette(cl3))# only of the "best" subset of 46
## The full silhouette: internally needs large (36 MB) dist object:
sf <- silhouette(cl3, full = TRUE) ## this is the same as
s.full <- silhouette(cl3$clustering, daisy(xc1k))
if(paste(R.version$major, R.version$minor, sep=".") >= "2.3.0")
  stopifnot(all.equal(sf, s.full, check.attributes = FALSE, tolerance = 0))
## color dependent on original "3 groups of each 1000":
plot(sf, col = 2+ as.integer(names(cl3$clustering) ) %/% 1000,
     main ="plot(silhouette(clara(.), full = TRUE))")

## Silhouette for a hierarchical clustering:
ar <- agnes(ruspini)
si3 <- silhouette(cutree(ar, k = 5), # k = 4 gave the same as pam() above
                  daisy(ruspini))
plot(si3, nmax = 80, cex.names = 0.5)
## 2 groups: Agnes() wasn't too good:
si4 <- silhouette(cutree(ar, k = 2), daisy(ruspini))
plot(si4, nmax = 80, cex.names = 0.5)

