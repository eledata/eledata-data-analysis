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



