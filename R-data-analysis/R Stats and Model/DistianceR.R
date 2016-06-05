# 在做分类中需要估算样本之间的相似性度量，通常的做法是采用计算样本之间的距离
# 1. 欧式距离
# 设置 5X20 矩阵
x <- matrix(rnorm(100), nrow = 5)

x1 <- c(1,5)
x2 <- c(7,4)
x3 <- cbind(x1, x2)
x3
dist1 <- dist(x3, method = "manhattan")
dist1
dist2 <- dist(x3, method = "maximum") # 切比雪夫 距离
dist2
dist3 <- dist(x3, method = "manhattan")
dist3
dist4 <- Mahalanobis(x3, method = "manhattan")
dist4
