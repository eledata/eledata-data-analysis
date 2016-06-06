# 在做分类中需要估算样本之间的相似性度量，通常的做法是采用计算样本之间的距离
# 1. 欧式距离
# 1.1 二维平面上两点A（x1, y1) 和B(x2, y2)
# Euclidean Dist = sqrt((x1-x2)^2 + (y1-y2^2))
# 1.2 两个n维向量 a(x11, x12, x13, ...) 和b(x21, x22, x23, ...)
# Euclidean Dist = sqrt((x11-x21)^2 + (x12-x22^2) + ...)
# 两个向量之间的距离，在矩阵里面的表示就是，两行
# 设置 2X20 矩阵
data.x <- matrix(rnorm(20), nrow = 2)
eculidean.dist <- function(input.val){
  mtr <- as.matrix(input.val)
  distance <- sqrt(sum((mtr[1,]-mtr[2,])^2))
  return (distance)
}
eculidean.dist(data.x)
dist(data.x)

# 2. manhattan dist
# 2.1 二维平面上两点A（x1, y1) 和B(x2, y2)
# Manhattan Dist = |x1 - x2| + |y1 - y2|
# 2.2 两个n维向量 a(x11, x12, x13, ...) 和b(x21, x22, x23, ...)
# Manhattan Dist = Sum(x1k - x2k)
manhattan.dist <- function(input.val){
  mtr <- as.matrix(input.val)
  distance <- sum(abs((mtr[1,]-mtr[2,])))
  return (distance)
}
manhattan.dist(data.x)
dist(data.x, method = "manhattan")

# 3. 切比雪夫距离
# 3.1 二维平面上两点A（x1, y1) 和B(x2, y2)
# Chebyshev Dist = Max(|x1 - x2|, |y1 - y2|)
# 3.2 两个n维向量 a(x11, x12, x13, ...) 和b(x21, x22, x23, ...)
# Chebyshev Dist = Max(|x1k - x2k|)
maximum.dist <- function(input.val){
  mtr <- as.matrix(input.val)
  distance <- max(abs((mtr[1,]-mtr[2,])))
  return (distance)
}
maximum.dist(data.x)
dist(data.x, method = "maximum")

# 4. 马氏距离
# 4.1 设x，y均值为m，总体的协方差S，来之同一总体的样本，dist(x,y) = sqrt((x-y)T*S*(x-y))-- x,y之间的马氏距离
# x与总体X的距离，dist(x,X) = sqrt((x-X)T*S(-1)*(x-X))
# inverted 是指明协方差是否已经是逆矩阵
# %*% 表示矩阵相乘 #矩阵X乘矩阵Y。若Y是数值型的向量，R会自动判断其为行向量还是列向量。若X与Y为维度匹配的数值型向量，则返回的是矩阵型向量的内积。
# mahalanobis(x = 总体，mu = 样本的均值，协方差 = 样本的协方差)


x <- matrix(rnorm(10*3), ncol = 3) # 列：3个因子，行：10个样本
x.cov <- cov(x)
mahalanobis.dist <- function(x, center, cov, inverted = FALSE, ...){
  x <- if (is.vector(x))
    matrix(x, ncol = length(x))
  else as.matrix(x) # 转换成矩阵
  if(!identical(center, FALSE))
    x <- sweep(x, 2L, center) # 在列方向上减去均值
  if(!inverted)
    cov <- solve(cov, ...) # 协方差求逆矩阵
  setNames(rowSums(x %*% cov * x), rownames(x))
}
mahalanobis.dist(x, colMeans(x), cov = x.cov)
mahalanobis(x, colMeans(x), cov = x.cov)


# 5. 余弦夹角
# 5.1 cos(x) = (a * b)/(||a|| * ||b||)，讲的余弦夹角与相似度之间的关系，用向量去思考就可以知道。
# 余弦夹角值大 --> 两者相似度高， x的值低，两者和靠近。 反之亦然。
x <- matrix(rnorm(10*2), ncol = 2)
y <- cbind(1:2, 3:4)
cosine.dist <- function(x){
  dist <- (sum(x[1,] %*% x[2,]))/(sum(sqrt(rowSums(x^2))))
  return (dist)
}
cosine.dist(y)

# 6. hamming dist
# 两个等长的字符串，其中一个变成另外一个的变换次数 1111-> 1001 Hamming Dist 2
x <- c(0, 0, 1, 1, 1)
y <- c(1, 0, 1, 1, 0)
dist(rbind(x, y), method = "binary")
# 2/(5-1)


# 7. Minkowski dist
# dist = (sum(|x1k - x2k|^p)^(1/p)
# p = 1 : Manhattan dist
# p = 2 : 欧式距离
# p = inf : 切比雪夫距离

x <- matrix(rnorm(30), nrow = 3)
y <- matrix(rnorm(20), ncol = 2)
dist(x, method = "minkowski", p = 2)
dist(x, method = "minkowski")

# 总结： 曼哈顿距离，欧式距离，切比雪夫距离，闵式距离，都是看向量，两个向量，多个向量的计算。向量的思想去思考
# 马氏距离，要加入总体样本的概念，列表示不同的样本，行表示不同样本的值。这是要注意的地方，马氏距离算的是样本内的值到
# 中心的距离。
