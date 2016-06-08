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
# mahalanobis(x = 测试样本，mu = 总体样本的均值，协方差 = 测试样本的协方差)

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
# Example:
classX1<-data.frame(
  x1=c(6.60,  6.60,  6.10,  6.10,  8.40,  7.2,   8.40,  7.50,  
       7.50,  8.30,  7.80,  7.80),
  x2=c(39.00, 39.00, 47.00, 47.00, 32.00,  6.0, 113.00, 52.00,
       52.00,113.00,172.00,172.00),
  x3=c(1.00,  1.00,  1.00,  1.00,  2.00,  1.0,   3.50,  1.00,
       3.50,  0.00,  1.00,  1.50),
  x4=c(6.00,  6.00,  6.00,  6.00,  7.50,  7.0,   6.00,  6.00,
       7.50,  7.50,  3.50,  3.00),
  x5=c(6.00, 12.00,  6.00, 12.00, 19.00, 28.0,  18.00, 12.00,
       6.00, 35.00, 14.00, 15.00),
  x6=c(0.12,  0.12,  0.08,  0.08,  0.35,  0.3,   0.15,  0.16,
       0.16,  0.12,  0.21,  0.21),
  x7=c(20.00, 20.00, 12.00, 12.00, 75.00, 30.0,  75.00, 40.00,
       40.00,180.00, 45.00, 45.00)
)
classX2<-data.frame(
  x1=c(8.40,  8.40,  8.40,  6.3, 7.00,  7.00,  7.00,  8.30,
       8.30,   7.2,   7.2,  7.2, 5.50,  8.40,  8.40,  7.50,
       7.50,  8.30,  8.30, 8.30, 8.30,  7.80,  7.80),
  x2=c(32.0 ,32.00, 32.00, 11.0, 8.00,  8.00,  8.00, 161.00,
       161.0,   6.0,   6.0,  6.0, 6.00,113.00,113.00,  52.00,
       52.00, 97.00, 97.00,89.00,56.00,172.00,283.00),
  x3=c(1.00,  2.00,  2.50,  4.5, 4.50,  6.00,  1.50,  1.50,
       0.50,   3.5,   1.0,  1.0, 2.50,  3.50,  3.50,  1.00,
       1.00,  0.00,  2.50, 0.00, 1.50,  1.00,  1.00),
  x4=c(5.00,  9.00,  4.00,  7.5, 4.50,  7.50,  6.00,  4.00,
       2.50,   4.0,   3.0,  6.0, 3.00,  4.50,  4.50,  6.00,
       7.50,  6.00,  6.00, 6.00, 6.00,  3.50,  4.50),
  x5=c(4.00, 10.00, 10.00,  3.0, 9.00,  4.00,  1.00,  4.00,
       1.00,  12.0,   3.0,  5.0, 7.00,  6.00,  8.00,  6.00,
       8.00,  5.00,  5.00,10.00,13.00,  6.00,  6.00),
  x6=c(0.35,  0.35,  0.35,  0.2, 0.25,  0.25,  0.25,  0.08,
       0.08,  0.30,   0.3,  0.3, 0.18,  0.15,  0.15,  0.16,
       0.16,  0.15,  0.15, 0.16, 0.25,  0.21,  0.18),
  x7=c(75.00, 75.00, 75.00,  15.0, 30.00, 30.00, 30.00, 70.00,
       70.00,  30.0,  30.0,  30.0, 18.00, 75.00, 75.00, 40.00,
       40.00,180.00,180.00,180.00,180.00, 45.00, 45.00)
)
tx1 <- as.matrix(classX1) 
tx2 <- as.matrix(classX2)
ctx <- rbind(classX1, classX2)
tx <- as.matrix(ctx)
tx1 # 7个属性，12个样本，因子1
tx2 # 7个属性，23个样本，因子2
tx  # 7个属性，35个样本，回代来判定是否分的准确

# 表明tx到tx1的马氏距离， tx 测试样本，colMeans总体样本中心点，总体样本的协方差. 每一行，也就是向量，到中心点的距离情况。综合了7个因子的情况，之后
# 求和的距离
D2 <- mahalanobis(tx, colMeans(tx1), cov(tx1)) 
D2
D3 <- mahalanobis(tx, colMeans(tx2), cov(tx2))
D3
dis <- D3 - D2 # 到tx1近，还是tx2近。
dis


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

y <- scale(x, center = F, scale = T)/sqrt(nrow(x) - 1)
c <- t(y) %*% y


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
dist(x, method = "minkowski", p = 2)
dist(x, method = "minkowski")

# 8. Lance dist or Canberra Dist Canberra 是Lance的加强版
# dist = sum|x1k - x2k|/|x1k + x2k|

x <- matrix(rnorm(30), nrow = 3)
dist(x, method = "canberra")


# 总结： 曼哈顿距离，欧式距离，切比雪夫距离，闵式距离，都是看向量，两个向量，多个向量的计算。向量的思想去思考
# 马氏距离，向量化的思想要贯彻。这是要注意的地方，马氏距离算的是样本内的值到
# 中心的距离。
