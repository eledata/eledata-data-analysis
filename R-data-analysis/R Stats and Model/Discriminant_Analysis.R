# 判别分析
# 1.1 判别分析概念
# 判别分析，以判别个体所属群体的一种统计方法。
# 判别分析的假设： 1. 每个判别变量不能是其他判别变量的线性组合，不存在多重共线性的情况。2. 各个判别变量具有多远正态分布
# 2.1 距离判别
# 2.1.1 判别准则和判别函数
# 1. 两个总体协方差相同
# w(x) = (x - mu)^T S^(-1)(mu1-mu2) -- 距离判别函数
# 判别准则： R1 = {x|w(x) >= 0} R2 = {x|w(x) < 0}
# 具体算法里面，就是通过马氏距离的大小来判别
# 2. 两个总体协方差不同
# w(x) = (x - mu1)^T S1^(-1)(x-mu1) - (x - mu2)^T S2^(-1)(x-mu2)-- 距离判别函数
# 判别准则： R1 = {x|w(x) >= 0} R2 = {x|w(x) < 0}
# 具体算法里面，就是通过马氏距离的大小来判别

# > a <- c(1,2,3,4)
# > as.matrix(a)
# [,1]
# [1,]    1
# [2,]    2
# [3,]    3
# [4,]    4
# > t(as.matrix(a))
# [,1] [,2] [,3] [,4]
# [1,]    1    2    3    4

# test data:
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

# 两个类别的距离判别分析
discriminant.dist <- function(x1, x2, test = NULL, var.equal = FALSE){
  if(is.null(test)) test <- rbind(x1, x2)
  if(is.vector(test)){
    test <- t(as.matrix(test)) # 由于as.matrix()将向量转换为矩阵，会导致生成许多样本。需要t来转置，一个向量对应一个样本。
  }else if(is.matrix(test) != TRUE){
    test <- as.matrix(test)
  }
  
  if(is.matrix(x1)) x1 <- as.matrix(x1)
  if(is.matrix(x2)) x2 <- as.matrix(x2)
  
  mu1 <- colMeans(x1)
  mu2 <- colMeans(x2)
  
  nt <- nrow(test) # 表示要测距的向量个数(行数)
  result <- matrix(rep(0, nt), nrow = 1, byrow = TRUE, dimnames = list("result", 1:nt))
  
  if(var.equal){
    S <- cov(rbind(x1, x2))
    compare <- mahalanobis(test, colMeans(tx1), S) - mahalanobis(test, colMeans(tx2), S)
  }else{
    S1 <- cov(x1)
    S2 <- cov(x2)
    compare <- mahalanobis(test, colMeans(tx1), S1) - mahalanobis(test, colMeans(tx2), S2)
  }
  
  for(i in 1:nt){
    if(compare[i] > 0){
      result[i] <- 2
    }else{
      result[i] <- 1
    }
  }
  result
}

# test, classX1,X2, 属于训练数据回代测试。
(discriminant.dist(classX1, classX2))

(discriminant.dist(classX1, classX2, var.equal = TRUE))

# 多类别判别分析函数，因子数大于2个，用iris来测试

discriminant.multi.dist <- function(trnx, trng, tstx = NULL, var.equal = TRUE){
  # 考虑因子问题
  if(is.factor(trng) == FALSE){
    # 如果没有，那么自己造一个
    trnx.rn <- nrow(trnx)
    trng.rn <- nrow(trng)
    trnx <- rbind(trnx, trng)
    trng <- factor(rep((1:2), c(trnx.rn, trng.rn))) #factor(rep((1:2), c(5, 10))) result :[1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 Levels: 1 2
  }
  
  # 测试数据
  if(is.null(tstx) == TRUE){
    tstx <- trnx
  }
  
  if(is.vector(tstx) == TRUE){
    tstx <- t(as.matrix(tstx))
  }else if(is.matrix(tstx) == FALSE){
    tstx <- as.matrix(tstx)
  }
  
  if(is.matrix(trnx) == FALSE){
    trnx <- as.matrix(trnx)
  }
  
  tstx.rn <- nrow(tstx)
  result <- matrix(rep(0,tstx.rn), nrow = 1, byrow = TRUE, dimnames = list("result", 1:tstx.rn)) # dimnames = list(行，列)
  
  trng.g <- length(levels(trng))
  # 总体样本均值，按照因子来分
  mu <- matrix(0, nrow = trng.g, ncol = ncol(trnx))
  for(i in 1:trng.g){
    mu[i,] <- colMeans(trnx[trng==i,])
  }
  
  # 马氏距离矩阵 因子数*测试样本向量数
  dist <- matrix(0, nrow = trng.g, ncol = tstx.rn)
  if(var.equal == TRUE || var.equal == T){
    for(i in 1:trng.g){
      dist[i,] <- mahalanobis(tstx, mu[i,], cov(trnx))  # 比较测试数据与各个因子之间的马氏距离。
    }
  }else{
    for(i in 1:trng.g){
      dist[i,] <- mahalanobis(tstx, mu[i,], cov(trnx[trng == i,])) 
    }
  }
  
  # 根据马氏距离进行分类
  for(j in 1:tstx.rn){
    min <- Inf
    for(i in 1:trng.g){
      if(dist[i,j] < min){
        min <- dist[i,j] # 在多个因子之间，比较最小的马氏距离，然后填入最小马氏距离的i值。相当于一个分发的作用。
        result[j] <- i
      }
    }
  }
  
  result
}

# test
tstx <- iris[,1:4]
trng <- gl(3,50) # 预先知道有3类
(discriminant.multi.dist(tstx, trng))


# 3.1 贝叶斯判别
# 3.1.1 误判概率和误判损失
# 1. 平均误判损失
# ECM(R1,R2) = L(2|1)P(2|1)p1 + L(1|2)P(1|2)p2, 选择是RCM最小
# 2. 判别函数和判别准则看<统计建模与R>
# W(x): 马氏距离只差，beta: ln((L(1|2)*P2)/(L(2|1)*P1)) (协方差相同) beta: ln((L(1|2)*P2)/(L(2|1)*P1)) + 0/5*(ln(|S1|/|S2|))(协方差不同)
# R1: W(x) >= beta R2: W(x) < beta

# rate 的理解是关键
discriminant.bayes <- function(trnx1, trnx2, rate = 1, tstx = NULL, var.equal = TRUE){
  if(is.null(tstx)) tstx <- rbind(trnx1, trnx2)
  if(is.vector(tstx)){
    tstx <- t(as.matrix(tstx)) # 由于as.matrix()将向量转换为矩阵，会导致生成许多样本。需要t来转置，一个向量对应一个样本。
  }else if(is.matrix(tstx) != TRUE){
    tstx <- as.matrix(tstx)
  }
  
  if(is.matrix(trnx1)) trnx1 <- as.matrix(trnx1)
  if(is.matrix(trnx2)) trnx2 <- as.matrix(trnx2)
  
  mu1 <- colMeans(trnx1)
  mu2 <- colMeans(trnx2)
  
  nt <- nrow(tstx) # 表示要测距的向量个数(行数)
  result <- matrix(rep(0, nt), nrow = 1, byrow = TRUE, dimnames = list("result", 1:nt))
  
  if(var.equal == TRUE){
    S <- cov(rbind(trnx1, trnx2))
    dist <- mahalanobis(tstx, mu1, S) - mahalanobis(tstx, mu2, S)
    beta <- 2*log(rate) # (L(1|2)*P2)/(L(2|1)*P1) --> rate 
  }else{
    S1 <- cov(trnx1)
    S2 <- cov(trnx2)
    dist <- mahalanobis(tstx, mu1, S1) - mahalanobis(tstx, mu2, S2)
    beta <- 2*log(rate) + 0.5*log(det(S1)/det(S2)) 
  }
  
  # 贝叶斯也是用马氏距离来判定两者的距离，但是在分类的过程中，存在不同的判定方法。
  # 贝叶斯是利用beta，也是综合了损失函数和密度函数，进行考量。
  for(i in 1:nt){
    if(dist[i] > beta){
      result[i] <- 1
    }else{
      result[i] <- 2
    }
  }
  
  result
}

# test example:
TrnX1<-matrix(
  c(24.8, 24.1, 26.6, 23.5, 25.5, 27.4, 
    -2.0, -2.4, -3.0, -1.9, -2.1, -3.1),
  ncol=2)
TrnX2<-matrix(
  c(22.1, 21.6, 22.0, 22.8, 22.7, 21.5, 22.1, 21.4, 
    -0.7, -1.4, -0.8, -1.6, -1.5, -1.0, -1.2, -1.3),
  ncol=2)
# 解释一下数据，由于春旱和无春旱的先验概率是6/14, 8/14. P1 = 6/14, p2 = 8/14. 在损失函数都一样的情况下
# (L(1|2)*P2)/(L(2|1)*P1) --> rate : (8/14)/(6/14) = 8/6
discriminant.bayes(TrnX1, TrnX2, rate=8/6, var.equal=TRUE)
discriminant.bayes(TrnX1, TrnX2, rate=8/6)

# 多类别判别分析函数，因子数大于2个，用iris来测试

discriminant.multi.bayes <- function(trnx, trng, p = rep(1,length(levels(trng))), tstx = NULL, var.equal = TRUE){
  # 考虑因子问题
  if(is.factor(trng) == FALSE){
    # 如果没有，那么自己造一个
    trnx.rn <- nrow(trnx)
    trng.rn <- nrow(trng)
    trnx <- rbind(trnx, trng)
    trng <- factor(rep(1:2, c(trnx.rn, trng.rn))) #factor(rep((1:2), c(5, 10))) result :[1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 Levels: 1 2
  }
  
  # 测试数据
  if(is.null(tstx) == TRUE){
    tstx <- trnx
  }
  
  if(is.vector(tstx) == TRUE){
    tstx <- t(as.matrix(tstx))
  }else if(is.matrix(tstx) == FALSE){
    tstx <- as.matrix(tstx)
  }
  
  if(is.matrix(trnx) == FALSE){
    trnx <- as.matrix(trnx)
  }
  
  tstx.rn <- nrow(tstx)
  result <- matrix(rep(0,tstx.rn), nrow = 1, byrow = TRUE, dimnames = list("result", 1:tstx.rn)) # dimnames = list(行，列)
  
  trng.g <- length(levels(trng))
  # 总体样本均值，按照因子来分
  mu <- matrix(0, nrow = trng.g, ncol = ncol(trnx))
  for(i in 1:trng.g){
    mu[i,] <- colMeans(trnx[trng==i,])
  }
  
  # 马氏距离矩阵 因子数*测试样本向量数
  dist <- matrix(0, nrow = trng.g, ncol = tstx.rn)
  
  if(var.equal == TRUE || var.equal == T){
    for(i in 1:trng.g){
      d <- mahalanobis(tstx, mu[i,], var(trnx))  # 比较测试数据与各个因子之间的马氏距离。
      dist[i,] <- d - 2*log(p[i]) # 判别函数具体公式书里有
    }
  }else{
    for(i in 1:trng.g){
      d <- mahalanobis(tstx, mu[i,], cov(trnx[trng==i,]))
      dist[i,] <- d - 2*log(p[i]) - log(det(cov(trnx[trng==i,])))
    }
  }
  
  # 根据马氏距离进行分类
  for(j in 1:tstx.rn){
    min <- Inf
    for(i in 1:trng.g){
      if(dist[i,j] < min){
        min <- dist[i,j] # 在多个因子之间，比较最小的马氏距离，然后填入最小马氏距离的i值。相当于一个分发的作用。
        result[j] <- i
      }
    }
  }
  
  result
}

# test
tstx <- iris[,1:4]
trng <- gl(3,50) # 预先知道有3类
(discriminant.multi.bayes(tstx, trng, var.equal = FALSE))


# 4.1 Fisher判别
# 4.1.1 原理
# 1. Fisher判别原理，按类内方差尽量小，类间方差尽量大的判别准则来求判别函数
# W = siga1^2 + siga2^2 类内偏差平方和，B = (u1 - u)^2 + (u2 - u)^2  u = (u1 + u2)/2
# 2. 线性判别函数中系数的确定
# 取u(x)为线性函数，u(x) = a^T * x (LDA, 线性判别分析)
# 判别函数 w = D^tS^(-1)(x-mean(x)) 具体看书里面

discriminant.fisher <- function(trnx1, trnx2, tstx = NULL){
  if(is.null(tstx)) tstx <- rbind(trnx1, trnx2)
  if(is.vector(tstx)){
    tstx <- t(as.matrix(tstx)) # 由于as.matrix()将向量转换为矩阵，会导致生成许多样本。需要t来转置，一个向量对应一个样本。
  }else if(is.matrix(tstx) != TRUE){
    tstx <- as.matrix(tstx)
  }
  
  if(is.matrix(trnx1)) trnx1 <- as.matrix(trnx1)
  if(is.matrix(trnx2)) trnx2 <- as.matrix(trnx2)
  
  nt <- nrow(tstx) # 表示要测距的向量个数(行数)
  result <- matrix(rep(0, nt), nrow = 1, byrow = TRUE, dimnames = list("result", 1:nt))
  trnx1.rn <- nrow(trnx1)
  trnx2.rn <- nrow(trnx2)
  mu1 <- colMeans(trnx1)
  mu2 <- colMeans(trnx2)
  rn <- trnx1.rn + trnx2.rn
  # 判别分析函数 -- 不是特别理解透彻
  S <- (trnx1.rn - 1)*var(trnx1) + (trnx2.rn - 1)*var(trnx2)
  mu <- trnx1.rn/rn*mu1 + trnx2.rn/rn*mu2
  w <- (tstx - rep(1,nt)%o%mu)%*%solve(S, mu2 - mu1);

  for(i in 1:nt){
    if(w[i] <= 0){
      result[i] <- 1
    }else{
      result[i] <- 2
    }
  }
  
  result
}

# test example:

(discriminant.fisher(classX1, classX2))

# 总结
# 好好理解各种计算判别分析的方法！！！
