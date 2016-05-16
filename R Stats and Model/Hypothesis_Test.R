#P Value Calculation Method
P_Value <- function(cdf, x, paramet = numeric(0), side = 0){
  n <- length(paramet)
  P <- switch (n + 1,
    cdf(x),
    cdf(x, paramet),
    cdf(x, paramet[1], paramet[2]),
    cdf(x, paramet[1], paramet[2], paramet[3])
  )
  if (side < 0) P
  else if (side > 0) 1 - P
  else 
    if(P < 0.5) 2*P
    else 2*(1 - P)
}

mean_test1 <- function(x, mu  = 0, sigma = -1, side = 0){
  n <- length(x)
  xb <- mean(x)
  if(sigma > 0){
    z <- (xb - mu)/(sigma/sqrt(n))
    P <- P_Value(pnorm, z, side = side)
    data.frame(mean = xb, df = n, Z = z, P_Value = P)
  }else{
    t <- (xb - mu)/(sd(x)/sqrt(n))
    P <- P_Value(pt, t, paramet = n - 1, side = side)
    data.frame(mean = xb, df = n - 1, T = t, P_Value = P)
  }
}

X<-c(159, 280, 101, 212, 224, 379, 179, 264,
     222, 362, 168, 250, 149, 260, 485, 170)
mean_test1(X, mu = 225, side = 1)

mean_test2 <- function(x, y, var.equal = FALSE, sigma = c(-1, -1), side = 0){
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  if(all(sigma > 0)){
    z <- (mx - my)/(sqrt(sigma[1]^2/nx + sigma[2]^2/ny))
    P <- P_Value(pnorm, z, side = side)
    data.frame(mean = mx - my, df = nx + ny, Z = z, P_Value = P)
  }else{
    if(var.equal == TRUE){
      sw <- sqrt(((nx - 1)*sd(x)^2 + (ny - 1)*sd(x)^2)/(nx + ny - 2))
      t <- (mx - my)/(sw*(sqrt(1/nx + 1/ny)))
      nu <- nx + ny - 2
    }else{
        s1 <- var(x)
        s2 <- var(y)
        nu <- (s1/nx + s2/ny)^2/(s1^2/(nx^2*(nx - 1)) + s2^2/ny^2*(ny - 1))
        t <- (mx - my)/sqrt(s1/nx + s2/ny)
      }
    P <- P_Value(pt, t, nu, side = side)
    data.frame(mean = mx - my, df = nu, T = t, P_Value = P)
  }
}

X<-c(78.1, 72.4, 76.2, 74.3, 77.4, 78.4, 76.0, 75.5, 76.7, 77.3)
Y<-c(79.1, 81.0, 77.3, 79.1, 80.0, 79.1, 79.1, 77.3, 80.2, 82.1)

mean_test2(X,Y, var.equal=TRUE, side=-1)
mean_test2(X,Y, side=-1)


var_test1 <- function(x, sigma = 1, mu = Inf, side = 0){
  n <- length(x)
  if(mu < Inf){
    s <- sum(x - mu)^2/n
    df = n
  }else{
    s <- var(x)
    df = n - 1
  }
  k <- df*s/sigma
  P <- P_Value(pchisq, k, paramet = df, side = side)
  data.frame(var = s, df = df, chisq = k, P_Value = P)
}
var_test1(X, sigma = 3.5)

var_test2 <- function(x, y, mu = c(Inf, Inf), side = 0){
  nx <- length(x)
  ny <- length(y)
  if(all(mu < Inf)){
    sx <- sum(x - mu[1])^2/nx
    sy <- sum(y - mu[2])^2/ny
    df1 <- nx
    df2 <- ny
  }else{
    sx <- var(x)
    sy <- var(y)
    df1 <- nx - 1
    df2 <- ny - 1
  }
  f <- sx/sy
  P <- P_Value(pf, f, paramet = c(df1, df2), side = side)
  data.frame(rate = f, df1 = df1, df2 = df2, F = f, P_Value = P)
}

var_test2(X,Y)

#pearson 拟合优度卡方检验
# 1. 检验样本是否符合某分布


#判断某个总体的不同的水平下是否存在显著差异。
X <- c(210, 312, 170, 85, 223)
n <- sum(X)
m <- length(X)
p <- rep(1/m, m)
K <- sum((X - n*p)^2/(n*p))
K
pr <- 1 - pchisq(K, m - 1)
pr

#检验某个总体是否符合理论分布
X <- 0:6
Y <- c(7, 10, 12, 8, 3, 2, 0)
q <- ppois(X, mean(rep(X,Y)))
n <- length(Y)
p[1] <- q[1]
p[n] <- 1 - q[n-1]
for(i in 2:(n - 1)){
  p[i] <- q[i] - q[i - 1]
}

chisq.test(Y,p=p)

X <- 0:4
Y <- c(7, 10, 12, 8, 5)
q <- ppois(X, mean(rep(X,Y)))
n <- length(Y)
p[1] <- q[1]
p[n] <- 1 - q[n-1]
for(i in 2:(n - 1)){
  p[i] <- q[i] - q[i - 1]
}

chisq.test(Y,p=p)


#列联表
X <- data.frame(
  a1 <- c(20, 24, 80, 82),
  a2 <- c(22, 38, 104, 125),
  a3 <- c(13, 28, 81, 113),
  a4 <- c(7, 18, 54, 92)
)
chisq.test(X)
X
as.matrix(X, nrow = 4, ncol = 4)
chisq.test(X)

#Fisher 精确独立检验
x <- c(4,5,18,6)
dim(x) <- c(2,2)
fisher.test(x)

#spearman
x <- c(1,2,3,4,5,6)
y <- c(6,5,4,3,2,1)
cor.test(x,y,method = "spearman")



