# 因子分析
# 1.1 主成分分析的推广和发展，多元统计分析中降维的方法，分析隐藏于表象的因子作用。
# 1.2 应用
# 1. 简化观测系统，将复杂关系的对象综合为少数因子，再现因子和原始变量的关系
# 2. 分类
# 1.3 因子分析类别
# 1. R型因子分析，变量之间的相关关系，通过对变量的相关阵或协方差阵内部研究，找出主因子，进行分类
# 2. Q型因子分析，样本之间的相关关系，对相似矩阵研究，找出主因子，进行分类
# 1.4 因子模型
# 1. X = μ + AF + ε F：公共因子向量 ε：特殊因子向量 A：因子载荷矩阵， A，ε 都是彼此不相关

# 2.1 参数估计
# 2.1.1 估计A(因子载荷矩阵 aij)，D(特殊方差矩阵diag(σ1^2,......))
# 2.2 主成分法

# Data Set
x<-c(1.000, 
     0.923, 1.000,
     0.841, 0.851, 1.000,  
     0.756, 0.807, 0.870, 1.000, 
     0.700, 0.775, 0.835, 0.918, 1.000, 
     0.619, 0.695, 0.779, 0.864, 0.928, 1.000, 
     0.633, 0.697, 0.787, 0.869, 0.935, 0.975, 1.000, 
     0.520, 0.596, 0.705, 0.806, 0.866, 0.932, 0.943, 1.000)

names <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")

R <- matrix(0, nrow = 8, ncol = 8, dimnames = list(names, names))

for (i in 1:8){
  for (j in 1:i){
    R[i,j] <- x[(i-1)*i/2 + j]
    R[j,i]<-R[i,j] # 形成对称矩阵
  }
}


# 主成分分析法，m为主因子的个数，取决于经验
Factor_Analysis_Principal <- function(S, m){
  nrs <- nrow(S)
  SS <- diag(S) # 取对角线的值
  sum_rank <- sum(SS)
  
  rowname <- paste("X", 1:p, sep = "")
  colname <- paste("Factor", 1:m, sep = "")
  
  A <- matrix(0, nrow = nrs, ncol = m, dimnames = list(rowname, colname))  
  # 计算S的特征根和特征向量
  S.eigen <- eigen(S)
  for(i in 1:m){
    A[,i] <- sqrt(S.eigen$values[i])*S.eigen$vectors[,i] # 特征根开方*特征向量
  }
  AAT <- diag(A %*% t(A))
  D <- SS - AAT # S近似分解等式 SS = D + AA^T
  
  result.rowname <- c("SS loadings", "Proportion Var", "Cumulation Var")
  B <- matrix(0, nrow = 3, ncol = m, dimnames = list(result.rowname, colname))
  
  # 计算因子贡献率
  for(i in 1:m){
    B[1,i] <- sum(A[,i]^2)
    B[2,i] <- B[1,i]/sum_rank
    B[3,i] <- sum(B[1,1:i])/sum_rank
  }
  
  result.method <- c("Factor Analysis-Principal Component Method")
  list(method = result.method, loadings = A, var = cbind(common_var = AAT, special_var = D), ROC = B)
}

fap <- Factor_Analysis_Principal(R,2)

# 计算误差平方和
E <- R - fap$loadings %*% t(fap$loadings)-diag(fap$var[,2])
sum(E^2)

# 2.3 主因子法

# 主因子分析法，m为主因子的个数，取决于经验。R* 简约相关阵， d特殊方差估计值
Factor_Analysis_PrinFactor <- function(R, m, d){
  nrs <- nrow(R)
  RR <- diag(R) # 取对角线的值
  sum_rank <- sum(RR)
  
  rowname <- paste("X", 1:p, sep = "")
  colname <- paste("Factor", 1:m, sep = "")
  
  A <- matrix(0, nrow = nrs, ncol = m, dimnames = list(rowname, colname))  
  
  # 迭代20次
  kmax <- 20
  k <- 1
  AAT <- RR - d
  repeat{
    diag(R) <- AAT # 简约相关阵，AAT取代R的对角线的值
    AAT1 <- AAT
    # 计算R的特征根和特征向量
    R.eigen <- eigen(R)
    for(i in 1:m){
      A[,i] <- sqrt(R.eigen$values[i])*R.eigen$vectors[,i] # 特征根开方*特征向量
    }
    AAT <- diag(A %*% t(A))
    if((sqrt(sum(AAT - AAT1))) < 1e-4 | k == kmax) break
    k <- k + 1
  }
  
  D <- RR - AAT # R近似分解等式 SS = D + AA^T
  
  result.rowname <- c("SS loadings", "Proportion Var", "Cumulation Var")
  B <- matrix(0, nrow = 3, ncol = m, dimnames = list(result.rowname, colname))
  
  # 计算因子贡献率
  for(i in 1:m){
    B[1,i] <- sum(A[,i]^2)
    B[2,i] <- B[1,i]/sum_rank
    B[3,i] <- sum(B[1,1:i])/sum_rank
  }
  
  result.method <- c("Factor Analysis-Principal Factor Method")
  list(method = result.method, loadings = A, var = cbind(common_var = AAT, special_var = D), ROC = B, Iterative = k)
}

d <- c(0.123, 0.112, 0.155, 0.116, 0.073, 0.045, 0.033, 0.095)
fapf <- Factor_Analysis_PrinFactor(R, 2, d)

# 计算误差平方和
E <- R - fapf$loadings %*% t(fapf$loadings)-diag(fapf$var[,2])
sum(E^2)

