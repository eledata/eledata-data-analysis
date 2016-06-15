# 主成分分析
# 1.1 含义：将多指标转化为少数几个综合指标的方法。
# PCA的思想是将n维特征映射到k维上（k<n），这k维是全新的正交特征。这k维特征称为主元，是重新构造出来的k维特征，
# 而不是简单地从n维特征中去除其余n-k维特征。
# 求解方法在<<统计建模与R>>中详细介绍。
# 基本步骤：求协方差--> 求特征值和特征向量--> 选取最大的几个，占到整体的9成以上的，然后根据特征向量来映射Z
# 是否要选择在求解之前，数据进行标准化

# 学生体型主成分分析
# X1 : 身高 X2 : 体重 X3 : 胸围 X4 : 坐高

student <- data.frame(
  X1 = c(148, 139, 160, 149, 159, 142, 153, 150, 151, 139, 
       140, 161, 158, 140, 137, 152, 149, 145, 160, 156, 
       151, 147, 157, 147, 157, 151, 144, 141, 139, 148), 
  X2 = c(41, 34, 49, 36, 45, 31, 43, 43, 42, 31, 
       29, 47, 49, 33, 31, 35, 47, 35, 47, 44,
       42, 38, 39, 30, 48, 36, 36, 30, 32, 38),
  X3 = c(72, 71, 77, 67, 80, 66, 76, 77, 77, 68, 
       64, 78, 78, 67, 66, 73, 82, 70, 74, 78, 
       73, 73, 68, 65, 80, 74, 68, 67, 68, 70),
  X4 = c(78, 76, 86, 79, 86, 76, 83, 79, 80, 74, 
       74, 84, 83, 77, 73, 79, 79, 77, 87, 85, 
       82, 78, 80, 75, 88, 80, 76, 76, 73, 78)
)

student.princomp <- princomp(student)
summary(student.princomp, loading = TRUE)

# result:
# Importance of components:
#   Comp.1     Comp.2     Comp.3
# Standard deviation     10.9842501 3.23837983 1.55316058
# Proportion of Variance  0.8913911 0.07747883 0.01782215
# Cumulative Proportion   0.8913911 0.96886995 0.98669211
# Comp.4
# Standard deviation     
# Proportion of Variance 0.01330789
# Cumulative Proportion  1.00000000

# Loadings:
#   Comp.1 Comp.2 Comp.3 Comp.4
# X1  0.624  0.646  0.224 -0.379
# X2  0.559 -0.346 -0.746 -0.108
# X3  0.408 -0.660  0.624       
# X4  0.362  0.166         0.915


# 手工计算
# 1. 计算协方差
(student.cov <- cov(student))
# 2. 计算协方差的特征向量和特征值
student.eigen <- eigen(student.cov)
# 特征值
student.eigen$values
# 特征向量
student.eigen$vectors

# 3. 计算方差贡献率和累积贡献率
(student.pov <- (student.eigen$values/sum(student.eigen$values)))
(student.cp <- cumsum(student.pov))

# 4. 主成分的值
(student.z <- sqrt(student.eigen$values))

student.z <- z.show(student, student.eigen$vectors)
student.z
(student.z.cov <- cov(t(student.z)))

z.show <- function(data, eigen.vec){
  k <- length(data)
  tmp.z <- matrix(rep(0, k^2), nrow = k, ncol = k, byrow = TRUE)
  for(i in 1:k){
    tmp.z[i,] <- colSums(t(eigen.vec[,i]) * data)
  }
  tmp.z
}







