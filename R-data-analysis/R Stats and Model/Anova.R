## aov.tab function
aov.tab <- function(aov.res){
  aov.sum <- summary(aov.res)
  k <- length(aov.sum) + 2
  temp <- c(sum(aov.sum[[1]][,1]),sum(aov.sum[[1]][,2]),rep(NA,k))
  aov.sum[[1]]["Total",] <- temp
  aov.sum  
}

# 使用前设置一下路径 setwd("~/GitHub/r_dataanalysis/R Stats and Model/Test")
source("IntervalEst.R")
# 1 aov, interval estimate multiple check

product <- data.frame(
  X = c(115,116,98,83,103,107,118,116,73,89,85,97),
  A = gl(3,4,12)
)

# show the level 1 product details: product$X[product$A == 1]

product.aov <- aov(X ~ A, data = product)
aov.tab(product.aov)

# calculate the interval estimate
interval.est.mean(product$X[product$A == 1])
interval.est.mean(product$X[product$A == 2])
interval.est.mean(product$X[product$A == 3])

interval.est.sigma(product$X[product$A == 1], mu = mean(product$X[product$A == 1]))

# mulitple check
pairwise.t.test(product$X, product$A, p.adjust.method = "none")

# 正态性和齐次性检验
shapiro.test(product$X[product$A == 1])

bartlett.test(product)

# Friedman 秩检验，需要每组要均匀分配,需要有序号。

X<-matrix(
  c(1.00, 1.01, 1.13, 1.14, 1.70, 2.01, 2.23, 2.63,
    0.96, 1.23, 1.54, 1.96, 2.94, 3.68, 5.59, 6.96,
    2.07, 3.72, 4.50, 4.90, 6.00, 6.84, 8.23, 10.33),
  ncol = 3, dimnames = list(1:8, c("A","B","C"))
)

X

Y<-data.frame(
  x = c(1.00, 1.01, 1.13, 1.14, 1.70, 2.01, 2.23, 2.63,
    0.96, 1.23, 1.54, 1.96, 2.94, 3.68, 5.59, 6.96,
    2.07, 3.72, 4.50, 4.90, 6.00, 6.84, 8.23, 10.33),
  A = gl(3,8,24),  # 这里需要看看
  B = gl(8,1,24)
)
# gl(3,8,24), 1,2,3 重复8次，11111111,22222222,33333333,总共24次
# gl(8,1,24), 1,2,3,4,5,6,7,8 重复1次，然后总共要24次

Y
friedman.test(x~A|B, data = Y)

# Kruskal.test 秩检验 每组随机次数

food<-data.frame(
  x=c(164, 190, 203, 205, 206, 214, 228, 257,
      185, 197, 201, 231,
      187, 212, 215, 220, 248, 265, 281,
      202, 204, 207, 227, 230, 276),
  g=factor(rep(1:4, c(8,4,7,6)))
)
food

kruskal.test(x~g, data = food) # 不用序号

# 双因素方差分析
agriculture<-data.frame(
  Y=c(325, 292, 316, 317, 310, 318, 
      310, 320, 318, 330, 370, 365),
  A=gl(4,3),
  B=gl(3,1,12)
)
# 未考虑交叉影响
agriculture.aov <- aov(Y ~ A + B, data = agriculture)
summary(agriculture.aov)
aov.tab(agriculture.aov)

# 考虑交叉影响
# Ai Bj 代表A，B水平数量，没有交叉的时候，Ai，Bj 之间只有一次实验，假如，Ai，Bj
# 之间有k次重复实验。那么就有交叉影响了。

tree<-data.frame(
  A=gl(3,20,60),
  B=gl(4,5,60),
  Y=c(23, 25, 21, 14, 15, 20, 17, 11, 26, 21, 
      16, 19, 13, 16, 24, 20, 21, 18, 27, 24,
      28, 30, 19, 17, 22, 26, 24, 21, 25, 26,
      19, 18, 19, 20, 25, 26, 26, 28, 29, 23,
      18, 15, 23, 18, 10, 21, 25, 12, 12, 22, 
      19, 23, 22, 14, 13, 22, 13, 12, 22, 19)
)
tree
tree.aov <- aov(Y~A + B + A:B, data = tree)
aov.tab(tree.aov)

# 另外一个例子
quality <- data.frame(
  X = c(4.6,4.3,6.1,6.5,6.8,6.4,6.3,6.7,3.4,3.8,4,3.8,4.7,4.3,3.9,3.5,6.5,7),
  A = gl(3,2,18),
  B = gl(3,6,18)
)
quality
tree.aov <- aov(X~A + B + A:B, data = quality)
aov.tab(tree.aov)

# Running Result
# Df Sum Sq Mean Sq F value   Pr(>F)    
# A            2  4.441   2.221   29.83 0.000107 ***
# B            2  3.974   1.987   26.69 0.000164 ***
# A:B          4 21.159   5.290   71.06 8.34e-07 ***
# Residuals    9  0.670   0.074                     
# Total       17 30.244   
# 可以看出交互作用 A:B > A > B

# 找出交互作用
ab <- function(x,y){
  n <- length(x)
  z <- rep(0,n)
  for(i in 1:n){
    if(x[i] == y[i]){
      z[i] <- 1
    }else{
      z[i] <- 2
    }
  }
    factor(z)
}

quality$AB <- ab(quality$A, quality$B)

K <- matrix(0, nrow = 3, ncol = 3, dimnames = list(1:3, c("A","B","AB")))
for(i in 2:4){
  for(j in 1:3){
    K[j, i - 1] <- mean(quality$X[quality[i] == j])   # quality$X[quality[3]--B == 3--第三行] B3的数据
  }
}

K
#     A -- 3  B -- 1   AB -- 2 选均值大的那个
# 1 5.150000 5.783333 4.933333
# 2 4.533333 4.666667 5.250000
# 3 5.750000 4.983333      NaN --- A3B1

mu <- mean(quality$X[quality$A == 3&quality$B== 1])
interval.est.mean(quality$X[quality$A == 3&quality$B== 1])

# 多重检验
pairwise.t.test(quality$X, quality$A)
pairwise.t.test(quality$X, quality$B)
pairwise.t.test(quality$X, quality$AB)





