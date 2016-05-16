# 位置的度量
# mean sort quantile median
# 分散程度的度量&分布形状的度量
# 方差，变异系数， 极差， 标准误，偏度系数，峰度系数

# mean - 均值 v - 样本方差  s - 标准差 me - 中位数
# cv - 变异系数 css - 样本校正平方和 uss - 样本未校正平方和
# R - 极差 R1 - 四分位差 sm - 样本标准误 g1 - 偏度系数
# g2 - 峰度系数

data_outline <- function(x){
  n <- length(x)
  m <- mean(x)
  v <- var(x)
  s <- sd(x)
  me <- median(x)
  cv <- 100*s/m
  css <- sum((x - m)^2)
  uss <- sum(x^2)
  R <- max(x) - min(x)
  R1 <- quantile(x, 3/4) - quantile(x, 1/4)
  sm <- s/sqrt(n)
  g1 <- (n/((n-1)*(n-2)*s^3))*((sum(x-m)^3)/n)
  g2 <- ((n*n*(n+1))/((n-1)*(n-2)*(n-3)*(s^4)))*(sum(x-m)^4/n)-(3*(n-1)^2)/((n-2)*(n-3))
  data.frame(
    N = n, Mean = m, Var = v, SD = s, Median = me, CV = cv, CSS = css, USS = uss, R = R,
    R1 = R1, StandardMean = sm, Skewness = g1, Kurtosis = g2, row.names =  1
  )
}

p_data <- scan("Problem_3.1.data")
data_outline(p_data)

#hist 直方图, 默认的是频率直方图。
hist(p_data)
# 密度直方图
hist(p_data, freq = FALSE)
#密度估计函数
density(p_data)
lines(density(p_data), col = "red")

#经验分布函数,可以估计样本总体的分布函数F(X)
ecdf(p_data)
plot(ecdf(p_data), verticals = TRUE, do.p = FALSE)
x <- 60:90
lines(x, pnorm(x, mean(p_data), sd(p_data)))

#QQ 图可以帮我们鉴别样本的分布是否近似于某种类型的分布
qqnorm(p_data)
qqline(p_data)

#茎叶图， 看出数据的分布情况
stem(p_data)

#箱线图， 展现数据分布的主要特征
boxplot(p_data)

#5数总括， min, Q1, mid, Q3, max
fivenum(p_data)






#正态性W检验
p_data <- scan("Problem_3.1.data")
shapiro.test(p_data)

#经验分布的K-S检验方法， 检验经验分布和假设总体分布函数之间的差异
ks.test(p_data)





