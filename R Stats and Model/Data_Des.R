# Data source
df_people_info <- data.frame(Name = c("A", "B", "C", "D", "E"),
                             Sex = c("F", "M", "F", "F", "M"),
                             Age = c(16, 27, 28, 20, 21),
                             Height = c(180, 177, 189, 168, 172),
                             Weight = c(76, 87, 69, 79, 70)
                             )
df_people_info

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)
incomes

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

data_outline(df_people_info$Weight)

