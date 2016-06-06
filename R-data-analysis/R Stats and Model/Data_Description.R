# Data Description
# Test Data
x <- c(74.3,78.8,68.8,78.0,70.4,80.5,80.5,69.7,71.2,73.5,
79.5,75.6,75.0,78.8,72.0,72.0,72.0,74.3,71.2,72.0,
75.0,73.5,78.8,74.3,75.8,65.0,74.3,71.2,69.7,68.0,
73.5,75.0,72.0,64.3,75.8,80.3,69.7,74.3,73.5,73.5,
75.8,75.8,68.8,76.5,70.4,71.2,81.2,75.0,70.4,68.0,
70.4,72.0,76.5,74.3,76.5,77.6,67.3,72.0,75.0,74.3,
73.5,79.5,73.5,74.7,65.0,76.5,81.6,75.4,72.7,72.7,
67.2,76.5,72.7,70.4,77.2,68.8,67.3,67.3,67.3,72.7,
75.8,73.5,75.0,73.5,73.5,73.5,72.7,81.6,70.3,74.3,
73.5,79.5,70.4,76.5,72.7,77.2,84.3,75.0,76.5,70.4)

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
# cv - 变异系数 css - 样本校正平方差 uss - 样本未校正平方和
# R - 极差 R1 - 四分位差 sm - 样本标准差 g1 - 偏度系数
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
data_outline(x)

#hist 直方图 默认的是频率直方图
p_data <- x

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

#5数总括图 min, Q1, mid, Q3, max
fivenum(p_data)

#正态性W检验
p_data <- scan("Problem_3.1.data")
shapiro.test(p_data)

#经验分布的K-S检验方法， 检验经验分布和假设总体分布函数之间的差异
ks.test(p_data)


# 相关性检验

p_data <- data.frame(
  x <- c(2,4,3,2,4,7,7,2,2,5,4,3),
  y <- c(5,6,8,5,10,7,12,12,6,6,6,8),
  z <- c(7,11,6,6,7,9,5,5,10,6,3,10)
)
p_data
# Step 1: calculate the mean
p_data.mx <- mean(p_data$x)
p_data.mx
p_data.my <- mean(p_data$y)
p_data.my
p_data.mz <- mean(p_data$z)
p_data.mz


# Step 2: Calculate the cov&cor
p_data.v <- cov(p_data)
p_data.v
p_data.r <- cor(p_data)
p_data.r

# Step 3: Using the cor.test to verify the sample's cor and tot's cor
cor.test(~x + y, data = p_data)
cor.test(~x + z, data = p_data)
cor.test(~y + z, data = p_data)


p_data <- data.frame(
  x <- c(2,4,3,2,4,7,7,2,2,5,4,NaN),
  y <- c(5,6,8,5,10,7,12,12,6,6,NaN,NaN),
  z <- c(7,11,6,6,7,9,5,5,10,6,3,10)
)
plot(p_data)
boxplot(p_data)
plot(z)



building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2016-06-04 17:51:57 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///C:/Program Files/R/R-3.2.5/library/rattle/csv/audit.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# 可以保持每次随机抽样数据的一致性
set.seed(crv$seed)
crs$nobs <- nrow(crs$dataset) # 2000 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 1400 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 300 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 300 observations

# The following variable selections have been noted.

crs$input <- c("Age", "Employment", "Education", "Marital",
               "Occupation", "Income", "Gender", "Deductions",
               "Hours")

crs$numeric <- c("Age", "Income", "Deductions", "Hours")

crs$categoric <- c("Employment", "Education", "Marital", "Occupation",
                   "Gender")

crs$target  <- "TARGET_Adjusted"
crs$risk    <- "RISK_Adjustment"
crs$ident   <- "ID"
crs$ignore  <- "IGNORE_Accounts"
crs$weights <- NULL





