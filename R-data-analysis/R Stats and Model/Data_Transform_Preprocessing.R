# 数据读取
# 1. 从csv文件中读取数据，然后在按照观察数据，校验数据，测试数据，比例为 70/15/15来分

# 结果可重复性
crv$seed <- 42 

# Load the data.
crs$dataset <- read.csv("file:///C:/Program Files/R/R-3.1.2/library/rattle/csv/audit.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# training/validate/test datasets.
set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 2000 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 1400 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 300 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 300 observations， 用了两次减法操作

# 集合操作函数
# test data:
(x <- c(sort(sample(1:20, 9)), NA))
# [1]  2  4  6  7 12 15 16 19 20 NA
(y <- c(sort(sample(3:23, 7)), NA))
# [1]  3  6  7  9 15 17 21 NA
union(x, y) # 并集操作
# [1]  2  4  6  7 12 15 16 19 20 NA  3  9 17 21
intersect(x, y) # 交集操作
# [1]  6  7 15 NA
setdiff(x, y) # X - Y
# [1]  2  4 12 16 19 20
setdiff(y, x) # Y - X
# [1]  3  9 17 21
setequal(x, y) # 求集合是否相等
# [1] FALSE


# 数据转换
# 1. 中心化变换 x = x - mu(x), 变换后均值为0，方差阵不变
# 2. 中心标准化， x = (x-mu(x))/S, 均值为0， 方差为1，变换后的数据与量纲无关

scale(x, center = TRUE, scale = TRUE)

x <- matrix(1:10, ncol = 2)
(center.x <- scale(x, scale = FALSE)) # 中心化

(center.x <- scale(x)) # 中心化和标准化, cov值为1

# 3.极差标准化
# x = (x-mu(x))/R R= Max(x) - Min(x). R 为极差，变换之后，均值为0， 极差为1
x <- matrix(1:10, ncol = 2)
center <- sweep(x, 2, apply(x, 2, mean)) # 默认FUN = ”-“
R <- apply(x, 2, max) - apply(x, 2, min)
x.r <- sweep(center, 2, R, "/") # 1 行，2列，"/" 求除
x.r

# 4.极差正规化
# x = (x-min(x))/R R= Max(x) - Min(x). R 为极差，变换之后，均值为0， 极差为1
x <- matrix(1:10, ncol = 2)
center <- sweep(x, 2, apply(x, 2, min)) # 默认FUN = ”-“
R <- apply(x, 2, max) - apply(x, 2, min)
x.r <- sweep(center, 2, R, "/") # 1 行，2列，"/" 求除
x.r


















