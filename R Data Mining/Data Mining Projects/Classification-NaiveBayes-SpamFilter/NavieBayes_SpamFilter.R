# 多个决策边界进行分类

# Data
require(ggplot2)
x <- runif(1000, 0, 40)
y1 <- cbind(runif(100, 0, 10), 1)
y2 <- cbind(runif(800, 10, 30), 2)
y3 <- cbind(runif(100, 30, 40), 1)
value <- data.frame(cbind(x, rbind(y1, y2, y3)), stringsAsFactors = TRUE) # 表明字符可转因子

# 用ggplot2画图
hline <- data.frame(hl = c(10, 30)) # 用于分割geom_hline
mutil <- ggplot(value, aes(x, V2)) +
  geom_jitter(aes(shape = as.factor(V3)), height = 2) +
  scale_shape_discrete(guide = "none", solid = FALSE) +
  geom_hline(aes(yintercept = hl), linetype = 3, hline) +
  theme_bw() +
  xlab("X") +
  ylab("Y")
mutil


# 垃圾邮件分类
require(tm)

# 设置数据路径
spam.path <- file.path("data", "spam")
spam2.path <- file.path("data", "spam_2")
easyham.path <- file.path("data", "easy_ham")
easyham2.path <- file.path("data", "easy_ham_2")
hardham.path <- file.path("data", "hard_ham")
hardham2.path <- file.path("data", "hard_ham_2")

# 读取邮件正文
get.msg <- function(path){
  
}


