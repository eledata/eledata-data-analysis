# 协方差矩阵
# 如果两个变量的变化趋势一致，也就是说如果其中一个大于自身的期望值，另外一个也大于自身的期望值，那么两个变量之间的协方差就是正值。
# 如果两个变量的变化趋势相反，即其中一个大于自身的期望值，另外一个却小于自身的期望值，那么两个变量之间的协方差就是负值。
# cov(x,y) = 0 说明两者之间相互独立
# cov(x,y) = E((x-Ex)*(y-Ey))
require(corrplot)

x1 <- c(65,70,70,69,66,67,68,72,66,68)
x2 <- c(45,45,48,46,50,46,47,43,47,48)
x3 <- c(27.6 ,30.7,31.8,32.6,31,31.3,37,33.6,33.1,34.2)
# 此时test_data 有3个样本总体，此时算彼此之间的协方差情况
test_data <- cbind(x1, x2, x3)
test_data

# var 方差函数，也可以计算出协方差矩阵。
# cov(x,x) = var(x)
# cov(x,y) = cov(y,x)
var(test_data)
cov(test_data)

# cor 相关系数
# 简单的相关系数的分类
# 0.8-1.0 极强相关
# 0.6-0.8 强相关
# 0.4-0.6 中等程度相关
# 0.2-0.4 弱相关
# 0.0-0.2 极弱相关或无相关

cor_t <- cor(test_data)
symnum(cor_t)
corrplot(cor_t, method = "circle")

# 使用longley数据集来测试cor
# Longley数据集来自J．W．Longley（1967）发表在JASA上的一篇论文，是强共线性的宏观经济数据,
# 包含GNP deflator(GNP平减指数)、GNP(国民生产总值)、Unemployed(失业率)、ArmedForces(武装力量)、
# Population(人口)、year(年份)，Emlpoyed(就业率)。
# LongLey数据集因存在严重的多重共线性问题，在早期经常用来检验各种算法或计算机的计算精度。

(clp <- cor(longley, method = "pearson"))
(cls <- cor(longley, method = "spearman"))
(clk <- cor(longley, method = "kendall"))

corrplot(clp, method = "circle")
corrplot(cls, method = "number")
corrplot(clk, method = "pie")

# 判别两个总体是否相关的函数，cor.test
cor.test(longley$GNP, longley$Unemployed)


