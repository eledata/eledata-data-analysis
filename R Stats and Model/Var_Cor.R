# 协方差矩阵
require(corrplot)

x1 <- c(65,70,70,69,66,67,68,72,66,68)
x2 <- c(45,45,48,46,50,46,47,43,47,48)
x3 <- c(27.6 ,30.7,31.8,32.6,31,31.3,37,33.6,33.1,34.2)

test_data <- cbind(x1, x2, x3)
test_data

# var 方差函数，也可以计算出协方差矩阵。
var(test_data)
cov(test_data)

# cor 相关新系数
cor_t <- cor(test_data)
symnum(cor_t)
corrplot(cor_t, method = "number")

# 使用longley数据集来测试cor
# Longley数据集来自J．W．Longley（1967）发表在JASA上的一篇论文，是强共线性的宏观经济数据,
# 包含GNP deflator(GNP平减指数)、GNP(国民生产总值)、Unemployed(失业率)、ArmedForces(武装力量)、
# Population(人口)、year(年份)，Emlpoyed(就业率)。
# LongLey数据集因存在严重的多重共线性问题，在早期经常用来检验各种算法或计算机的计算精度。

(clp <- cor(longley, method = "pearson"))
(clS <- cor(longley, method = "spearman"))
(clS <- cor(longley, method = "spearman"))

corrplot(clp, method = "number")
corrplot(clS, method = "number")
corrplot(clS, method = "number")




## How much do they differ?
i <- lower.tri(Cl)
cor(cbind(P = Cl[i], S = clS[i], K = clK[i]))


## cov2cor() scales a covariance matrix by its diagonal
##           to become the correlation matrix.
cov2cor # see the function definition {and learn ..}
stopifnot(all.equal(Cl, cov2cor(cov(longley))),
          all.equal(cor(longley, method = "kendall"),
                    cov2cor(cov(longley, method = "kendall"))))

##--- Missing value treatment:

C1 <- cov(swiss)
range(eigen(C1, only.values = TRUE)$values) # 6.19        1921

## swM := "swiss" with  3 "missing"s :
swM <- swiss
colnames(swM) <- abbreviate(colnames(swiss), min=6)
swM[1,2] <- swM[7,3] <- swM[25,5] <- NA # create 3 "missing"

## Consider all 5 "use" cases :
(C. <- cov(swM)) # use="everything"  quite a few NA's in cov.matrix
try(cov(swM, use = "all")) # Error: missing obs...
C2 <- cov(swM, use = "complete")
stopifnot(identical(C2, cov(swM, use = "na.or.complete")))
range(eigen(C2, only.values = TRUE)$values) # 6.46   1930
C3 <- cov(swM, use = "pairwise")
range(eigen(C3, only.values = TRUE)$values) # 6.19   1938

## Kendall's tau doesn't change much:
symnum(Rc <- cor(swM, method = "kendall", use = "complete"))
symnum(Rp <- cor(swM, method = "kendall", use = "pairwise"))
symnum(R. <- cor(swiss, method = "kendall"))

## "pairwise" is closer componentwise,
summary(abs(c(1 - Rp/R.)))
summary(abs(c(1 - Rc/R.)))

## but "complete" is closer in Eigen space:
EV <- function(m) eigen(m, only.values=TRUE)$values
summary(abs(1 - EV(Rp)/EV(R.)) / abs(1 - EV(Rc)/EV(R.)))





x <- c(1,2)





