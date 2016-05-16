ore<-data.frame(
     x=c(67, 54, 72, 64, 39, 22, 58, 43, 46, 34),
     y=c(24, 15, 23, 19, 16, 11, 20, 16.1, 17, 13)
)
ore.mx<-mean(ore$x); ore.mx
ore.my<-mean(ore$y); ore.my
ore.s<-cov(ore); ore.s
ore.r<-cor(ore); ore.r

attach(ore)
cor.test(x,y)

cor.test(x,y, method="spearman")

cor.test(x,y, method="kendall")


newdata <- data.frame(
    x = c(6,	7,	3,	1,	2,	3,	10,	17,	12,	12,	12,	9,	20,	22,	18,	21,	13,	13,	15,	15,	16,	19,	14,	17,	14,	19,	18,	23,	20,	18),
    y = c(51,	25,	44,	25,	31,	10,	52,	11,	23,	15,	36,	15,	3,	26,	8,	40,	23,	9,	12,	18,	24,	12,	16,	9,	11,	19,	38,	30,	0,	0)
)

newdata.mx<-mean(newdata$x); newdata.mx
newdata.my<-mean(newdata$y); newdata.my
newdata.s<-cov(newdata); newdata.s
newdata.r<-cor(newdata); newdata.r

attach(newdata)
r <- cor.test(x,y)
r.t <- r[1]
r.df <- r[2]
r.p <- r[3]
r.estcor <- r[4]
