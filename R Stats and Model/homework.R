Chap 7

anova.tab<-function(fm){
  tab<-summary(fm)
  k<-length(tab[[1]])-2
  temp<-c(sum(tab[[1]][,1]), sum(tab[[1]][,2]), rep(NA,k))
  tab[[1]]["Total",]<-temp
  tab
}

7.1
item <- data.frame(
  x = c(115,116,98,83,103,107,118,116,73,89,85,97),
  a = factor(c(rep(1,4), rep(2,4),rep(3,4)))
  )
item$x[item$a==1]

shapiro.test(item$x[item$a==1])
shapiro.test(item$x[item$a==2])
shapiro.test(item$x[item$a==3])

bartlett.test(x~a, data=item)

item.aov <- aov(x~a, data = item)
anova.tab(item.aov)

item.mean <- data.frame (A = mean(item$x[item$a==1]),
                         B = mean(item$x[item$a==2]),
                         c = mean(item$x[item$a==3])
                           )
item.mean
source("interval_estimate1.R"); 
interval_estimate1(item$x[item$a==1])
interval_estimate1(item$x[item$a==2])
interval_estimate1(item$x[item$a==3])
a <- t.test(item$x[item$a==1]);a 

pairwise.t.test(item$x, item$a, p.adjust.method = "none")

7.2

item_old <- data.frame(
  Y=c(20,18,19,17,15,16,13,18,22,17,
      26,19,26,28,23,25,24,25,18,22,27,24,12,14),
  A = factor(c(rep(1,10), rep(2,6),rep(3,6),rep(4,2)))
  )
item_old
item_old.aov <- aov(Y~A, data = item_old)
anova.tab(item_old.aov)
pairwise.t.test(item_old$Y, item_old$A, p.adjust.method = "none")





7.3

rat<-data.frame(X=c(30,27,35,35,29,33,32,36,26,41,33,31,43,45,53,44,
                    51,53,54,37,47,57,48,42,82,66,66,86,56,52,76,83,
                    72,73,59,53),A=gl(3,12))
rat$A

shapiro.test(rat$X[rat$A==1])
shapiro.test(rat$X[rat$A==2])
shapiro.test(rat$X[rat$A==3])
bartlett.test(X~A, data=rat)


7.5
sleep<-data.frame(Y=c(23.1,57.6,10.5,23.6,11.9,54.6,21.0,20.3,22.7,
                      53.2,9.7,19.6,13.8,47.1,13.6,23.6,22.5,53.7,10.8,21.1,13.7,39.2,
                      13.7,16.3,22.6,53.1,8.3,21.6,13.3,37.0,14.8,14.8),X=gl(4,8))
sleep.aov <- aov(Y~X, data = sleep)
summary(sleep.aov)

7.6
pro<-data.frame(Y=c(4.6,4.3,6.1,6.5,6.8,6.4,6.3,6.7,3.4,3.8,4.0,3.8, 
                    4.7,4.3,3.9,3.5,6.5,7.0),A=gl(3,2,18),B=gl(3,6,18))
pro.aov <- aov(Y~A+B+A:B, data = pro)
summary(pro.aov)



