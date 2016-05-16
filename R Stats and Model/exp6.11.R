Anscombe<-data.frame(
  X =c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0),
  Y1=c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68),
  Y2=c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74),
  Y3=c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.44, 5.73),
  X4=c(rep(8,7), 19, rep(8,3)),
  Y4=c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.89)
)

summary(lm(Y1~X, data = Anscombe))
plot(Anscombe$Y1, Anscombe$X)






interval_estimate_m <- function(x, sigma = -1, alpha = 0.05){
    n <- length(x)
    m <- mean(x)
    
    if(sigma >= 0){
      tmp <- (sigma/sqrt(n))*qnorm(1 - alpha/2)
      df <- n
    }
    else{
      tmp <- (sd(x)/sqrt(n))*qt(1 - alpha/2, n-1)
      df <- n - 1
    }
    data.frame(mean = m, df = df, a = m - tmp, b = m + tmp)
}

x <- c(14.6, 15.1, 14.9, 14.8, 15.2, 15.1)
interval_estimate_m(x)








interval_estimate_var <- function(x, m = Inf, alpha = 0.05){
  n <- length(x)
  
  if(m < Inf){
    S <- sum((x-m)^2)/n
    df <- n
  }
  else{
    S <- sum((x-m)^2)/(n - 1)
    df <- n - 1
  }
  a <- df*S/qchisq(1 - alpha/2, df)
  b <- df*S/dchisq(alpha/2, df)
  data.frame(var = S, df = df, a = a, b = b)
}

x <- c(14.6, 15.1, 14.9, 14.8, 15.2, 15.1)

interval_estimate_var(x, m = 14.95)

interval_estimate_var(x)


interval_estimate_mm <- function(x, y, sigma = c(-1, -1), var.equal = FALSE, alpha = 0.05){
n1 <- length(x)
n2 <- length(y)
xm <- mean(x)
ym <- mean(y)
if (all(sigma >= 0 )){
  tmp <- qnorm(1-alpha/2)*sqrt(sigma[1]^2/n1 + sigma[2]^2/n2)
  df <- n1 + n2
}else{
  if(var.equal == TRUE){
    sw <- sqrt(((n1-1)*var(x)^2 + (n2-1)*var(y)^2)/(n1 + n2 -2))
    tmp <- qt(1-alpha/2, n1+n2-2)*sqrt(1/n1 + 1/n2)*sw
    df <- n1 + n2 -2
  }else{
    S1 <- var(x)
    S2 <- var(y)
    nu <- (S1^2/n1 + S2^2/n2)^2/((S1)^4/(n1^2*(n1-1)) + (S2)^4/(n2^2*(n2-1)))
    tmp <- qt(1-alpha/2,nu)*sqrt(S1^2/n1 + S2^2/n2)
    df <- nu
  }
}
data.frame(mean  = xm - ym, df = df, a = xm-ym-tmp, b = xm-ym + tmp)
}
x <- rnorm(12, 501.1, 2.4)
y <- rnorm(17, 499.7, 4.7)
interval_estimate_mm(x,y)



