
## interval estimate
# 1. mean interval estimate
interval.est.mean <- function(x, sigma = -1, alpha = 0.05){
    n <- length(x)
    mu <- mean(x)
    if(sigma >= 0){
        tmp <- (sigma/sqrt(n))*qnorm(1-alpha/2)
        df <- n
    }else{
        tmp <- (sd(x)/sqrt(n))*qt(1-alpha/2,df = n - 1)
        df <- n - 1
    }
    data.frame(mean = mu, df = df, left = mu - tmp, right = mu + tmp)
}

# 2. sigma interval estimate
interval.est.sigma <- function(x, mu = Inf, alpha = 0.05){
    n <- length(x)
    if (mu < Inf){
        sigma <- (1/n)*sum((x - mu)^2)
        df <- n
    }else{
        sigma <- (1/(n-1))*sum((x-mu)^2)
        df <- n - 1
    }
    left <- qchisq(alpha/2, df)
    right <- qchisq(1 - alpha/2,df)
    data.frame(Var = sigma, df = df, left = left, right = right)
}









