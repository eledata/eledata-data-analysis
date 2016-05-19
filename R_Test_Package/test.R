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