# 线性回归

# 1 积雪融化对下游灌溉的影响

snow <- data.frame(
    X = c(5.1,3.5,7.1,6.2,8.8,7.8,4.5,5.6,8,6.4),
    Y = c(1907,1287,2700,2373,3260,3000,1947,2273,3113,2493)
)

plot(snow$X,snow$Y)

snow.lm <- lm(Y ~ 1 + X, data = snow)
summary(snow.lm)

Y <- predict(snow.lm, data.frame(X = 7),interval = "prediction",level = 0.95)
Y
