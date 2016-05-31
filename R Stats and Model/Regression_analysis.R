# 线性回归
require(ggplot2)
# 1 积雪融化对下游灌溉的影响

snow <- data.frame(
    X = c(5.1,3.5,7.1,6.2,8.8,7.8,4.5,5.6,8,6.4),
    Y = c(1907,1287,2700,2373,3260,3000,1947,2273,3113,2493)
)

snow.plot <- ggplot(snow, aes(X, Y)) + 
  geom_point() + 
  geom_smooth(method = lm)
snow.plot

snow.lm <- lm(Y ~ 1 + X, data = snow)
summary(snow.lm)

Y <- predict(snow.lm, data.frame(X = 7),interval = "prediction",level = 0.95)
Y


Anscombe<-data.frame(
  X =c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0),
  Y1=c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68),
  Y2=c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74),
  Y3=c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.44, 5.73),
  X4=c(rep(8,7), 19, rep(8,3)),
  Y4=c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.89)
)

Anscombe.plot <- ggplot(Anscombe, aes(Y1, X)) + 
  geom_point() + 
  geom_smooth(method = lm)
Anscombe.plot
summary(lm(Y1~X, data = Anscombe))


# 1000人寿命密度图，以是否吸烟为因子

longevity <- read.csv2("longevity.csv",header = TRUE, sep = ",")
longevity$Smokes <- as.factor(longevity$Smokes)
smoke_life <- ggplot(longevity, aes(AgeAtDeath, fill = Smokes)) +
  geom_density(position = "stack") + 
  facet_grid(Smokes ~ .) + 
  theme_bw() +
  xlab("寿命密度") +
  ylab("去世年龄")

smoke_life

# 体重相对身高的散点图
height.weight <- read.csv2("01_heights_weights_genders.csv", header = TRUE, sep = ",")
head(height.weight)
hw_point <- ggplot(height.weight, aes(Height, Weight)) +
  geom_point(aes(colour = factor(Gender))) + 
  theme_bw() +
  xlab("体重") +
  ylab("身高")
hw_point


