library("ggplot2")

set.seed(1000)
dsmall <- diamonds[sample(nrow(diamonds),100),]


# 1 qplot basic

qplot(carat, price, data = diamonds)

qplot(log(carat),log(price), data = diamonds)

qplot(carat, x*y*z, data = diamonds)

qplot(carat, price, data = dsmall, colour = color)

qplot(carat, price, data = dsmall, shape = cut)

# 调节alpha值，看数据重叠的情况
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))
qplot(carat, price, data = diamonds, alpha = I(1/500))

# 2 几何对象 geom

# 几何对象用集合传入geom
qplot(carat, price, data = dsmall, geom = c("point","smooth"))
qplot(carat, data = diamonds, geom = c("histogram"))




