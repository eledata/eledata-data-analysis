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

p <- qplot(displ,hwy, data = mpg, colour = factor(cyl))
p
summary(p)


p <- ggplot(mtcars)
summary(p)

p <- p + aes(wt, hp)
p <- p + geom_point()
p
summary(p)


p <- p + geom_point(aes(colour = "darkblue"))
p
p <- p + geom_point(aes(y = disp))
p

library(nlme)

q <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
q <- q + geom_smooth(aes(group = Subject), method = "lm", se = F)
q

q <- q + geom_smooth(aes(group = 1), method = "lm", size = 2, se = F)
q
boybox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
boybox

boybox <- boybox + geom_line(aes(group = Subject), colour = "#3366FF")
boybox

d <- ggplot(diamonds, aes(carat)) + xlim(0,3)

d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")

d + stat_bin(aes(size = ..density..), binwidth = 0.1, geom = "point", position = "identity")

d + stat_bin(
  aes(y = 1, fill = ..count..), binwidth = 0.1,
  geom = "tile", position="identity")



test.df <- data.frame(
    x = c(3,1,5),
    y = c(2,4,6),
    label = c("a","b", "c")
  )

test.p <- ggplot(test.df, aes(x,y)) + xlab(NULL) + ylab(NULL)
test.p + geom_point() + labs(title = "geom_point")
test.p + geom_bar(stat = "identity") + labs(title = "geom_bar(stat = \"identity\")")
test.p + geom_line() + labs(title = "geom_line")
test.p + geom_area() + labs(title = "geom_area")
test.p + geom_path() + labs(title = "geom_path")
test.p + geom_text(aes(label = label)) + labs(title = "geom_text")
test.p + geom_tile() + labs(title = "geom_tile")
test.p + geom_polygon() + labs(title = "geom_polygon")



# 展示数据分布

depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58,68)
# 直方图, 用了分面对比
depth_dist + geom_histogram(aes(y = ..density.., colour = cut), binwidth = 0.1) + facet_grid(cut ~ .)
# 条件密度图
depth_dist + geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill")
# 频率多边形图
depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut), binwidth = 0.1)



library(plyr)
qplot(cut, depth, data = diamonds, geom = "boxplot")
qplot(carat, depth, data = diamonds, geom = "boxplot", group = round_any(carat, 0.1, floor), xlim = c(0,3))

qplot(class, cty, data = mpg, geom = "jitter")

qplot(depth, data=diamonds, geom="density", xlim = c(54, 70))
qplot(depth, data=diamonds, geom="density", xlim = c(54, 70),
      fill = cut, alpha = I(0.2))



# 处理掩盖问题

test.data <- data.frame(
  x = rnorm(2000),
  y = rnorm(2000)
  )
test.data
norm <- ggplot(test.data, aes(x,y))
norm + geom_point()
norm + geom_point(shape = 1) # 中空的点
norm + geom_point(shape = ".") # 点的大小为像素级别

# alpha 分母表示一个位置的颜色完全变为不透明时，所需要重叠点的数量
norm + geom_point(colour = "black", alpha = 1/3) 
norm + geom_point(colour = "black", alpha = 1/5)
norm + geom_point(colour = "black", alpha = 1/10)

td <- ggplot(diamonds, aes(table, depth)) +
  xlim(50, 70) + ylim(50, 70)
td + geom_point()
td + geom_jitter()
jit <- position_jitter(width = 0.5)
td + geom_jitter(position = jit)
td + geom_jitter(position = jit, colour = alpha("black", 1/10))
td + geom_jitter(position = jit, colour = alpha("black", 1/50))
td + geom_jitter(position = jit, colour = alpha("black", 1/200))




# Chap 6

plot <- qplot(cty, hwy, data = mpg)
plot

plot + aes(x = drv)

plot + aes(x = drv) + scale_x_discrete()


p <- qplot(sleep_total, sleep_cycle, data = msleep, colour = vore)
p
# Explicitly add the default scale
p + scale_colour_hue()
# Adjust parameters of the default, here changing the appearance
# of the legend
p + scale_colour_hue("What does\nit eat?",
                     breaks = c("herbi", "carni", "omni", NA),
                     labels = c("plants", "meat", "both", "don’t know"))
# Use a different scale
p + scale_colour_brewer(palette = "Set1")



p <- qplot(cty, hwy, data = mpg, colour = displ)
p
p + scale_x_continuous("City mpg")
p + xlab("City mpg") # x 坐标轴名称换成City mpg
p + ylab("Highway mpg") # y 坐标轴名称
p + labs(x = "City mpg", y = "Highway", colour = "Displacement") # 同时开始换
p + xlab(expression(frac(miles, gallon)))






p <- qplot(cyl, wt, data = mtcars)
p
p + scale_x_continuous(breaks = c(5.5, 6.5))
p + scale_x_continuous(limits = c(5.5, 6.5))
p <- qplot(wt, cyl, data = mtcars, colour = cyl)
p
p + scale_colour_gradient(breaks = c(5.5, 6.5))
p + scale_colour_gradient(limits = c(5.5, 6.5))


qplot(log10(carat), log10(price), data = diamonds)
qplot(carat, price, data = diamonds) + scale_x_log10() + scale_y_log10()

plot <- ggplot(data = economics, aes(date, psavert)) + geom_line() + geom_hline(xintercept = 0, colour = "grey50")
plot
+ ylab("Personal savings rate") + geom_hline(xintercept = 0, colour = "grey50")
plot
plot + scale_x_date(major = "10 years")
plot + scale_x_date(
  limits = as.Date(c("2004-01-01", "2005-01-01")),
  format = "%Y-%m-%d"
)


# 颜色渐变，从浓变淡
point <- qplot(brainwt, bodywt, data = msleep, log = "xy",
               colour = vore)
area <- qplot(log10(brainwt), data = msleep, fill = vore,
              binwidth = 1)
point + scale_colour_brewer(palette = "Set1")
point + scale_colour_brewer(palette = "Set2")
point + scale_colour_brewer(palette = "Pastel1")
area + scale_fill_brewer(palette = "Set1")
area + scale_fill_brewer(palette = "Set2")
area + scale_fill_brewer(palette = "Pastel1")





plot <- qplot(brainwt, bodywt, data = msleep, log = "xy")
plot + aes(colour = vore) +
  scale_colour_manual(values = c("red", "orange", "yellow",
                                "green", "blue"))
colours <- c(carni = "red", "NA" = "orange", insecti = "yellow",herbi = "green", omni = "blue")
plot + aes(colour = vore) + scale_colour_manual(values = colours)
plot + aes(shape = vore) +
  scale_shape_manual(values = c(1, 2, 6, 0, 23))



huron <- data.frame(year = 1875:1972, level = LakeHuron)
huron
ggplot(huron, aes(year)) + 
  geom_line(aes(y = level - 5), colour = "blue") + 
  geom_line(aes(y = level + 5), colour = "red") + 
  scale_colour_manual("Direction", values = c("below" = "blue", "above" = "red"))


             
# Chap 7

mpg2 <- subset(mpg, cyl != 5&drv %in% c("4","f"))
mpg2

qplot(cty, hwy, data = mpg2)

# 一行多列
qplot(cty, hwy, data = mpg2) + facet_grid(.~cyl)
# 一列多行
qplot(cty, hwy, data = mpg2) + facet_grid(cyl~.)

# 多行多列
qplot(cty, hwy, data = mpg2) + facet_grid(drv~cyl)



# 电影 10 年评分分布
data(movies)
movies$decade <- round_any(movies$year, 10, floor)
qplot(rating, ..density.., data=subset(movies, decade > 1890),
      geom="histogram", binwidth = 0.5) +
  facet_wrap(~ decade, ncol = 6)




             
             