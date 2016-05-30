library(maps)
library(ggplot2)

USA.POP <- ggplot(us.cities, aes(x = long, y = lat)) + 
  xlim(-130, -65) +
  borders("state", size=0.5) + # 画出美国州地图
  geom_point(aes(size = log(pop), color = factor(capital), alpha = 1/50)) + # 画出人口对数数据，用点来表示
  scale_size(range=c(0, 7), name = "log(City population)") +
  scale_color_manual(values = c("black", "red"), labels = c("state capital", "city")) +
  guides(color = guide_legend(title=NULL)) + 
  scale_alpha(guide = FALSE) +
  labs(x = "longtitude", y = "latitude", title = "City Population in the United States") +
  theme(plot.title = element_text(size=20))

USA.POP
#输出图像 并用cairo包进行抗锯齿处理
ggsave(USA.POP, file = "USA_POP.png", type = "cairo", width = 10, height = 6.75)






if (require("maps")) {
  
  ia <- map_data("county", "iowa")
  mid_range <- function(x) mean(range(x))
  seats <- plyr::ddply(ia, "subregion", plyr::colwise(mid_range, c("lat", "long")))
  ggplot(ia, aes(long, lat)) +
    geom_polygon(aes(group = group), fill = NA, colour = "grey60") +
    geom_text(aes(label = subregion), data = seats, size = 2, angle = 45)
  
  data(us.cities)
  capitals <- subset(us.cities, capital == 2)
  ggplot(capitals, aes(long, lat)) +
    borders("state") +
    geom_point(aes(size = pop)) +
    scale_size_area() +
    coord_quickmap()
  
  # Same map, with some world context
  ggplot(capitals, aes(long, lat)) +
    borders("world", xlim = c(-130, -60), ylim = c(20, 50)) +
    geom_point(aes(size = pop)) +
    scale_size_area() +
    coord_quickmap()
}














conc <- c(2.856829, 5.005303, 7.519473, 22.101664, 27.769976, 39.198025, 45.483269, 203.784238)
rate <- c(14.58342, 24.74123, 31.34551, 72.96985, 77.50099, 96.08794, 96.96624, 108.88374)
L.minor <- data.frame(conc, rate)
L.minor.m1 <- nls(rate ~ Vm * conc/(K + conc), data = L.minor, #采用M-M动力学方程
                  start = list(K = 20, Vm = 120), #初始值设置为K=20，Vm=120
                  trace = TRUE) #占线拟合过程
#确定x轴范围并构建数据集
min <- range(L.minor$conc)[1]
max <- range(L.minor$conc)[2]
line.data <- data.frame(conc = seq(min, max, length.out = 1000))
#用模型预测数据构建数据集
line.data$p.predict <- predict(L.minor.m1, newdata = line.data)

require(ggplot2)
M_Mfunction <- ggplot() +
  geom_point(aes(x = conc, y = rate), data = L.minor,
             alpha = 0.5, size = 5, color = "red") +
  geom_line(aes(x = conc, y = p.predict), data = line.data,
            size = 1, color = "blue") +
  scale_x_continuous(
    name = expression(Substrate ~~ concentration(mmol ~~ m^3)),#采用expression来表示数学公式
    breaks = seq(0, 200, by = 25)) +
  scale_y_continuous(
    name = "Uptake rate (weight/h)",
    breaks = seq(0, 120, by = 10)) +
  geom_text(aes(x = 100, y = 60),
            label = "bolditalic(f(list(x, (list(K, V[m])))) == frac(V[m]%.%x, K+x))",
            #注意 geom_text中如果用expression()来进行表达，必须开启parse = TRUE
            #同时以字符串""的形式表示，不能使用expression
            parse = TRUE, 
            size = 5, family = "times"
  ) +
  theme_bw() +
  theme(
    axis.title.x=element_text(size=16),
    axis.title.y=element_text(size=16),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12))
M_Mfunction





#showtext
require(showtext)
require(ggplot2)
require(Cairo)
font.add("BlackoakStd", "C://Windows//Fonts//BlackoakStd.otf")
font.add("BrushScriptStd", "C://Windows//Fonts//BrushScriptStd.otf")
font.add("times", "C://Windows//Fonts//times.ttf")
font.add("STHUPO", "C://Windows//Fonts//STHUPO.ttf")
CairoPDF("showtext_output", 8, 8)
showtext.begin()
ggplot() +
  geom_text(aes(x = 16, y = 16.25), label = "Blackoak Std", size = 8, 
            family = "BlackoakStd") +
  geom_text(aes(x = 16, y = 16), label ="Brush Script Std", size = 16,
            family = "BrushScriptStd") +
  geom_text(aes(x = 16, y = 15.75), label = "Times New Roman", size = 16,
            family = "times") +
  geom_text(aes(x = 16, y = 15.50), label = "华文琥珀", size = 16,
            family = "STHUPO") +
  ylim(c(15.25, 16.50)) +
  labs(x = "", y = "") +
  theme_bw() #在用RStudio输出
http://docs.ggplot2.org/current/index.html

