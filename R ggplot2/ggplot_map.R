library(maps)
library(ggplot2)

USA.POP <- ggplot(us.cities, aes(x = long, y = lat)) + 
  xlim(-130, -65) +
  borders("state", size=0.5) +
  geom_point(aes(size = log(pop), color = factor(capital), alpha = 1/50)) +
  scale_size(range=c(0, 7), name = "log(City population)") +
  scale_color_manual(values = c("black", "red"), labels = c("state capital", "city")) +
  guides(color = guide_legend(title=NULL)) + 
  scale_alpha(guide = FALSE) +
  labs(x = "longtitude", y = "latitude", title = "City Population in the United States") +
  theme(plot.title = element_text(size=20))

USA.POP
#输出图像 并用cairo包进行抗锯齿处理
ggsave(USA.POP, file = "USA_POP.png", type = "cairo", width = 10, height = 6.75)
