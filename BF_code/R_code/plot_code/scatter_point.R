print(getwd())
setwd("D:/Paper/Boreal_forest/Results")


# 导入所需的包
library(MASS)
library(ggplot2)
library(viridis)

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
# 从CSV文件中读取数据
data <- read.csv("scatter_plot_DL2.csv")  # 替换为你的CSV文件路径
#cor(data)
data$density <- get_density(data$Ture, data$Prediction, n = 200)
my.formula <- y ~ poly(x, 1)

# 使用ggplot2绘制散点密度图
ggplot(data, aes(x =Ture, y = Prediction, color = density)) + scale_color_viridis(option = "H")+ 
  geom_point(size=6, shape=18,alpha = 1)+
  #geom_smooth( method = "lm", formula = my.formula, color="red", se= TRUE, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 2) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.8), breaks = seq(0,1.8,0.5))+
  scale_x_continuous(expand = c(0,0), limits = c(0,1.8), breaks = seq(0,1.8,0.5))+
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position=c(0.85, 0.15),legend.title=element_blank(),legend.key.height = unit(1, 'cm'),
        legend.key = element_rect(colour = NA),legend.key.size=unit(0.8,'cm'),
        legend.background = element_rect(fill="transparent"))+
  labs(x="Reference H’ values", y = "Predicted H’ values")+ coord_fixed()


ggsave("scatter_plot_DL.jpg",device = "jpg",width = 320, height = 320, units = "mm", dpi = 300)

library(Metrics)
library(Hmisc)

rmse(data$Prediction,data$Ture)
cor(data$Prediction,data$Ture)
mae(data$Prediction,data$Ture)
t.test(data$Prediction,data$Ture)





