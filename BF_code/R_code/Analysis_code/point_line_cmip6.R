print(getwd())

setwd("D:/Paper/Boreal_forest/Divers_environmental_variables/temperature/model2")

library(tidyverse)
library(ggplot2)
library(ggpp)
library(ggpmisc)
library(cowplot)
library(ggpubr)
library(gridExtra)
library(viridis)
library(grid)
data <-read_csv("model_plot.csv")
#load formula
my.formula <- y ~ poly(x, 3)

df <-data.frame(data=data, x="year",
                y1="ACCESS_CM2",
                y2="ACCESS_ESM1_5",
                y3="AWI_CM_1_1_MR",
                y4="BCC_CSM2_MR",
                y5="Boreal",
                y6="Polar")

ACCESS_CM2 <- ggplot(data=data, mapping = aes(x = year, y= ACCESS_CM2)) +
  scale_x_continuous(expand = c(0,0), limits = c(1958,2102),breaks = seq(1960,2100,30))+
  # scale_y_continuous(expand = c(0,0), limits = c(1995,2025), breaks = seq(1998,2024,4))+
  geom_line(color= "#EC5621",size = 1.3) +
  geom_point(color= "#EC3521",size = 2) +
  geom_smooth(aes(x = year, y= ACCESS_CM2), method = "lm", formula = my.formula, color="black", se= TRUE,linetype = "dashed", size = 1.5) +
  #labs(x=element_blank(), y = "Temperature (°C)")
  labs(x=element_blank(), y = element_blank())+
  theme_linedraw(base_size = 60) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank())
ACCESS_CM2  

ACCESS_ESM1_5 <- ggplot(data=data, mapping = aes(x = year, y= ACCESS_ESM1_5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1958,2102),breaks = seq(1960,2100,30))+
  # scale_y_continuous(expand = c(0,0), limits = c(1995,2025), breaks = seq(1998,2024,4))+
  geom_line(color= "#EC5621",size = 1.3) +
  geom_point(color= "#EC3521",size = 2) +
  geom_smooth(aes(x = year, y= ACCESS_ESM1_5), method = "lm", formula = my.formula, color="black", se= TRUE,linetype = "dashed", size = 1.5) +
  #labs(x=element_blank(), y = "Temperature (°C)")
  labs(x=element_blank(), y = element_blank())+
  theme_linedraw(base_size = 60) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank())
ACCESS_ESM1_5 

AWI_CM_1_1_MR <- ggplot(data=data, mapping = aes(x = year, y= AWI_CM_1_1_MR)) +
  scale_x_continuous(expand = c(0,0), limits = c(1958,2102),breaks = seq(1960,2100,30))+
  # scale_y_continuous(expand = c(0,0), limits = c(1995,2025), breaks = seq(1998,2024,4))+
  geom_line(color= "#EC5621",size = 1.3) +
  geom_point(color= "#EC3521",size = 2) +
  geom_smooth(aes(x = year, y= AWI_CM_1_1_MR), method = "lm", formula = my.formula, color="black", se= TRUE,linetype = "dashed", size = 1.5) +
  #labs(x=element_blank(), y = "Temperature (°C)")
  labs(x=element_blank(), y = element_blank())+
  theme_linedraw(base_size = 60) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank())
AWI_CM_1_1_MR

BCC_CSM2_MR <- ggplot(data=data, mapping = aes(x = year, y= BCC_CSM2_MR)) +
  scale_x_continuous(expand = c(0,0), limits = c(1958,2102),breaks = seq(1960,2100,30))+
  # scale_y_continuous(expand = c(0,0), limits = c(1995,2025), breaks = seq(1998,2024,4))+
  geom_line(color= "#EC5621",size = 1.3) +
  geom_point(color= "#EC3521",size = 2) +
  geom_smooth(aes(x = year, y= BCC_CSM2_MR), method = "lm", formula = my.formula, color="black", se= TRUE,linetype = "dashed", size = 1.5) +
  #labs(x=element_blank(), y = "Temperature (°C)")
  labs(x=element_blank(), y = element_blank())+
  theme_linedraw(base_size = 60) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank())
BCC_CSM2_MR



temperature_model <- plot_grid(ACCESS_CM2, ACCESS_ESM1_5, AWI_CM_1_1_MR, BCC_CSM2_MR,labels = c("a", "b", "c", "d"), label_size = 35, ncol = 2, nrow = 2, label_fontface = "plain")
temperature_model
ggsave("temperature_model.jpg",device = "jpg",width = 800, height = 500, units = "mm", dpi = 300)
