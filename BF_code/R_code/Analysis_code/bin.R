print(getwd())
setwd("D:/Paper/Boreal_forest/Divers_environmental_variables/Trend")

library(tidyverse)
library(ggplot2)
library(ggpp)
library(ggpmisc)
library(cowplot)
library(ggpubr)
library(gridExtra)
library(viridis)
library(grid)

####################   boxplot  ##############

data <-read_csv("Point_3.csv")
head(data)

f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

ggplot(data) +
  #geom_boxplot(aes(x = as.factor(TMP_re), y = H_slope, fill = as.factor(TMP_re)),outlier.shape = NA) +
  stat_summary(aes(x = as.factor(TMP_re), y = H_slope, fill = TMP_me_re2,width=0.8),fun.data = f, geom="boxplot", size=1)+
  scale_fill_gradient(low=hcl(195,100,75), high=hcl(15,100,75))+
  #scale_fill_gradient(low="#4DAC26",  high="#B7198B")+
  scale_y_continuous(expand = c(0,0), limits = c(-0.08,0.16),breaks = seq(-0.05,0.15,0.05))+
  labs(x=element_blank(), y = "Shannon diveristy trend") +
  theme_linedraw(base_size = 65) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position=c(0.8,0.85),legend.title=element_blank(), legend.text = element_text(size = 25),axis.text.x = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1.2,'cm'),
        legend.background = element_rect(fill="transparent"))

ggsave("trend_TMP.jpg",device = "png", width = 400, height = 400, units = "mm", dpi = 500)





####################   point with errorbar  ##############


data <-read_csv("Trend_tmp.csv")
head(data)

my.formula <- y ~ poly(x, 3)
#my.formula <- y ~ x

Trend <- ggplot(data,aes(x = as.factor(Value),  color = MEAN_TMP)) +
  geom_errorbar(aes(ymin = MEAN_H - STD_H, ymax =  MEAN_H + STD_H),width = 0, size=2, color = "#A7A7A7") +
  geom_point(aes(x = as.factor(Value), y = MEAN_H), size = 10)+
  scale_color_gradient(low=hcl(195,100,75), high=hcl(15,100,75))+
  scale_y_continuous(expand = c(0,0), limits = c(-0.08,0.16),breaks = seq(-0.05,0.15,0.05))+
  labs(x=element_blank(), y = "Shannon diveristy trend") +
  theme_linedraw(base_size = 65) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position=c(0.8,0.85),legend.title=element_blank(), legend.text = element_text(size = 25),axis.text.x = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1.2,'cm'),
        legend.background = element_rect(fill="transparent"))
Trend


ggsave("trend_TMP_point.jpg",device = "png", width = 400, height = 400, units = "mm", dpi = 500)

