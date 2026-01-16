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


####################   TMP  ##############

data <-read_csv("Point_3.csv")
head(data)

mod <- lm(data$H_slope ~ data$TMP_re)
cf <- coef(mod)
Slope <- cf[2]
Slope




f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

ggplot(data) +
  #geom_boxplot(aes(x = as.factor(TMP_re), y = H_slope, fill = as.factor(TMP_re)),outlier.shape = NA) +
  stat_summary(aes(x = as.factor(TMP_re), y = H_slope, fill = TMP_re,width=0.8),fun.data = f, geom="boxplot", size=1)+
  scale_fill_gradientn(colours = c("#2b83ba", "#abdda4", "#ffffff", "#fdae61", "#d7191c"))+
  #scale_fill_gradient(low="#4DAC26",  high="#B7198B")+
  scale_y_continuous(expand = c(0,0), limits = c(-0.08,0.16),breaks = seq(-0.05,0.14,0.05))+
  labs(x=element_blank(), y = element_blank()) +
  
  theme_linedraw(base_size = 45) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(), legend.text = element_text(size = 15),axis.text.x = element_blank(),axis.text.y = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1,'cm'),
        legend.background = element_rect(fill="transparent"))

ggsave("trend_TMP2.jpg",device = "png", width = 320, height = 300, units = "mm", dpi = 500)


####################   PRE  ##############

data <-read_csv("Point_0817.csv")
head(data)

mod <- lm(data$H_slope ~ data$Pre_new)
cf <- coef(mod)
Slope <- cf[2]
Slope


f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

ggplot(data) +
  #geom_boxplot(aes(x = as.factor(TMP_re), y = H_slope, fill = as.factor(TMP_re)),outlier.shape = NA) +
  stat_summary(aes(x = as.factor(Pre_re), y = H_slope, fill =  Pre_re, width=0.8),fun.data = f, geom="boxplot", size=1)+
  scale_fill_gradientn(colours = c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26"))+
  #scale_fill_gradient(low="#4DAC26",  high="#B7198B")+
  scale_y_continuous(expand = c(0,0), limits = c(-0.07,0.13),breaks = seq(-0.05,0.15,0.05))+
  labs(x=element_blank(), y = element_blank()) +
  theme_linedraw(base_size = 45) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(), legend.text = element_text(size = 15),axis.text.x = element_blank(),axis.text.y = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1,'cm'),
        legend.background = element_rect(fill="transparent"))

ggsave("trend_PRE2.jpg",device = "png", width = 320, height = 300, units = "mm", dpi = 500)


####################   FIRE  ##############

data <-read_csv("Point_0817_fire2.csv")
head(data)

mod <- lm(data$H_slope ~ data$Fire_re)
cf <- coef(mod)
Slope <- cf[2]
Slope


f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

ggplot(data) +
  #geom_boxplot(aes(x = as.factor(TMP_re), y = H_slope, fill = as.factor(TMP_re)),outlier.shape = NA) +
  stat_summary(aes(x = as.factor(Fire_re), y = H_slope, fill =  Fire_re, width=0.8),fun.data = f, geom="boxplot", size=1)+
  scale_fill_gradientn(colours = c("#0571b0", "#92c5de", "#f7f7f7", "#f4a582", "#ca0020"))+
  #scale_fill_gradient(low="#4DAC26",  high="#B7198B")+
  scale_y_continuous(expand = c(0,0), limits = c(-0.06,0.15),breaks = seq(-0.05,0.14,0.05))+
  labs(x=element_blank(), y = element_blank()) +
  theme_linedraw(base_size = 45) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(), legend.text = element_text(size = 15),axis.text.x = element_blank(),axis.text.y = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1,'cm'),
        legend.background = element_rect(fill="transparent"))

ggsave("trend_Fire22.jpg",device = "png", width = 320, height = 300, units = "mm", dpi = 500)




