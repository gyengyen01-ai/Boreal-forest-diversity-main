print(getwd())
setwd("D:/Papering/boreal_forest/Divers_environmental_variables/Trend")

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

data <-read_csv("TMP_H_slope_stats.csv")
head(data)


TMP <- ggplot(data, aes(x = Value)) +
  scale_x_continuous(expand = c(0,0), limits = c(15,38),breaks = seq(15,38,5))+
  scale_y_continuous( expand = c(0,0), limits = c(0,1.1),breaks = seq(0,1,0.2),
    name = element_blank(),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name=element_blank()))+
  coord_cartesian(ylim = c(0,1)) +
  geom_bar(aes(x = Value, y= COUNT4, fill=Value2), stat="identity",  alpha=1) +
  #scale_fill_gradientn(colours = c("#2b83ba", "#abdda4", "#ffffff", "#fdae61", "#d7191c", "#d7191c"))+
  scale_fill_gradientn(colours = c("#2b83ba", "#2b83ba","#4f9cb4", "#73b6ae", "#94cda8", "#abdda4","#ffffff", "#fdae61", "#d7191c", "#d7191c"))+
  geom_errorbar(aes(ymin = MEAN_H2 - STD_H2, ymax =  MEAN_H2 + STD_H2),width = 0.7, size=1.2, color = "#A7A7A7") +
  geom_point(aes(x = Value, y= MEAN_H2), shape=15, size = 7)+
  geom_hline(yintercept=0.4, linetype="dashed", color = "red", size=1.8)+
  geom_smooth(aes(x = Value, y= MEAN_H2), color = "blue", method = "lm", se=F, size = 1.5)+
  theme_linedraw(base_size = 50) +
  theme(text = element_text(size = 30), axis.text = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(), legend.text = element_text(size = 15),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1,'cm'),
        legend.background = element_rect(fill="transparent"))
TMP

ggsave("trend_TMP_0908.jpg",device = "png", width = 320, height = 300, units = "mm", dpi = 500)



####################   PR  ##############

data <-read_csv("PRE_H_slope_stats.csv")
head(data)

PR <- ggplot(data, aes(x = Slope)) +
  scale_x_continuous(expand = c(0,0), limits = c(-6,6.5),breaks = seq(-6,6,2))+
  scale_y_continuous( expand = c(0,0), limits = c(0,1.1),breaks = seq(0,1,0.2),
                      name = element_blank(),
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name=element_blank()))+
  coord_cartesian(ylim = c(0,1)) +
  geom_bar(aes(x = Slope, y= COUNT4, fill=Slope), stat="identity",  alpha=1) +
  scale_fill_gradientn(colours = c("#d01c8b","#d01c8b", "#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26", "#4dac26","#4dac26"))+
  geom_errorbar(aes(ymin = MEAN2 - STD2, ymax =  MEAN2 + STD2),width = 0.4, size=1.2, color = "#A7A7A7") +
  geom_point(aes(x = Slope, y= MEAN2), shape=15, size = 7)+
  geom_hline(yintercept=0.4, linetype="dashed", color = "red", size=1.8)+
  geom_smooth(aes(x = Slope, y= MEAN2),method="lm",  formula=y ~ poly(x, 2, raw=TRUE), color = "blue",  se=F, size = 1.5)+
  theme_linedraw(base_size = 50) +
  theme(text = element_text(size = 30), axis.text = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(), legend.text = element_text(size = 15), axis.title.x = element_blank(),axis.title.y = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1,'cm'),
        legend.background = element_rect(fill="transparent"))
PR

ggsave("trend_PR_0907.jpg",device = "png", width = 320, height = 300, units = "mm", dpi = 500)





####################   FIRE  ##############

data <-read_csv("Fire_H_slope_stats.csv")
head(data)

Fire <- ggplot(data, aes(x = Slope)) +
  scale_x_continuous(expand = c(0,0), limits = c(-13,19),breaks = seq(-12,19,4))+
  scale_y_continuous( expand = c(0,0), limits = c(0,1.1),breaks = seq(0,1,0.2),
                      name = element_blank(),
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name=element_blank()))+
  coord_cartesian(ylim = c(0,1)) +
  geom_bar(aes(x = Slope, y= COUNT4, fill=Slope), stat="identity",  alpha=1) +
  scale_fill_gradientn(colours = c("#0571b0","#0571b0", "#92c5de", "#f7f7f7", "#f4a582","#f4a582", "#ca0020", "#ca0020"))+
  geom_errorbar(aes(ymin = MEAN2 - STD2, ymax =  MEAN2 + STD2),width = 0.9, size=1.2, color = "#A7A7A7") +
  geom_point(aes(x = Slope, y= MEAN2), shape=15, size = 7)+
  geom_hline(yintercept=0.4, linetype="dashed", color = "red", size=1.8)+
  geom_smooth(aes(x = Slope, y= MEAN2),method="lm",  formula=y ~ poly(x, 2, raw=TRUE), color = "blue",  se=F, size = 1.5)+
  theme_linedraw(base_size = 45) +
  theme(text = element_text(size = 30), axis.text = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(), legend.text = element_text(size = 15), axis.title.x = element_blank(),axis.title.y = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1,'cm'),
        legend.background = element_rect(fill="transparent"))
Fire 
ggsave("trend_Fire_0907.jpg",device = "png", width = 320, height = 300, units = "mm", dpi = 500)





####################   AGE  ##############

data <-read_csv("age_diversity_stats.csv")
head(data)

AGE <- ggplot(data, aes(x = Value)) +
  # scale_x_continuous(expand = c(0,0), limits = c(-13,19),breaks = seq(-12,19,4))+
  scale_y_continuous( expand = c(0,0), limits = c(0,1.1),breaks = seq(0,1,0.2),
                      name = element_blank(),
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name=element_blank()))+
  coord_cartesian(ylim = c(0,1)) +
  geom_bar(aes(x = Value, y= COUNT2_c10, fill=Value), stat="identity",  alpha=1) +
  scale_fill_gradientn(colours = c("#0571b0","#0571b0", "#92c5de", "#f7f7f7", "#f4a582","#f4a582", "#ca0020", "#ca0020"))+
  geom_errorbar(aes(ymin = MEAN_c05 - STD2_c05, ymax =  MEAN_c05 + STD2_c05),width = 0.9, size=1.2, color = "#A7A7A7") +
  geom_point(aes(x = Value, y= MEAN_c05), shape=15, size = 7)+
  #geom_hline(yintercept=0.4, linetype="dashed", color = "red", size=1.8)+
  geom_smooth(aes(x = Value, y= MEAN_c05),method="lm",  formula=y ~ poly(x, 2, raw=TRUE), color = "blue",  se=F, size = 1.5)+
  theme_linedraw(base_size = 45) +
  theme(text = element_text(size = 30), axis.text = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(), legend.text = element_text(size = 15), axis.title.x = element_blank(),axis.title.y = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1,'cm'),
        legend.background = element_rect(fill="transparent"))
AGE


ggsave("trend_AGE_1104.jpg",device = "png", width = 320, height = 300, units = "mm", dpi = 500)




####################   FIRE  ##############

data <-read_csv("Fire_diversity_stats.csv")
head(data)

Fire <- ggplot(data, aes(x = Value)) +
  #scale_x_continuous(expand = c(0,0), limits = c(-13,19),breaks = seq(-12,19,4))+
  scale_y_continuous( expand = c(0,0), limits = c(0,1.1),breaks = seq(0,1,0.2),
                      name = element_blank(),
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name=element_blank()))+
  coord_cartesian(ylim = c(0,1)) +
  geom_bar(aes(x = Value, y= COUNT2, fill=Value), stat="identity",  alpha=1) +
  scale_fill_gradientn(colours = c("#0571b0","#0571b0", "#92c5de", "#f7f7f7", "#f4a582","#f4a582", "#ca0020", "#ca0020"))+
  geom_errorbar(aes(ymin = MEAN2 - STD2, ymax =  MEAN2 + STD2),width = 0.9, size=1.2, color = "#A7A7A7") +
  geom_point(aes(x = Value, y= MEAN2), shape=15, size = 7)+
  geom_hline(yintercept=0.4, linetype="dashed", color = "red", size=1.8)+
  geom_smooth(aes(x = Value, y= MEAN2),method="lm",  formula=y ~ poly(x, 2, raw=TRUE), color = "blue",  se=F, size = 1.5)+
  theme_linedraw(base_size = 45) +
  theme(text = element_text(size = 30), axis.text = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(), legend.text = element_text(size = 15), axis.title.x = element_blank(),axis.title.y = element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1,'cm'),
        legend.background = element_rect(fill="transparent"))
Fire 

ggsave("trend_Fire_1104.jpg",device = "png", width = 320, height = 300, units = "mm", dpi = 500)


