print(getwd())

setwd("D:/Papering/boreal_forest/Divers_environmental_variables/stats")

library(dismo)
library(tidyverse)
library(ggplot2)
library(caret)
library(gbm)

data <- read.csv("results_importance_1123.csv")

ggplot(data, aes(x=reorder(Variables, Importance ), y=Importance, group=Year, color=Year, fill=Year)) +
  geom_bar(stat="identity", color="black", width=.7, position=position_dodge()) +
  geom_errorbar(aes(ymin=Importance-sd, ymax=Importance+sd), width=.5,size = 0.8, position=position_dodge(0.7))+ 
  #geom_pointrange(aes(ymin=Importance-sd, ymax=Importance+sd),  size=1, shape=15)+ geom_line(size=1.4)+
  scale_fill_manual(values = c("2000_A" = "#f16c23", "2010_A" = "#2b6a99", "2020_A" = "#1B7C3D"))+
  scale_color_manual(values = c("2000_A" = "#f16c23", "2010_A" = "#2b6a99", "2020_A" = "#1B7C3D"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,60), breaks = seq(0,60,15))+
  theme_linedraw(base_size = 30)+
  theme(text = element_text(size = 18), axis.text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position=c(0.67, 0.15),legend.title=element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(0.8,'cm'),
        legend.background = element_rect(fill="transparent"))+
  labs(x=element_blank(), y = "Explanatory power (%)")+coord_flip()

ggsave("bar_error_new1125.png",device = "png",width = 150, height = 250, units = "mm", dpi = 400)




# ggplot(data, aes(x=reorder(Variables, Importance ), y=Importance, group=Year, color=Year)) +
#   #geom_bar(stat="identity", color="black", position=position_dodge()) +
#   geom_pointrange(aes(ymin=Importance-sd, ymax=Importance+sd),  size=1, shape=15)+ geom_line(size=1.4)+
#   scale_color_manual(values = c("2000_A" = "#f16c23", "2010_A" = "#2b6a99", "2020_A" = "#1B7C3D"))+
#   scale_y_continuous(expand = c(0,0), limits = c(0,60), breaks = seq(0,60,15))+
#   theme_linedraw(base_size = 30)+
#   theme(text = element_text(size = 18), axis.text = element_text(size = 15),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank())+
#   theme(legend.position=c(0.67, 0.15),legend.title=element_blank(),
#         legend.key = element_rect(colour = NA),legend.key.size=unit(1.3,'cm'),
#         legend.background = element_rect(fill="transparent"))+
#   labs(x=element_blank(), y = "Explanatory power (%)")+coord_flip()
# 
# ggsave("bar_error_new1123.png",device = "png",width = 150, height = 250, units = "mm", dpi = 400)
