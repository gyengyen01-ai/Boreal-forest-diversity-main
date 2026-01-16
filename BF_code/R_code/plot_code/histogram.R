print(getwd())
#setwd("D:/Papering/boreal_forest/Forest_diversity_results/BRT/Estimate_results/tats_csv")

library(tidyverse)
library(ggplot2)
library(ggpp)
library(ggpmisc)
library(cowplot)
library(ggpubr)
library(gridExtra)
library(viridis)
# 
# data <-read_csv("Pixels_Stats.csv")
# 
# his1 <- ggplot(data, aes(x=Max2, y=Count, fill=Year, color= Year))+
#   # scale_fill_brewer(palette="red")+
#   scale_fill_manual(values=c('#d665a1','#bbcd1b','#45db8b'),labels=c('2000', '2010', '2020'))+scale_color_manual(values=c('#d665a1','#bbcd1b','#45db8b'),labels=c('2000', '2010', '2020'))+
#   geom_bar(stat="identity",  alpha=0.7, width=0.0089,
#            position=position_dodge())+
#   geom_line(stat = "identity", size=1.5,alpha=0.7) + 
#   geom_vline(xintercept = 0.59, size=2, color ="#45db8b")+
#   geom_vline(xintercept = 0.44, size=2, color ="#d665a1")+
#   geom_vline(xintercept = 0.51, size=2, color ="#bbcd1b")+
#   scale_y_continuous(expand = c(0,0), limits = c(0,9), breaks = seq(0,9,2))+
#   scale_x_continuous(expand = c(0,0), limits = c(0.1,1.3), breaks = seq(0.2,1.2,0.2))+
#   theme_linedraw(base_size = 60)+
#   theme(text = element_text(size = 40), axis.text = element_text(size = 40),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank())+
#   theme(legend.position=c(0.85, 0.85),legend.title=element_blank(),
#         legend.key = element_rect(colour = NA),legend.key.size=unit(1.3,'cm'),
#         legend.background = element_rect(fill="transparent"))+
#   labs(x="Shannon diversity index", y = "Proportion(%)")
# 
# his1
# 
# ggsave("histogram.jpg",device = "jpg",width = 400, height = 300, units = "mm", dpi = 300)

#################################

# data <-read_csv("Pixels_Stats_new.csv")
# 
# his1 <- ggplot(data, aes(x=Max2, y=Count, fill=Year, color= Year))+
#   # scale_fill_brewer(palette="red")+
#   scale_fill_manual(values=c('#E68F1B','#F9DD07','#4575B4'),labels=c('2000', '2010', '2020'))+scale_color_manual(values=c('#D73027','#F9DD07','#4575B4'),labels=c('2000', '2010', '2020'))+
#   geom_bar(stat="identity",  alpha=0.7, width=0.0089,
#            position=position_dodge())+
#   geom_line(stat = "identity", size=1.5,alpha=0.7) + 
#   geom_vline(xintercept = 0.409, size=2, color ="#D73027")+
#   geom_vline(xintercept = 0.431, size=2, color ="#F9DD07")+
#   geom_vline(xintercept = 0.455, size=2, color ="#4575B4")+
#   scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = seq(0,6,2))+
#   scale_x_continuous(expand = c(0,0), limits = c(0,1.35), breaks = seq(0,1.3,0.2))+
#   theme_linedraw(base_size = 60)+
#   theme(text = element_text(size = 40), axis.text = element_text(size = 40),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank())+
#   theme(legend.position=c(0.85, 0.85),legend.title=element_blank(),
#         legend.key = element_rect(colour = NA),legend.key.size=unit(1.3,'cm'),
#         legend.background = element_rect(fill="transparent"))+
#   labs(x="Shannon index", y = "Proportion(%)")
# 
# his1



data <-read_csv("Pixels_Stats_new.csv")

his1 <- ggplot(data, aes(x=Max2, y=Count, fill=Year, color= Year))+
  # scale_fill_brewer(palette="red")+
  scale_fill_manual(values=c('#f16c23','#2b6a99','#1B7C3D'),labels=c('2000', '2010', '2020'))+scale_color_manual(values=c('#f16c23','#2b6a99','#1B7C3D'),labels=c('2000', '2010', '2020'))+
  geom_bar(stat="identity",  alpha=0.7, width=0.0089,
           position=position_dodge())+
  geom_line(stat = "identity", size=1.5,alpha=0.7) + 
  geom_vline(xintercept = 0.409, size=2, color ="#f16c23")+
  geom_vline(xintercept = 0.431, size=2, color ="#2b6a99")+
  geom_vline(xintercept = 0.455, size=2, color ="#1B7C3D")+
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = seq(0,6,2))+
  scale_x_continuous(expand = c(0,0), limits = c(0,1.35), breaks = seq(0,1.3,0.2))+
  theme_linedraw(base_size = 60)+
  theme(text = element_text(size = 40), axis.text = element_text(size = 40),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position=c(0.85, 0.85),legend.title=element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1.3,'cm'),
        legend.background = element_rect(fill="transparent"))+
  labs(x="Shannon index", y = "Proportion(%)")

his1

ggsave("histogram0705.jpg",device = "jpg",width = 400, height = 300, units = "mm", dpi = 300)


data <-read_csv("Increased_value1127.csv")
his2 <- ggplot(data, aes(x=Values1127, y=Count,fill=type))+
  # scale_fill_brewer(palette="red")+
  scale_fill_manual(values=c("D"='#6DADD1',"I" = '#F6B293',"N"= '#E9F1F4'))+
  #scale_fill_manual(values=c('#D4D4D4'))+
  geom_bar(stat="identity", color="#555555",  width=0.0145,alpha=0.8,
           position=position_dodge())+
  #geom_vline(xintercept = 0.03, size=2, color ="red")+
  geom_vline(xintercept = 0, size=1.5, color ="black",linetype = "dashed")+
  scale_y_continuous(expand = c(0,0), limits = c(0,5.2), breaks = seq(0,4,1))+
  scale_x_continuous(expand = c(0,0), limits = c(-0.40,0.55), breaks = seq(-0.4,0.4,0.2))+
  theme_linedraw(base_size = 60)+
  theme(text = element_text(size = 40), axis.text = element_text(size = 40),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.position="none",legend.title=element_blank(),
        legend.key = element_rect(colour = NA),legend.key.size=unit(1.3,'cm'),
        legend.background = element_rect(fill="transparent"))+
  labs(x="Change in Shannon Index during 2000-2020", y = "Proportion(%)")

his2

ggsave("Incread_value1127.jpg",device = "jpg",width = 400, height = 300, units = "mm", dpi = 300)




plot_leaf <- ggarrange(his1, his2, ncol = 2, nrow = 1,labels = c("e", "f"),font.label = list(size = 50))
plot_leaf 
ggsave("plot_1127.jpg",device = "jpg",width = 800, height = 300, units = "mm", dpi = 500)
