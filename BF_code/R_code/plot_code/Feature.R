print(getwd())
setwd("D:/Paper/Boreal_forest/Methods")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(readr)
library(MASS)
library(ggpp)
library(ggpmisc)
library(cowplot)
library(ggpubr)
library(gridExtra)
library(viridis)

theme_set(theme_bw(base_size=16))

data1 <-read_csv("Feature_important_band_month.csv")

data2 <-read_csv("Feature_important_HM.csv")


feature_month <- ggplot(data1, aes(x=reorder(Features_month, Important_month), y=Important_month)) +
  geom_bar(position="dodge",stat="identity",width = 0.45, fill = "dodgerblue3") +
  geom_errorbar(aes(ymin=Important_month - sd_month, ymax=Important_month + sd_month), width=.2, size=1, position=position_dodge(.9)) +
  labs(x=element_blank(), y = element_blank()) +theme_linedraw(base_size = 60)+
  scale_y_continuous(expand = c(0,0), limits = c(0,18), breaks = seq(0,17,4))+
  theme(legend.position = c(0.75, 0.12), legend.key.size=unit(1.6,'cm'), legend.title = element_blank()) + coord_flip() +
  theme(text = element_text(size = 30), axis.text.x = element_blank(),
        axis.text.y = element_text(size = 30),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
feature_month
ggsave("feature_month2.jpg",device = "jpg",width = 220, height = 300, units = "mm", dpi = 300)


feature <-ggplot(data1,aes(x=reorder(Features, Important), y=Important)) +
  geom_bar(position="dodge",stat="identity",width = 0.45, fill = "#c24553") +
  geom_errorbar(aes(ymin=Important - sd, ymax=Important + sd), width=.2, size=1, position=position_dodge(.9)) +
  labs(x=element_blank(), y = element_blank()) +theme_linedraw(base_size = 60)+
  scale_y_continuous(expand = c(0,0), limits = c(0,18), breaks = seq(0,17,4))+
  theme(legend.position = c(0.75, 0.12), legend.key.size=unit(1.6,'cm'), legend.title = element_blank()) + coord_flip() +
  theme(text = element_text(size = 30), axis.text.x = element_blank(),
        axis.text.y = element_text(size = 30),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
feature
ggsave("feature2.jpg",device = "jpg",width = 220, height = 300, units = "mm", dpi = 300)

Features_HM <- ggplot(data2, aes(x=reorder(Features_HM, Important_HM), y=Important_HM)) +
  geom_bar(position="dodge",stat="identity",width = 0.45, fill = "#faaf42") +
  geom_errorbar(aes(ymin=Important_HM - HM_sd, ymax=Important_HM + HM_sd), width=.2, size=1, position=position_dodge(.9)) +
  labs(x=element_blank(), y = element_blank()) +theme_linedraw(base_size = 60)+
  scale_y_continuous(expand = c(0,0), limits = c(0,18), breaks = seq(0,17,4))+
  theme(legend.position = c(0.75, 0.12), legend.key.size=unit(1.6,'cm'), legend.title = element_blank()) + coord_flip() +
  theme(text = element_text(size = 30), axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
Features_HM
ggsave("Features_HM2.jpg",device = "jpg",width = 220, height = 300, units = "mm", dpi = 300)


Features_STMs <- ggplot(data2, aes(x=reorder(Features_STMs, Important_STMs), y=Important_STMs)) +
  geom_bar(position="dodge",stat="identity",width = 0.45, fill = "#0FA14A") +
  geom_errorbar(aes(ymin=Important_STMs - STMs_sd, ymax=Important_STMs + STMs_sd), width=.2, size=1, position=position_dodge(.9)) +
  labs(x=element_blank(), y = element_blank()) +theme_linedraw(base_size = 60)+
  scale_y_continuous(expand = c(0,0), limits = c(0,18), breaks = seq(0,17,4))+
  theme(legend.position = c(0.75, 0.12), legend.key.size=unit(1.6,'cm'), legend.title = element_blank()) + coord_flip() +
  theme(text = element_text(size = 30), axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
Features_STMs
ggsave("Features_STMs2.jpg",device = "jpg",width = 220, height = 300, units = "mm", dpi = 300)




plot_threshold <- ggarrange(feature_month, feature, Features_HM, Features_STMs, ncol = 2, nrow = 2,labels = c("a", "b", "c", "d"),font.label = list(size = 20))
plot_threshold


ggsave("climate_importance_1130.jpg",device = "jpg",width = 420, height = 290, units = "mm", dpi = 300)
