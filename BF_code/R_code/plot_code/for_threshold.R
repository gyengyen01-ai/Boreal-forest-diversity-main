print(getwd())

setwd("D:/Papering/boreal_forest/Forest_diversity_results/total/for_slope_std")


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


data <-read_csv("for_threshold.csv")

# hist(data$StdDev, breaks=50)
# hist(data$Slope, breaks=50)


his1 <- ggplot(data = data, aes(x = data$StdDev2)) +
  geom_histogram(bins = 30, color = "white", fill = "#36ACA2")+
  geom_density(aes(y=0.01*..count..), colour="black", adjust=1, linewidth = 1)+ 
  geom_vline(aes(xintercept = 0.25), color = "red", linewidth = 1.5)+
  scale_y_continuous(expand = c(0,0), limits = c(0,95), breaks = seq(0,90,20))+
  scale_x_continuous(expand = c(0,0), limits = c(0,0.33), breaks = seq(0,0.3,0.05))+
  labs(x="Standard deviation of H’ index", y = "Count")+
  theme_linedraw(base_size = 60) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
his1


his2 <- ggplot(data = data, aes(x = data$Slope3)) +
  geom_histogram(bins = 35, color = "white", fill = "#4c8bc0")+
  geom_density(aes(y=0.045*..count..), colour="black", adjust=2, linewidth = 1)+ 
  geom_vline(aes(xintercept = -0.02), color = "red", linewidth = 1.5)+
  geom_vline(aes(xintercept = 0.02), color = "red", linewidth = 1.5)+
  scale_y_continuous(expand = c(0,0), limits = c(0,95), breaks = seq(0,90,20))+
  scale_x_continuous(expand = c(0,0), limits = c(-1,1.2), breaks = seq(-0.8,1.0,0.4))+
  labs(x="Changes in H’ index", y = "Count")+
  theme_linedraw(base_size = 60) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

his2 

plot_threshold <- ggarrange(his1, his2, ncol = 2, nrow = 1,labels = c("a", "b"),font.label = list(size = 40))
plot_threshold
ggsave("plot_threshold1127.jpg",device = "jpg",width = 600, height = 300, units = "mm", dpi = 400)

