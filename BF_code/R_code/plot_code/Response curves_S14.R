print(getwd())
setwd("D:/Paper/Boreal_forest/biomass/recalss_for_supplementary/stats")

library(tidyverse)
library(ggplot2)
library(ggpp)
library(ggpmisc)
library(cowplot)
library(ggpubr)
library(gridExtra)
library(viridis)
library(grid)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(Hmisc)
library(corrplot)
library(CARBayes)  
library(lme4)  


## Load data
data <- read.csv("Trend_all.csv", header=TRUE)     ## load csv file
head(data)

#################################################################################################################################     NPP_S

model <- glm(NPP_S ~  H_mean + H_slope +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean, data = data)
summary(model)
# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("H_slope", "H_mean", "TMP_slope", "Pre_slope", "TMP_mean", "PRE_mean", "FIRE_slope", "POP_slope", "FIRE_mean", "POP_mean")])
cor(predicted_data)
head(predicted_data)

# 计算Slope   #
column_of_interest <- "predictions"

# 初始化一个空列表来存储结果
slope_results <- list()

# 循环计算指定列与其他列的斜率，并存储结果
for (col in colnames(predicted_data)) {
  if (col != column_of_interest) {
    model <- lm(predicted_data[[column_of_interest]] ~ predicted_data[[col]])
    slope <- coefficients(model)[2]  # 提取斜率
    slope_results[[col]] <- slope
  }
}

result_df <- data.frame(Column = names(slope_results), Slope = unlist(slope_results))
print(result_df)
# 计算Slope 结束 #



H_slope <- ggplot(predicted_data, aes(x = H_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(-0.1, 0.2) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_slope

H_mean <- ggplot(predicted_data, aes(x = H_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0.1, 0.9) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_mean

TMP_mean <- ggplot(predicted_data, aes(x = TMP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(3, 17) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_mean


PRE_mean <- ggplot(predicted_data, aes(x = PRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(1, 4) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
PRE_mean


TMP_slope <- ggplot(predicted_data, aes(x = TMP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0, 0.1) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_slope

Pre_slope <- ggplot(predicted_data, aes(x = Pre_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(-5, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Pre_slope

plot_NPP_S <- plot_grid(H_slope, H_mean, TMP_mean, PRE_mean,TMP_slope, Pre_slope, 
                            align="hv", label_size = 20, ncol = 6, nrow = 1, label_fontface = "plain")
plot_NPP_S


#################################################################################################################################     kNDVI


model <- glm(kNDVI_S2 ~  H_mean + H_slope +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("H_slope", "H_mean", "TMP_slope", "Pre_slope", "TMP_mean", "PRE_mean", "FIRE_slope", "POP_slope", "FIRE_mean", "POP_mean")])
cor(predicted_data)
head(predicted_data)

# 计算Slope   #
column_of_interest <- "predictions"

# 初始化一个空列表来存储结果
slope_results <- list()

# 循环计算指定列与其他列的斜率，并存储结果
for (col in colnames(predicted_data)) {
  if (col != column_of_interest) {
    model <- lm(predicted_data[[column_of_interest]] ~ predicted_data[[col]])
    slope <- coefficients(model)[2]  # 提取斜率
    slope_results[[col]] <- slope
  }
}

result_df <- data.frame(Column = names(slope_results), Slope = unlist(slope_results))
print(result_df)
# 计算Slope 结束 #

H_slope <- ggplot(predicted_data, aes(x = H_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(-0.1, 0.2) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_slope

H_mean <- ggplot(predicted_data, aes(x = H_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0.1, 0.9) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_mean

TMP_mean <- ggplot(predicted_data, aes(x = TMP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(3, 17) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_mean


PRE_mean <- ggplot(predicted_data, aes(x = PRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(1, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
PRE_mean


TMP_slope <- ggplot(predicted_data, aes(x = TMP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0, 0.1) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_slope

Pre_slope <- ggplot(predicted_data, aes(x = Pre_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(-5, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Pre_slope

plot_kNDVI <- plot_grid(H_slope, H_mean, TMP_mean, PRE_mean,TMP_slope, Pre_slope, 
                        align="hv", label_size = 20, ncol = 6, nrow = 1, label_fontface = "plain")
plot_kNDVI




#################################################################################################################################     VOD_Ku_S

model <- glm(VOD_Ku_S ~  H_mean + H_slope +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("H_slope", "H_mean", "TMP_slope", "Pre_slope", "TMP_mean", "PRE_mean", "FIRE_slope", "POP_slope", "FIRE_mean", "POP_mean")])
cor(predicted_data)
head(predicted_data)

# 计算Slope   #
column_of_interest <- "predictions"

# 初始化一个空列表来存储结果
slope_results <- list()

# 循环计算指定列与其他列的斜率，并存储结果
for (col in colnames(predicted_data)) {
  if (col != column_of_interest) {
    model <- lm(predicted_data[[column_of_interest]] ~ predicted_data[[col]])
    slope <- coefficients(model)[2]  # 提取斜率
    slope_results[[col]] <- slope
  }
}

result_df <- data.frame(Column = names(slope_results), Slope = unlist(slope_results))
print(result_df)
# 计算Slope 结束 #


H_slope <- ggplot(predicted_data, aes(x = H_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(-0.1, 0.2) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_slope

H_mean <- ggplot(predicted_data, aes(x = H_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0.1, 0.9) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_mean

TMP_mean <- ggplot(predicted_data, aes(x = TMP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(3, 17) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_mean


PRE_mean <- ggplot(predicted_data, aes(x = PRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(1, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
PRE_mean


TMP_slope <- ggplot(predicted_data, aes(x = TMP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0, 0.1) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_slope

Pre_slope <- ggplot(predicted_data, aes(x = Pre_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(-5, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Pre_slope

plot_VOD_Ku_S <- plot_grid(H_slope, H_mean, TMP_mean, PRE_mean,TMP_slope, Pre_slope, 
                        align="hv", label_size = 20, ncol = 6, nrow = 1, label_fontface = "plain")
plot_VOD_Ku_S


#################################################################################################################################     Carbon_S

model <- glm(Carbon_S ~  H_mean + H_slope +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("H_slope", "H_mean", "TMP_slope", "Pre_slope", "TMP_mean", "PRE_mean", "FIRE_slope", "POP_slope", "FIRE_mean", "POP_mean")])
cor(predicted_data)
head(predicted_data)


# 计算Slope   #
column_of_interest <- "predictions"

# 初始化一个空列表来存储结果
slope_results <- list()

# 循环计算指定列与其他列的斜率，并存储结果
for (col in colnames(predicted_data)) {
  if (col != column_of_interest) {
    model <- lm(predicted_data[[column_of_interest]] ~ predicted_data[[col]])
    slope <- coefficients(model)[2]  # 提取斜率
    slope_results[[col]] <- slope
  }
}

result_df <- data.frame(Column = names(slope_results), Slope = unlist(slope_results))
print(result_df)
# 计算Slope 结束 #


H_slope <- ggplot(predicted_data, aes(x = H_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(-0.1, 0.2) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_slope

H_mean <- ggplot(predicted_data, aes(x = H_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0.1, 0.9) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_mean

TMP_mean <- ggplot(predicted_data, aes(x = TMP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(3, 17) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_mean


PRE_mean <- ggplot(predicted_data, aes(x = PRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(1, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
PRE_mean


TMP_slope <- ggplot(predicted_data, aes(x = TMP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= F, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0, 0.01) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_slope

Pre_slope <- ggplot(predicted_data, aes(x = Pre_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(-5, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Pre_slope

plot_Carbon_S <- plot_grid(H_slope, H_mean, TMP_mean, PRE_mean,TMP_slope, Pre_slope, 
                           align="hv", label_size = 20, ncol = 6, nrow = 1, label_fontface = "plain")
plot_Carbon_S


#################################################################################################################################     Biomass_

model <- glm(Biomass_ ~  H_mean + H_slope +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("H_slope", "H_mean", "TMP_slope", "Pre_slope", "TMP_mean", "PRE_mean", "FIRE_slope", "POP_slope", "FIRE_mean", "POP_mean")])
cor(predicted_data)
head(predicted_data)

# 计算Slope   #
column_of_interest <- "predictions"

# 初始化一个空列表来存储结果
slope_results <- list()

# 循环计算指定列与其他列的斜率，并存储结果
for (col in colnames(predicted_data)) {
  if (col != column_of_interest) {
    model <- lm(predicted_data[[column_of_interest]] ~ predicted_data[[col]])
    slope <- coefficients(model)[2]  # 提取斜率
    slope_results[[col]] <- slope
  }
}

result_df <- data.frame(Column = names(slope_results), Slope = unlist(slope_results))
print(result_df)
# 计算Slope 结束 #


H_slope <- ggplot(predicted_data, aes(x = H_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(-0.1, 0.2) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_slope

H_mean <- ggplot(predicted_data, aes(x = H_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0.1, 0.9) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_mean

TMP_mean <- ggplot(predicted_data, aes(x = TMP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(3, 17) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_mean


PRE_mean <- ggplot(predicted_data, aes(x = PRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(1, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
PRE_mean


TMP_slope <- ggplot(predicted_data, aes(x = TMP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0, 0.1) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_slope

Pre_slope <- ggplot(predicted_data, aes(x = Pre_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(-5, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Pre_slope

plot_Biomass_S <- plot_grid(H_slope, H_mean, TMP_mean, PRE_mean,TMP_slope, Pre_slope, 
                           align="hv", label_size = 20, ncol = 6, nrow = 1, label_fontface = "plain")
plot_Biomass_S


#################################################################################################################################     Stabi_S

model <- glm(Stabi_S ~  H_mean + H_slope +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("H_slope", "H_mean", "TMP_slope", "Pre_slope", "TMP_mean", "PRE_mean", "FIRE_slope", "POP_slope", "FIRE_mean", "POP_mean")])
cor(predicted_data)
head(predicted_data)

# 计算Slope   #
column_of_interest <- "predictions"

# 初始化一个空列表来存储结果
slope_results <- list()

# 循环计算指定列与其他列的斜率，并存储结果
for (col in colnames(predicted_data)) {
  if (col != column_of_interest) {
    model <- lm(predicted_data[[column_of_interest]] ~ predicted_data[[col]])
    slope <- coefficients(model)[2]  # 提取斜率
    slope_results[[col]] <- slope
  }
}

result_df <- data.frame(Column = names(slope_results), Slope = unlist(slope_results))
print(result_df)
# 计算Slope 结束 #


H_slope <- ggplot(predicted_data, aes(x = H_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(-0.1, 0.2) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_slope

H_mean <- ggplot(predicted_data, aes(x = H_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0.1, 0.9) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
H_mean

TMP_mean <- ggplot(predicted_data, aes(x = TMP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(3, 17) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_mean


PRE_mean <- ggplot(predicted_data, aes(x = PRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(1, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
PRE_mean


TMP_slope <- ggplot(predicted_data, aes(x = TMP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0, 0.1) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
TMP_slope

Pre_slope <- ggplot(predicted_data, aes(x = Pre_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(-5, 5) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Pre_slope

plot_Stabi_S <- plot_grid(H_slope, H_mean, TMP_mean, PRE_mean,TMP_slope, Pre_slope, 
                            align="hv", label_size = 20, ncol = 6, nrow = 1, label_fontface = "plain")
plot_Stabi_S






plot_H_climate <- plot_grid(plot_NPP_S, plot_kNDVI,  plot_VOD_Ku_S, plot_Carbon_S, plot_Biomass_S, plot_Stabi_S, 
                            align="hv",  ncol = 1, nrow = 6, label_fontface = "plain")
plot_H_climate


ggsave("plot_trend2.jpg",device = "jpg",width = 800, height = 800, units = "mm", dpi = 600)




































# NPPs_S_SAND <- ggplot(data) +
#   #geom_point(aes(x = POP_mean, y = NPP_S), size = 6)+
#   geom_smooth(aes(x = S_SAND, y = NPP_S), method = "lm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
#   scale_y_continuous(expand = c(0,0), limits = c(-5,5), breaks = seq(-4,4,2))+
#   #scale_x_continuous(expand = c(0,0), limits = c(0,1), breaks = seq(0,1,0.2))+
#   labs(x=element_blank(), y = element_blank()) +
#   theme_linedraw(base_size = 65) +
#   theme(text = element_text(size = 30), axis.text = element_text(size = 30),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank())+
#   theme(legend.position=c(0.8,0.85),legend.title=element_blank(), legend.text = element_text(size = 25),axis.text.x = element_blank(),#axis.text.y = element_blank(),
#         legend.key = element_rect(colour = NA),legend.key.size=unit(1.2,'cm'),
#         legend.background = element_rect(fill="transparent"))
# NPPs_S_SAND
# 
# 
# str(data)
# # Impute missing values with column means
# data[is.na(data)] <- colMeans(data, na.rm = TRUE)
# 
# 
# 
# #data <-read_csv("total_2000.csv")
# res2 <- rcorr(as.matrix(data))
# res2
# 
# res <- cor(data)
# round(res, 2)
# 
# testRes = cor.mtest(data, conf.level = 0.95)
# 
# # corrplot(res, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
# #          addCoef.col = "black", tl.col = "black", tl.srt = 45,
# #          sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
# #          insig = 'label_sig', pch.col = 'grey20', order = 'AOE')
# 
# 
# corrplot(res, type = "upper", order = "hclust", addCoef.col = "black",diag = FALSE,number.cex = 1.5, tl.cex = 1.3, cl.cex = 1.5,
#          p.mat = testRes$p, sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.1,insig = 'label_sig',
#          tl.col = "black", tl.srt = 45)

