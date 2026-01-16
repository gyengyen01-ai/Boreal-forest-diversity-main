print(getwd())
setwd("D:/Papering/boreal_forest/biomass/recalss_for_supplementary/stats")

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
#library(CARBayes)  
library(lme4)  


## Load data
data <- read.csv("Trend_point_biomass_11262.csv", header=TRUE)     ## load csv file
head(data)

#################################################################################################################################     NPP_S

model <- glm(NPP_S ~  H_mean + H_D +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean + Age + DEM + S_CEC + S_OC + S_SAND, data = data)
summary(model)
# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("FIRE_mean", "POP_mean", "FIRE_slope", "POP_slope", "Age", "DEM", "S_CEC", "S_OC", "S_SAND")])
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



FIRE_mean <- ggplot(predicted_data, aes(x = FIRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0, 10000000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_mean

POP_mean <- ggplot(predicted_data, aes(x = POP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0, 100) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_mean

FIRE_slope <- ggplot(predicted_data, aes(x = FIRE_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(-250000, 250000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_slope


POP_slope <- ggplot(predicted_data, aes(x = POP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(-10, 10) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_slope


Age <- ggplot(predicted_data, aes(x = Age, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0, 200) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Age


DEM <- ggplot(predicted_data, aes(x = DEM, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0, 2000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
DEM



S_CEC <- ggplot(predicted_data, aes(x = S_CEC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0, 150) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_CEC



S_OC <- ggplot(predicted_data, aes(x = S_OC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0, 40) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_OC


S_SAND <- ggplot(predicted_data, aes(x = S_SAND, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 2) + xlim(0, 80) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_SAND


plot_NPP_S <- plot_grid(FIRE_mean, POP_mean, FIRE_slope, POP_slope, Age, DEM, S_CEC, S_OC, S_SAND,
                        align="hv", label_size = 20, ncol = 9, nrow = 1, label_fontface = "plain")
plot_NPP_S


#################################################################################################################################     kNDVI


model <- glm(kNDVI_S2 ~  H_mean + H_D +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean + Age + DEM + S_CEC + S_OC + S_SAND, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("FIRE_mean", "POP_mean", "FIRE_slope", "POP_slope", "Age", "DEM", "S_CEC", "S_OC", "S_SAND")])
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
# 计算Slope 结束 #ylim(-0.004, 0.005)

FIRE_mean <- ggplot(predicted_data, aes(x = FIRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= F, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0, 1000000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_mean

POP_mean <- ggplot(predicted_data, aes(x = POP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0, 100) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_mean

FIRE_slope <- ggplot(predicted_data, aes(x = FIRE_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(-250000, 250000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_slope


POP_slope <- ggplot(predicted_data, aes(x = POP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(-10, 10) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_slope

Age <- ggplot(predicted_data, aes(x = Age, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0, 200) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Age


DEM <- ggplot(predicted_data, aes(x = DEM, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0, 2000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
DEM



S_CEC <- ggplot(predicted_data, aes(x = S_CEC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0, 150) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_CEC



S_OC <- ggplot(predicted_data, aes(x = S_OC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0, 40) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_OC


S_SAND <- ggplot(predicted_data, aes(x = S_SAND, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.004, 0.005) + xlim(0, 80) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_SAND

plot_kNDVI <- plot_grid(FIRE_mean, POP_mean, FIRE_slope, POP_slope, Age, DEM, S_CEC, S_OC, S_SAND,
                        align="hv", label_size = 20, ncol = 9, nrow = 1, label_fontface = "plain")
plot_kNDVI




#################################################################################################################################     VOD_Ku_S

model <- glm(VOD_Ku_S ~  H_mean + H_D +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean + Age + DEM + S_CEC + S_OC + S_SAND, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("FIRE_mean", "POP_mean", "FIRE_slope", "POP_slope", "Age", "DEM", "S_CEC", "S_OC", "S_SAND")])
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
# 计算Slope 结束 #ylim(0, 0.005)


FIRE_mean <- ggplot(predicted_data, aes(x = FIRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0, 10000000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_mean

POP_mean <- ggplot(predicted_data, aes(x = POP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= F, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0, 0.1) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_mean

FIRE_slope <- ggplot(predicted_data, aes(x = FIRE_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(-250000, 250000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_slope


POP_slope <- ggplot(predicted_data, aes(x = POP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(-10, 10) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_slope

Age <- ggplot(predicted_data, aes(x = Age, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0, 200) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Age


DEM <- ggplot(predicted_data, aes(x = DEM, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0, 2000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
DEM



S_CEC <- ggplot(predicted_data, aes(x = S_CEC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0, 150) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_CEC



S_OC <- ggplot(predicted_data, aes(x = S_OC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0, 40) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_OC


S_SAND <- ggplot(predicted_data, aes(x = S_SAND, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(0, 0.005) + xlim(0, 80) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_SAND


plot_VOD_Ku_S <- plot_grid(FIRE_mean, POP_mean, FIRE_slope, POP_slope, Age, DEM, S_CEC, S_OC, S_SAND,
                           align="hv", label_size = 20, ncol = 9, nrow = 1, label_fontface = "plain")
plot_VOD_Ku_S


#################################################################################################################################     Carbon_S

model <- glm(Carbon_S ~  H_mean + H_D +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean + Age + DEM + S_CEC + S_OC + S_SAND, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("FIRE_mean", "POP_mean", "FIRE_slope", "POP_slope", "Age", "DEM", "S_CEC", "S_OC", "S_SAND")])
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
# 计算Slope 结束 #ylim(-0.8, 0.8)

FIRE_mean <- ggplot(predicted_data, aes(x = FIRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0, 10000000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_mean

POP_mean <- ggplot(predicted_data, aes(x = POP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0, 100) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_mean

FIRE_slope <- ggplot(predicted_data, aes(x = FIRE_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(-250000, 250000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_slope


POP_slope <- ggplot(predicted_data, aes(x = POP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(-10, 10) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_slope



Age <- ggplot(predicted_data, aes(x = Age, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0, 200) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Age

DEM <- ggplot(predicted_data, aes(x = DEM, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0, 2000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
DEM



S_CEC <- ggplot(predicted_data, aes(x = S_CEC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0, 150) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_CEC



S_OC <- ggplot(predicted_data, aes(x = S_OC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0, 40) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_OC


S_SAND <- ggplot(predicted_data, aes(x = S_SAND, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.8, 0.8) + xlim(0, 80) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_SAND


plot_Carbon_S <- plot_grid(FIRE_mean, POP_mean, FIRE_slope, POP_slope, Age, DEM, S_CEC, S_OC, S_SAND,
                           align="hv", label_size = 20, ncol = 9, nrow = 1, label_fontface = "plain")
plot_Carbon_S


#################################################################################################################################     Biomass_

model <- glm(Biomass_ ~ H_mean + H_D +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean + Age + DEM + S_CEC + S_OC + S_SAND, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("FIRE_mean", "POP_mean", "FIRE_slope", "POP_slope", "Age", "DEM", "S_CEC", "S_OC", "S_SAND")])
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
# 计算Slope 结束 #ylim(-0.2, 0.3)

FIRE_mean <- ggplot(predicted_data, aes(x = FIRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0, 10000000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_mean

POP_mean <- ggplot(predicted_data, aes(x = POP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0, 100) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_mean

FIRE_slope <- ggplot(predicted_data, aes(x = FIRE_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(-250000, 250000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_slope


POP_slope <- ggplot(predicted_data, aes(x = POP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(-10, 10) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_slope


Age <- ggplot(predicted_data, aes(x = Age, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0, 200) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Age

DEM <- ggplot(predicted_data, aes(x = DEM, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0, 2000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
DEM



S_CEC <- ggplot(predicted_data, aes(x = S_CEC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0, 150) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_CEC



S_OC <- ggplot(predicted_data, aes(x = S_OC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0, 40) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_OC


S_SAND <- ggplot(predicted_data, aes(x = S_SAND, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-0.2, 0.3) + xlim(0, 80) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_SAND


plot_Biomass_S <- plot_grid(FIRE_mean, POP_mean, FIRE_slope, POP_slope, Age, DEM, S_CEC, S_OC, S_SAND,
                            align="hv", label_size = 20, ncol = 9, nrow = 1, label_fontface = "plain")
plot_Biomass_S


#################################################################################################################################     Stabi_S

model <- glm(Stabi_S ~ H_mean + H_D +TMP_slope + Pre_slope + TMP_mean + PRE_mean + FIRE_slope + POP_slope + FIRE_mean + POP_mean + Age + DEM + S_CEC + S_OC + S_SAND, data = data)
summary(model)

# 预测值及其置信区间
predictions <- predict(model, interval = "confidence", level = 0.95)
predicted_df <- as.data.frame(predictions)
predicted_data <- cbind(predicted_df, data[c("FIRE_mean", "POP_mean", "FIRE_slope", "POP_slope", "Age", "DEM", "S_CEC", "S_OC", "S_SAND")])
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
# 计算Slope 结束 #ylim(-1, 2)

FIRE_mean <- ggplot(predicted_data, aes(x = FIRE_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0, 10000000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_mean

POP_mean <- ggplot(predicted_data, aes(x = POP_mean, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0, 100) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_mean

FIRE_slope <- ggplot(predicted_data, aes(x = FIRE_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(-250000, 250000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
FIRE_slope


POP_slope <- ggplot(predicted_data, aes(x = POP_slope, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(-10, 10) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
POP_slope

Age <- ggplot(predicted_data, aes(x = Age, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0, 200) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
Age

DEM <- ggplot(predicted_data, aes(x = DEM, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0, 2000) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
DEM



S_CEC <- ggplot(predicted_data, aes(x = S_CEC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0, 150) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_CEC



S_OC <- ggplot(predicted_data, aes(x = S_OC, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0, 40) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_OC


S_SAND <- ggplot(predicted_data, aes(x = S_SAND, y = predictions)) +
  #geom_point()+
  geom_smooth(method = "glm",  color="black",  se= TRUE, fill = "#36ACA2", size = 1.5) +
  ylim(-1, 2) + xlim(0, 80) +
  theme_linedraw(base_size = 40)+
  theme(text = element_text(size = 30), axis.text = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
S_SAND


plot_Stabi_S <- plot_grid(FIRE_mean, POP_mean, FIRE_slope, POP_slope, Age, DEM, S_CEC, S_OC, S_SAND,
                          align="hv", label_size = 20, ncol = 9, nrow = 1, label_fontface = "plain")
plot_Stabi_S






plot_H_climate <- plot_grid(plot_NPP_S, plot_kNDVI,  plot_VOD_Ku_S, plot_Carbon_S, plot_Biomass_S, plot_Stabi_S, 
                            align="hv",  ncol = 1, nrow = 6, label_fontface = "plain")
plot_H_climate


ggsave("plot_trend_distur1126.jpg",device = "jpg",width = 1200, height = 800, units = "mm", dpi = 600)

