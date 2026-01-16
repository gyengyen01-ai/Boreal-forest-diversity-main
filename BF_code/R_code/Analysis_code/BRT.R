print(getwd())
setwd("D:/Papering/boreal_forest/Forest_diversity_results/BRT")

library(dismo)
library(tidyverse)
library(ggplot2)
library(caret)
library(gbm)

## Load data
data <- read.csv("total_point_1110_2010less.csv", header=TRUE)     ## load csv file
head(data)

set.seed(1)
indata <- createDataPartition(
  data$D_2010,
  p =.80,
  list = FALSE
)

training <- data[indata,]
testing <- data[-indata,]

set.seed(1)
#预测变量:gbm.x ; 响应变量:gbm.y;
Alpha <- gbm.step(data = training, gbm.x = 2:10, gbm.y = 1,
                       family = "gaussian", tree.complexity = 9,
                       learning.rate = 0.001, bag.fraction = 0.5)
summary(Alpha)
#gbm.plot.fits(Alpha)


test.features = subset(testing, select = -c(D_2010))
test.target = subset(testing, select=D_2010)[,1]
# test.features = subset(testing, select = -c(litterfall))
# test.target = subset(testing, select=litterfall)[,1]

preds <- predict.gbm(Alpha, testing,
                     n.trees = Alpha$gbm.call$best.trees,
                     type="response")


sqrt(mean((test.target - preds)^2))
cor(test.target, preds)^2

 
# set.seed(1)
# indata <- createDataPartition(
#   data$NPP,
#   p =.80,
#   list = FALSE
# )
# 
# training <- data[indata,]
# testing <- data[-indata,]
# 
# set.seed(1)
# #预测变量:gbm.x ; 响应变量:gbm.y;
# NPP_trend <- gbm.step(data = training, gbm.x = 2:5, gbm.y = 1,
#                             family = "gaussian", tree.complexity = 4,
#                             learning.rate = 0.01, bag.fraction = 0.5)
# summary(NPP_trend)
# test.features = subset(testing, select = -c(NPP_trend))
# test.target = subset(testing, select=NPP_trend)[,1]
# 
# preds <- predict.gbm(NPP_trend, testing,
#                      n.trees = NPP_trend$gbm.call$best.trees,
#                      type="response")
# 
# 
# sqrt(mean((test.target - preds)^2))
# cor(test.target, preds)^2

