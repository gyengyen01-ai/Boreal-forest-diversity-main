print(getwd())
setwd("D:/Papering/boreal_forest/Forest_diversity_results/BRT")

library(ggBRT)
library(dismo)
library(tidyverse)
library(ggplot2)
library(caret)
library(gbm)

data <- read.csv("total_point_BRTless.csv")
head(data)

set.seed(12)
indata <- createDataPartition(
  data$D_2020,
  p =.80,
  list = FALSE
)

training <- data[indata,]
testing <- data[-indata,]
set.seed(1)
brt1 <- gbm.step(data=data, gbm.x =2:10, gbm.y = 1,
                 family = "gaussian", tree.complexity = 9,
                 learning.rate = 0.02, bag.fraction = 0.4)
ggInfluence(brt1)
ggPD(brt1, rug = T)

#?ggPD
# Boostrap the BRT 1000 times to build confidence intervals
brt1.prerun<- plot.gbm.4list(brt1)
brt1.boot <- gbm.bootstrap.functions(brt1, list.predictors=brt1.prerun, n.reps=10)

ggPD <- ggPD_boot(brt1, list.4.preds=brt1.prerun,  cex.line =2,
          booted.preds=brt1.boot$function.preds, type.ci = "ribbon", ncol = 3, nrow = 3,
          rug = T, rug.pos = "b",smooth= F,col.line = "darkorange",
          y.label = "Shannon index")
ggPD

ggsave("BRT_1117_test.jpg", plot = ggPD, device = "jpg",width = 300, height = 300, units = "mm", dpi = 500)




