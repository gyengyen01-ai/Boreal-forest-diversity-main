print(getwd())
setwd("E:/SEM")

library(piecewiseSEM)
library(raster)
# 
# file <- 'mean_stability_annual_1982-2016.tif'
# sub <- raster(file)
# gpp <- as.matrix(sub)
# m <- dim(gpp)
# m
# sta <- matrix(gpp, m[1]*m[2],1)
# 
# file <- 'S_mean_raster_p25.tif'
# sub <- raster(file)
# gpp <- as.matrix(sub)
# bio <- matrix(gpp, m[1]*m[2],1)
# 
# file <- 'mean_CRU-pr_season_ab0_1982-2016.tif'
# sub <- raster(file)
# gpp <- as.matrix(sub)
# pr <- matrix(gpp, m[1]*m[2],1)
# 
# file <- 'mean_CRU-tmp_season_ab0_1982-2016.tif'
# sub <- raster(file)
# gpp <- as.matrix(sub)
# 
# tmp <- matrix(gpp, m[1]*m[2],1)
# 
# 
# file <- 'Beck_KG_biomes_025-present.tiff'
# sub <- raster(file)
# gpp <- as.matrix(sub)
# 
# lc <- matrix(gpp, m[1]*m[2],1)
# 
# 
# 
# daff <- data.frame(sta, bio,pr,tmp,lc)
# daff <- na.omit(daff)
# head(daff)
# min(daff)
# max(daff)
# nrow(daff)
# 
# 
# daff$lc[daff$lc ==2|daff$lc ==3] =1 ## group 6 and 7 shrub
# daff$lc[daff$lc ==5|daff$lc ==6|daff$lc ==7] = 4
# daff$lc[daff$lc ==9|daff$lc ==10|daff$lc ==11|daff$lc ==12|daff$lc ==13|daff$lc ==14|daff$lc ==15|daff$lc ==16] =8
# daff$lc[daff$lc ==18|daff$lc ==19|daff$lc ==20|daff$lc ==21|daff$lc ==22|daff$lc ==23|daff$lc ==24|daff$lc ==25|daff$lc ==26|daff$lc ==27|daff$lc ==28] =17 ##  forest
# daff$lc[daff$lc ==30] =29
# 
# unique(daff$lc)
# 
# nrow(daff)
# ####global
# ######################---------初次假设------###################
# pmodel <- psem(
#   glm(sta ~ bio , family = "gaussian",daff),
#   glm(bio ~ pr + tmp, family = "gaussian",daff),
#   pr %~~% tmp,
#   data = daff
# )
# ######################--------结果总结--------################
# summary(pmodel)
# summary(pmodel, conserve = T)
# 
# #####################-----模型评估和修改-----###############
# basisSet(pmodel)
# dSep(pmodel,conditioning=T)
# fisherC(pmodel)
# 
# #######################------升级模型-------################
# pmodel2 <- update(pmodel,
#                   sta ~ bio+ tmp,
#                   sta ~ pr + bio)
# 
# pmodel2
# 
# ######################--------再次评估---------#############
# basisSet(pmodel2)
# dSep(pmodel2,conditioning=T)
# fisherC(pmodel2)
# 
# summary(pmodel2)
# 
# 
# 
# 
# ## case1 tropical
# 
# daff1 <- subset(daff,lc==1)
# nrow(daff1)
# 
# ######################---------初次假设------###################
# pmodel <- psem(
#   glm(sta ~ bio , family = "gaussian",daff1),
#   glm(bio ~ pr + tmp, family = "gaussian",daff1),
#   pr %~~% tmp,
#   data = daff1
# )
# ######################--------结果总结--------################
# summary(pmodel)
# summary(pmodel, conserve = T)
# 
# #####################-----模型评估和修改-----###############
# basisSet(pmodel)
# dSep(pmodel,conditioning=T)
# fisherC(pmodel)
# 
# #######################------升级模型-------################
# pmodel2 <- update(pmodel,
#                   sta ~ bio+ tmp,
#                   sta ~ pr + bio)
# 
# pmodel2
# 
# ######################--------再次评估---------#############
# basisSet(pmodel2)
# dSep(pmodel2,conditioning=T)
# fisherC(pmodel2)
# summary(pmodel2)
# 
# 
# 
# 
# 
# ## case2 boreal
# 
# daff2 <- subset(daff,lc==17)
# nrow(daff2)
# 
# ######################---------初次假设------###################
# pmodel <- psem(
#   glm(sta ~ bio , family = "gaussian",daff2),
#   glm(bio ~ pr + tmp, family = "gaussian",daff2),
#   tmp %~~% pr,
#   data = daff2
# )
# ######################--------结果总结--------################
# summary(pmodel)
# summary(pmodel, conserve = T)
# 
# #####################-----模型评估和修改-----###############
# basisSet(pmodel)
# dSep(pmodel,conditioning=T)
# fisherC(pmodel)
# 
# 
# #######################------升级模型-------################
# pmodel2 <- update(pmodel,
#                   sta ~ bio + tmp,
#                   sta ~ pr + bio)
# 
# pmodel2
# 
# ######################--------再次评估---------#############
# basisSet(pmodel2)
# dSep(pmodel2,conditioning=T)
# fisherC(pmodel2)
# summary(pmodel2)



















print(getwd())
setwd("D:/Paper/Boreal_forest/Structural_Equation_Modeling/stats")

## Load data
data <- read.csv("SEM_forest.csv", header=TRUE)     ## load csv file
head(data)
nrow(data)


######################---------初次假设------###################
pmodel <- psem(
  glm(X2020_stab ~  Diversity_2020, family = "gaussian",data),
  glm(Diversity_2020 ~ Tem_2020 + Pre_2020 + DEM1+ POP_2020_2, family = "gaussian",data),
  data = data
)
######################--------结果总结--------################
summary(pmodel)
summary(pmodel, conserve = T)

#####################-----模型评估和修改-----###############
basisSet(pmodel)
dSep(pmodel,conditioning=T)
fisherC(pmodel)

#######################------升级模型-------################
pmodel2 <- update(pmodel,
                  X2020_stab ~ Diversity_2020 + Tem_2020 + Pre_2020 + POP_2020_2+ DEM1,
                  Diversity_2020 ~ Tem_2020 + Pre_2020 + DEM1)

pmodel2

######################--------再次评估---------#############
basisSet(pmodel2)
dSep(pmodel2,conditioning=T)
fisherC(pmodel2)

summary(pmodel2)
