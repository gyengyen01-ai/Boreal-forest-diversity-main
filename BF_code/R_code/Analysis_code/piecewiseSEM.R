print(getwd())
setwd("D:/Paper/Boreal_forest/Structural_Equation_Modeling/stats")

library(piecewiseSEM)

###################有 NPP
## Load data
data <- read.csv("total_enviroment.csv", header=TRUE)     ## load csv file
head(data)

pmodel <- psem(
  lm(Sta_2020 ~ D_2020 + NPP_2020 , data),
  lm(NPP_2020 ~  TMP_2020 + PRE_2020 + DEM + POP_2020_2 + FIRE_2020_2, data),
  lm(D_2020 ~ TMP_2020 + PRE_2020 + DEM + POP_2020_2 + FIRE_2020_2, data),

  data = data
)


summary(pmodel)

summary(pmodel, conserve = T)

basisSet(pmodel)

#conduct d-sep test
dsep<-dSep(pmodel)
dsep

dSep(pmodel,conditioning=T)

fisherC(pmodel)


pmodel2 <- update(pmodel,
                  Sta_2020 ~ TMP_2020 + D_2020 + NPP_2020,
                  Sta_2020 ~ PRE_2020 + D_2020 + NPP_2020,
                  Sta_2020 ~ DEM + D_2020 + NPP_2020,
                  Sta_2020 ~ POP_2020_2 + D_2020 + NPP_2020,
                  Sta_2020 ~ FIRE_2020_2 + D_2020 + NPP_2020,
                  NPP_2020 ~ D_2020 + TMP_2020 + PRE_2020 + DEM + POP_2020_2 + FIRE_2020_2)

pmodel2

basisSet(pmodel2)
dSep(pmodel2,conditioning=T)
fisherC(pmodel2)

summary(pmodel2)


pmodel3 <- update(pmodel,
                  Sta_2020 ~ TMP_2020 + FIRE_2020_2 + D_2020 + NPP_2020,
                  Sta_2020 ~ PRE_2020 + FIRE_2020_2 + D_2020 + NPP_2020,
                  Sta_2020 ~ DEM + FIRE_2020_2 + D_2020 + NPP_2020,
                  Sta_2020 ~ POP_2020_2 + FIRE_2020_2 + D_2020 + NPP_2020)
pmodel3
summary(pmodel3)

basisSet(pmodel3)
dSep(pmodel3,conditioning=T)

pmodel4 <- update(pmodel,
                  Sta_2020 ~ TMP_2020 + POP_2020_2 + FIRE_2020_2 + D_2020 + NPP_2020,
                  Sta_2020 ~ PRE_2020 + POP_2020_2 + FIRE_2020_2 + D_2020 + NPP_2020,
                  Sta_2020 ~ DEM + POP_2020_2 + FIRE_2020_2 + D_2020 + NPP_2020,
                  NPP_2020 ~ D_2020 + POP_2020_2 + FIRE_2020_2 + TMP_2020 + PRE_2020 + DEM)
pmodel4
summary(pmodel4)

basisSet(pmodel4)
dSep(pmodel4,conditioning=T)

pmodel5 <- update(pmodel,
                  Sta_2020 ~ TMP_2020 + DEM + POP_2020_2 + FIRE_2020_2 + D_2020 + NPP_2020)
pmodel5
summary(pmodel5)







# ################################ No NPP ##################
# 
# ## Load data
# data <- read.csv("25000.csv", header=TRUE)     ## load csv file
# head(data) 
# 
# 
# ######################---------初次假设------###################
# pmodel <- psem(
#   glm(Sta_2020 ~  Div_2020, family = "gaussian",data),
#   glm(Div_2020 ~ Temp_2020 + Pre_2020 + DEM+ pop_2020_2 +Fire_2019_2, family = "gaussian",data),
#   data = data
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
#                   Sta_2020 ~ Temp_2020 + Pre_2020 + Div_2020 + DEM+ pop_2020_2 + Fire_2019_2,
#                   Div_2020 ~ Temp_2020 + Pre_2020 + DEM + Fire_2019_2)
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





