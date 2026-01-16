print(getwd())
setwd("D:/Paper/Boreal_forest/Structural_Equation_Modeling/stats/wang")

library(piecewiseSEM)

## Load data
data <- read.csv("5.CSV", header=TRUE)     ## load csv file
head(data) 


######################---------初次假设------###################
pmodel <- psem(
  glm(VI ~  EI + SI + RI, family = "gaussian",data),
  glm(EI ~  DEM + Veg + Woody + POP_2 + T + P, family = "gaussian",data),
  glm(SI ~  DEM + Veg + Woody + POP_2 + T + P, family = "gaussian",data),
  glm(RI ~  DEM + Veg + Woody + POP_2 + T + P, family = "gaussian",data),
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
                  VI ~  EI + SI + RI+ DEM + Veg + Woody + T + P,
                  EI ~  DEM + Veg + Woody + POP_2 + T + P,
                  SI ~  DEM + Veg + Woody + POP_2 + T + P,
                  RI ~  DEM + Veg + Woody + POP_2 + T + P,
                  EI %~~% SI,
                  RI %~~% SI,
                  EI %~~% RI)
 
pmodel2

######################--------再次评估---------#############
basisSet(pmodel2)
dSep(pmodel2,conditioning=T)
fisherC(pmodel2)

summary(pmodel2)
