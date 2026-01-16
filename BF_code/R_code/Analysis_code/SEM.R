print(getwd())
setwd("D:/Paper/tropical_forest/environment/resample_to_1km/CSV")


## Load Libraries
library(lavaan)
library(lavaanPlot)
library(dplyr) 
library(tidyr)
library(knitr)
library(mvnormalTest)

## Load data
data <- read.csv("PCA_point_envi2.csv", header=TRUE)     ## load csv file
data <- na.omit(data) 
head(data)

mvnout <- mardia(data)
## Shapiro-Wilk Univariate normality test
mvnout$uv.shapiro
## Mardia Multivariate normaility test
mvnout$mv.test



## Model specification
model <- '
#1. latent variable definitions
  TEM =~ Temp_m
  PRE =~ Prec
  dem2 =~ DEM
  POP =~ POP2
  RAD =~ Srad
  Fire2 =~ Fire
  Soil =~ S_CEC
  GPP =~ FluxSat
  Diversity =~ FSDI
  Biomass =~ Carbon

#2. regressions
  Stability ~ Temp_m + PRE + dem2 + Diversity + Fire + POP
  
  Diversity ~ TEM + PRE + dem2+ Fire '
# Stability ~ TEM + PRE + DEM + POP + Type + NPP + Diversity
  # NPP ~ TEM + PRE + DEM + POP + Type + Diversity
  # Diversity ~ TEM + PRE + DEM + POP + Type + NPP'


## sem function syntax
fit.mod <- sem(model, data=data, std.lv = TRUE, estimator = "MLM")

fitMeasures(fit.mod, c("chisq.scaled", "df.scaled", "pvalue.scaled"))

fitMeasures(fit.mod, c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled"))

fitMeasures(fit.mod, c("cfi.scaled", "srmr"))
fitMeasures(fit.mod, "all")
fitMeasures(fit.mod, c("pvalue"))
# fitMeasures(fit.mod, c("npar"))
# fitMeasures(fit.mod, c("logl"))
#Akaike information criteria (AIC)=(2*"npar"-2*"logl")/n
summary(fit.mod,rsq = T,standardize=T)
resid(fit.mod, type = "standardized")
modindices(fit.mod)

# standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>%
#   filter(op == "=~") %>%
#   select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)

parameterEstimates(fit.mod, standardized=TRUE, rsquare = TRUE) %>% 
  filter(op == "r2") %>% 
  select(Item=rhs, R2 = est) 

standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)






