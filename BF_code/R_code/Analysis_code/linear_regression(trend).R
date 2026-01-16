print(getwd())
setwd("D:/Paper/Boreal_forest/Divers_environmental_variables/Trend/Fire")
print(getwd())
library(sp)
library(raster)
library(broom)
library(rgdal)

raster1<-stack(list.files(pattern='*.tif$'))
dim(raster1)
time<-1:nlayers(raster1)
length(time)
#coefficients
fun1 <- function(x){if(is.na(x[1])){ NA } else lm(x ~ time)$coefficients[2] }
#Pvalue
fun2 <- function(x) {if (is.na(x[1])){ NA } else glance(lm(x ~ time))$p.value }
#r2
fun3<- function(x) {if(is.na(x[1])){ NA } else glance(lm(x ~ time))$r.squared }

wue.b<-calc(raster1, fun1)
wue.p<-calc(raster1, fun2)
wue.r2<-calc(raster1, fun3)

writeRaster(wue.b, filename = file.path("D:/Paper/Boreal_forest/Divers_environmental_variables/Trend", "Fire_b.tif"),format="GTiff", overwrite=TRUE)
writeRaster(wue.p, filename = file.path("D:/Paper/Boreal_forest/Divers_environmental_variables/Trend", "Fire_p.tif"),format="GTiff", overwrite=TRUE)
writeRaster(wue.r2, filename = file.path("D:/Paper/Boreal_forest/Divers_environmental_variables/Trend", "Fire_r2.tif"),format="GTiff", overwrite=TRUE)

