print(getwd())
setwd("D:/Papering/boreal_forest")

#写出指定数据集中的一个
library(raster)
library(ncdf4)
ncfile<-nc_open("D:/Papering/boreal_forest/gloabl_forest_age.nc")
names(ncfile$var) #查看varname
prebr <- brick("D:/Papering/boreal_forest/gloabl_forest_age.nc")
raster::writeRaster(prebr, filename = paste0("D:/Papering/boreal_forest/ForestAge_TC010_",prebr@data@names),
                    bylayer=T, format="GTiff", varname = 'ForestAge_TC010')
#写出所有数据集
library(terra)
prebr <- rast("D:/Papering/boreal_forest/gloabl_forest_age.nc")
terra::writeRaster(prebr,paste0("D:/Papering/boreal_forest/gloabl_",names(prebr),'.tif'),
                   overwrite = T,filetype = "GTiff") 
