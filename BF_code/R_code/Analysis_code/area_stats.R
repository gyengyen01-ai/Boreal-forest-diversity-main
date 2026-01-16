print(getwd())
setwd("D:/Paper/Boreal_forest/Object_O_diversity/results/total_test")

library(raster)

raster_data <- raster("slope_in_and_de.tif")

projection(raster_data)

area(raster_data, na.rm = T)


threshold_value <- 0

selected_cells <- raster_data < threshold_value

area(selected_cells, na.rm = TRUE)

selected_area

# 将面积从平方米转换为平方千米
selected_area_km2 <- selected_area / 1000000
selected_area_km2
