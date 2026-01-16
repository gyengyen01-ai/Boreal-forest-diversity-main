print(getwd())

setwd("D:/Paper/Boreal_forest/Divers_environmental_variables/stats/resample_to_1")
#setwd("D:/Paper/Boreal_forest/Divers_environmental_variables\stats\new_for_OO")


library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(Hmisc)
library(corrplot)

data <-read_csv("total_2000.csv")
res2 <- rcorr(as.matrix(data))
res2

res <- cor(data)
round(res, 2)

testRes = cor.mtest(data, conf.level = 0.95)

# corrplot(res, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
#          addCoef.col = "black", tl.col = "black", tl.srt = 45,
#          sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
#          insig = 'label_sig', pch.col = 'grey20', order = 'AOE')

#trace(corrplot, edit=TRUE)
#change 442
# adjust text(X,Y ...) according to your needs, here +0.25 is added to the Y-position    
# place_points = function(sig.locs, point) {
#   text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
#        labels = point, col = pch.col, cex = pch.cex, 
#        lwd = 2)

corrplot(res, type = "upper", order = "hclust", addCoef.col = "black",diag = FALSE,number.cex = 1.5, tl.cex = 1.3, cl.cex = 1.5,
              p.mat = testRes$p, sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.1,insig = 'label_sig',
              tl.col = "black", tl.srt = 45)
