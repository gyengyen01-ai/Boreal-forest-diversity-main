print(getwd())

setwd("D:/Paper/Boreal_forest/Divers_environmental_variables/temperature/cru")

library(tidyverse)
library(ggplot2)
data <-read_csv("CRU_temperature.csv")
#load formula
my.formula <- y ~ poly(x, 2)

df <-data.frame(data=data, x="Year",
                y1="Global",
                y2="Tropical",
                y3="Drylands",
                y4="Temperate",
                y5="Boreal",
                y6="Polar")



plot5 <- ggplot(data=data, mapping = aes(x = Year, y= Boreal)) +
  scale_x_continuous(expand = c(0,0), limits = c(1958,2022),breaks = seq(1960,2020,10))+
  # scale_y_continuous(expand = c(0,0), limits = c(1995,2025), breaks = seq(1998,2024,4))+
  geom_line(color= "#EC5621",size = 1.3) +
  geom_point(color= "#EC3521",size = 2) +
  geom_smooth(aes(x = Year, y= Boreal), method = "lm", formula = my.formula, color="black", se= TRUE,linetype = "dashed", size = 1.5) +
  # geom_smooth(aes(x = Year, y= DCF3_Mean), method = "lm", formula = my.formula, color="#006300", se= FALSE,linetype = "dashed",size = 1.5, alpha=0.4) +
  # geom_smooth(aes(x = Year, y= MF5_Mean), method = "lm", formula = my.formula, color="#816815", se= FALSE,linetype = "dashed",size = 1.5) +
  # geom_smooth(aes(x = Year, y= DBF6_Mean), method = "lm", formula = my.formula, color="#ff0000", se= FALSE,linetype = "dashed",size = 1.5) +
  # geom_line(aes(y = DCF3_Mean), colour = "#006300",size = 1) +
  # geom_line(aes(y = MF5_Mean), colour = "#816815",size = 1) +
  # geom_line(aes(y = DBF6_Mean), colour = "#ff0000",size = 1) +
  labs(x="Year", y = "Temperature (°C)")
plot5  + theme_linedraw(base_size = 60) +
  theme(text = element_text(size = 30), axis.text = element_text(size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

ggsave("CRU_temperature.jpg",device = "jpg",width = 300, height = 300, units = "mm", dpi = 300)