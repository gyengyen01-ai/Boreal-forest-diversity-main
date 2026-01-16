print(getwd())

setwd("D:/Paper/Boreal_forest/Divers_environmental_variables/temperature/cru")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)

data <-read_csv("SSP_245_558.csv")
#load formula
my.formula <- y ~ poly(x, 2)

df <-data.frame(data=data, x="Year",
                y1="CRU",
                y2="Average_245",
                y3="STD_245",
                y4="Average_585",
                y5="STD_585",
                y6="historical",
                y7= "diversity")



plot5 <- ggplot(data=data, mapping = aes(x = Year)) +
  scale_x_continuous(expand = c(0,0), limits = c(1958,2102),breaks = seq(1960,2100,20))+
  scale_y_continuous( # Features of the first axis
    name = "Temperature (°C)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.0335, name="Shannon index"))+
  coord_cartesian(ylim = c(8.5, 20)) +
  geom_bar(aes(x = Year, y= diversity), stat="identity", width=6, size=5, fill="#2C8881", alpha=1) +
  geom_errorbar( aes(x = Year, ymin=diversity-diversity_std, ymax=diversity+diversity_std), width=3, colour="#24726C",  size=1)+
  geom_line(aes(x = Year, y= CRU), color= "#d55535",size = 1.5) +
  geom_line(aes(x = Year, y= Average_245), color= "#437899",size = 1.5) +
  geom_ribbon(aes(x = Year, y= Average_245, ymin = Average_245-STD_245, ymax = Average_245 + STD_245), fill = "#437899",alpha=0.2) +
  geom_line(aes(x = Year, y= Average_585), color= "#FA8600",size = 1.5) +
  geom_ribbon(aes(x = Year, y= Average_585, ymin = Average_585-STD_585, ymax = Average_585 + STD_585), fill = "#FA8600",alpha=0.2) +
  geom_line(aes(x = Year, y= historical), color= "black",size = 1.5) +
  geom_ribbon(aes(x = Year, y= historical, ymin = historical-STD, ymax = historical + STD), fill = "black",alpha=0.2) +
  
  # geom_point(color= "#EC3521",size = 2) +
  # geom_smooth(aes(x = Year, y= Boreal), method = "lm", formula = my.formula, color="black", se= TRUE,linetype = "dashed", size = 1.5) +

  labs(x="Year", y = "Temperature (°C)")
plot5  + theme_linedraw(base_size = 60) +
  theme(text = element_text(size = 35), axis.text = element_text(size = 35),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

ggsave("Temperature_1960_2100_0705.jpg",device = "jpg",width = 450, height = 300, units = "mm", dpi = 300)
