# Topic 4

# load libraries
library(tidyverse)
library(ggthemr)
ggthemr("fresh")

# Worldwide non-commercial space launches
# correlates with
# Sociology doctorates awarded (US)
# LINK: https://tylervigen.com/spurious-correlations

# define data
spaceLaunch <- c(54, 46, 42, 50, 43, 41, 46, 39, 37, 45, 45, 41, 54) 
year <- c(1997:2009)
degrees <- c(601, 579, 572, 617, 566, 547, 597, 580, 536, 579, 576, 601, 664)
# create dataframe
dfSpurious <- data.frame(year, spaceLaunch, degrees)

# plot data
ggplot(dfSpurious, aes(x = year))+
  geom_line(aes(y = spaceLaunch), color = "red")+
  geom_line(aes(y = degrees/10), color = "black")+
  geom_point(aes(y = spaceLaunch), color = "red")+
  geom_point(aes(y = degrees/10), color = "black")+
  scale_y_continuous(
    name = "Worldwide non-commercial space launches",
    sec.axis = sec_axis(~.*10 , name = "Sociology doctorates awarded (US)")
  )+
  labs(title = "Worldwide non-comercial space launches \ncorrelates with \nSociology doctorates awarded (US)")+
  theme(axis.title.y.left=element_text(colour="red"), axis.title.y.right = element_text(color = "black"))+
  scale_x_continuous("year",labels = as.character(year),breaks = year)

