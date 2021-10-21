library(ggplot2)
library(dplyr)

source("fct_plot.R")

########## CHARGEMENT DES DONNÉES ##########
#Données température
load("./data/t2m_daily_fr.RData")
#Données totla precipitation
load("./data/tp_daily_fr.RData")

load("../../Data/WEGE_ERA5_2000-2021.RData")
load("../../Data/Winter/WEGE_ERA5_1950-2021.RData")




