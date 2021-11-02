library(ggplot2)
library(dplyr)
library(plyr)

source("fct_plot.R")

########## CHARGEMENT DES DONNÉES ##########
#Données température
load("./data/t2m_daily_fr.RData")
#Données totla precipitation
load("./data/tp_daily_fr.RData")

# load("../../Data/Winter/WEGE_ERA5_1950-2021.Rdat")
load("../../Data/Winter/TG-Orly-m12d1_HW-animpsa_cal3_TG0meth1-834413.Rdat")

# View(simu.dyn)
simu.dyn.2010 <-  adply(simu.dyn$l.X$`2010`, 1)
# df  <- do.call(rbind,simu.dyn.2010)
simu.dyn.2010$t.sim <- as.character(simu.dyn.2010$t.sim)
simu.dyn.2010$t.sim <- as.Date(simu.dyn.2010$t.sim,format="%Y%m%d")

test <- cbind(simu.dyn.2010,"t"=seq(as.Date("2010-12-01"),as.Date("2011-03-01"),by="day"))
