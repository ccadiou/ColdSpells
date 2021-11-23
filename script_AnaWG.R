library(ggplot2)
library(dplyr)
library(plyr)
library(gridExtra)

source("fct_plot.R")

########## CHARGEMENT DES DONNÉES ##########
#### 2010 - alpha.cal=3 ####
load("../../Data/Winter/TG-France-Y2010m12d1_HW-animpsa_cal3_TG0meth1-860981.Rdat")
simu.dyn.2010.3 <-  adply(simu.dyn$l.X$`2010`, 1)
simu.dyn.2010.3$t.sim <- as.Date(as.character(simu.dyn.2010.3$t.sim),format="%Y%m%d")

simu.dyn.2010.3.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`2010`,1))
colnames(simu.dyn.2010.3.Xmean) <- c("nsim","y")
simu.dyn.2010.3.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`2010`,1))
colnames(simu.dyn.2010.3.Tmean) <- c("nsim","y")


#### 2010 - alpha.cal=6 ####
load("../../Data/Winter/TG-France-Y2010m12d1_HW-animpsa_cal6_TG0meth1-860981.Rdat")
simu.dyn.2010.6 <-  adply(simu.dyn$l.X$`2010`, 1)
simu.dyn.2010.6$t.sim <- as.Date(as.character(simu.dyn.2010.6$t.sim),format="%Y%m%d")

simu.dyn.2010.6.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`2010`,1))
colnames(simu.dyn.2010.6.Xmean) <- c("nsim","y")
simu.dyn.2010.6.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`2010`,1))
colnames(simu.dyn.2010.6.Tmean) <- c("nsim","y")


#### 1962 - alpha.cal=3 ####
load("../../Data/Winter/TG-France-Y1962m12d1_HW-animpsa_cal3_TG0meth1-860981.Rdat")
simu.dyn.1962.3 <-  adply(simu.dyn$l.X$`1962`, 1)
simu.dyn.1962.3$t.sim <- as.Date(as.character(simu.dyn.1962.3$t.sim),format="%Y%m%d")

simu.dyn.1962.3.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`1962`,1))
colnames(simu.dyn.1962.3.Xmean) <- c("nsim","y")
simu.dyn.1962.3.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`1962`,1))
colnames(simu.dyn.1962.3.Tmean) <- c("nsim","y")


#### 1962 - alpha.cal=6 ####
load("../../Data/Winter/TG-France-Y1962m12d1_HW-animpsa_cal6_TG0meth1-860981.Rdat")
simu.dyn.1962.6 <-  adply(simu.dyn$l.X$`1962`, 1)
simu.dyn.1962.6$t.sim <- as.Date(as.character(simu.dyn.1966.6$t.sim),format="%Y%m%d")

simu.dyn.1962.6.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`1962`,1))
colnames(simu.dyn.1962.6.Xmean) <- c("nsim","y")
simu.dyn.1962.6.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`1962`,1))
colnames(simu.dyn.1962.6.Tmean) <- c("nsim","y")

#### plot ####
p1 <- plot_box(simu.dyn.2010.3.Xmean,dfval=simu.dyn.2010.3.Tmean,title="2010 - alpha.cal=3")
p2 <- plot_box(simu.dyn.2010.6.Xmean,dfval=simu.dyn.2010.6.Tmean,title="2010 - alpha.cal=6")
p3 <- plot_box(simu.dyn.1962.3.Xmean,dfval=simu.dyn.1962.3.Tmean,title="1962 - alpha.cal=3")
p4 <- plot_box(simu.dyn.1962.6.Xmean,dfval=simu.dyn.1962.6.Tmean,title="1962 - alpha.cal=6")
grid.arrange(p1, p1, p3, p4, ncol=2, nrow = 2)

