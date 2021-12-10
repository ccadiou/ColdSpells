library(ggplot2)
library(dplyr)
library(plyr)
library(gridExtra)

source("fct_plot.R")

########## CHARGEMENT DES DONNÉES ##########
#importance sampling
path <- "~/Documents/These/Data/Winter/WEGE/"
fnames <- list.files(path)

name <- "TX-m12d1_UNCLE-min-animsa_cal3_TX0.5meth1-897290-1951-2021.Rdat"
ymin <- 1951
ymax <- 2020
mstart <- 12
dstart <- 01

plot_SWG(path,fnames[[1]],ymin,ymax,mstart,dstart)
plot_SWG(path,fnames[[5]],ymin,ymax,mstart,dstart)


load(paste(path,fnames[[5]],sep=""))

test <- bind_rows(lapply(simu.dyn$l.X[[1]],as.data.frame))
test$t.sim <- as.character(test$t.sim)
test$t.sim <- paste(substring(test$t.sim,1,4),"-",substring(test$t.sim,5,6),"-",substring(test$t.sim,7,8),sep="")

##########
# Boxplots pour les simulations d'un événement _______________________________________________________________________________________________________
##########

### 2010 - alpha.cal=3 ##
load("../../Data/Winter/TG-France-Y2010m12d1_HW-animpsa_cal3_TG0meth1-860981.Rdat")
simu.dyn.2010.3 <-  adply(simu.dyn$l.X$`2010`, 1)
simu.dyn.2010.3$t.sim <- as.Date(as.character(simu.dyn.2010.3$t.sim),format="%Y%m%d")

simu.dyn.2010.3.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`2010`,1))
colnames(simu.dyn.2010.3.Xmean) <- c("nsim","y")
simu.dyn.2010.3.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`2010`,1))
colnames(simu.dyn.2010.3.Tmean) <- c("nsim","y")


#### 2010 - alpha.cal=6 ##
load("../../Data/Winter/TG-France-Y2010m12d1_HW-animpsa_cal6_TG0meth1-860981.Rdat")
simu.dyn.2010.6 <-  adply(simu.dyn$l.X$`2010`, 1)
simu.dyn.2010.6$t.sim <- as.Date(as.character(simu.dyn.2010.6$t.sim),format="%Y%m%d")

simu.dyn.2010.6.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`2010`,1))
colnames(simu.dyn.2010.6.Xmean) <- c("nsim","y")
simu.dyn.2010.6.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`2010`,1))
colnames(simu.dyn.2010.6.Tmean) <- c("nsim","y")


#### 1962 - alpha.cal=3 ##
load("../../Data/Winter/TG-France-Y1962m12d1_HW-animpsa_cal3_TG0meth1-860981.Rdat")
simu.dyn.1962.3 <-  adply(simu.dyn$l.X$`1962`, 1)
simu.dyn.1962.3$t.sim <- as.Date(as.character(simu.dyn.1962.3$t.sim),format="%Y%m%d")

simu.dyn.1962.3.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`1962`,1))
colnames(simu.dyn.1962.3.Xmean) <- c("nsim","y")
simu.dyn.1962.3.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`1962`,1))
colnames(simu.dyn.1962.3.Tmean) <- c("nsim","y")


#### 1962 - alpha.cal=6 ##
load("../../Data/Winter/TG-France-Y1962m12d1_HW-animpsa_cal6_TG0meth1-860981.Rdat")
simu.dyn.1962.6 <-  adply(simu.dyn$l.X$`1962`, 1)
simu.dyn.1962.6$t.sim <- as.Date(as.character(simu.dyn.1962.6$t.sim),format="%Y%m%d")

simu.dyn.1962.6.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`1962`,1))
colnames(simu.dyn.1962.6.Xmean) <- c("nsim","y")
simu.dyn.1962.6.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`1962`,1))
colnames(simu.dyn.1962.6.Tmean) <- c("nsim","y")

#### plot ###
p1 <- plot_box(simu.dyn.2010.3.Xmean,dfval=simu.dyn.2010.3.Tmean,title="2010 - alpha.cal=3")
p2 <- plot_box(simu.dyn.2010.6.Xmean,dfval=simu.dyn.2010.6.Tmean,title="2010 - alpha.cal=6")
p3 <- plot_box(simu.dyn.1962.3.Xmean,dfval=simu.dyn.1962.3.Tmean,title="1962 - alpha.cal=3")
p4 <- plot_box(simu.dyn.1962.6.Xmean,dfval=simu.dyn.1962.6.Tmean,title="1962 - alpha.cal=6")
grid.arrange(p1, p2, p3, p4, ncol=2, nrow = 2)

