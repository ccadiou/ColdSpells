library(ggplot2)
library(dplyr)
library(plyr)
library(gridExtra)

source("fct_plot.R")

########## CHARGEMENT DES DONNÉES ##########
#importance sampling
path <- "~/Documents/These/Data/Winter/WEGE/"
fnames <- list.files(path,pattern="*906439*")
fnames[[5]] <- "TX-m12d1_UNCLE-min-animsa_cal3_TX0.5meth1-897290-1951-2021.Rdat"
fnames[[6]] <- "TX-m12d1_UNCLE-min-animsa_cal3_TX0meth1-940039-1951-2021.Rdat"

# name <- "TX-m12d1_UNCLE-min-animsa_cal3_TX0.5meth1-897290-1951-2021.Rdat"
ymin <- 1951
ymax <- 2020
mstart <- 12
dstart <- 01

#load(paste(path,fnames[[5]],sep=""))
par(mfcol=c(2,3),mar=c(3,3,2,2))
plot_SWG(path,fnames[[1]],ymin,ymax,mstart,dstart,substring(fnames[[1]],50,64))     #1851-1999 without 1963
plot_SWG(path,fnames[[3]],ymin,ymax,mstart,dstart,substring(fnames[[3]],50,64))     #1951-2021 without 1963
plot_SWG(path,fnames[[2]],ymin,ymax,mstart,dstart,substring(fnames[[2]],50,58))     #1951-1999
plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart,substring(fnames[[4]],50,58))     #1951-2021
plot_SWG(path,fnames[[5]],ymin,ymax,mstart,dstart,substring(fnames[[5]],50,58))
# plot_SWG(path,fnames[[5]],ymin,ymax,mstart,dstart,substring(fnames[[5]],50,58))


par(mfcol=c(1,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[3]],ymin,ymax,mstart,dstart,substring(fnames[[3]],50,64))
plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart,substring(fnames[[4]],50,58))

par(mfcol=c(1,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[6]],ymin,ymax,mstart,dstart,substring(fnames[[6]],48,56))
plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart,substring(fnames[[4]],50,58))

### Comparaison des distributions
load(paste(path,fnames[[4]],sep=""))
means_1951_2021 <- as.data.frame(cbind(unlist(simu.dyn$l.T.mean),unlist(lapply(simu.dyn$l.X.mean,mean))))

load(paste(path,fnames[[3]],sep=""))
means_1951_2021_w1963 <- as.data.frame(cbind(unlist(simu.dyn$l.T.mean),unlist(lapply(simu.dyn$l.X.mean,mean))))

plot_histo(means_1951_2021)
plot_histo(means_1951_2021_w1963)

#same histo
means <- as.data.frame(rbind(cbind(vals=means_1951_2021[,2],group="1951-2021"),cbind(vals=means_1951_2021_w1963[,2],group="without 1963")))
means$vals <- as.numeric(means$vals)
means$grp.mean<- ave(means$vals, means$group)
p <- ggplot(means, aes(x=vals, color=group)) + geom_histogram(position="dodge",alpha=0.5,fill='white')+
  theme_linedraw()+geom_vline(data=means, aes(xintercept=grp.mean, color=group),linetype="dashed")+
  theme(legend.position="top",legend.title=element_blank())+labs(x="Temperature (°C)",y="Count")
p
#test de Kolmogorov-Sirnov
ks.test(means_1951_2021[,2],means_1951_2021_w1963[,2])  # Test de Kolmogorov-Smirnov
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

