library(ggplot2)
library(dplyr)
# library(plyr)
# library(gridExtra)

setwd("~/Documents/Code/Winter")
source("fct_plot.R")
source("fct_AnaWG.R")

########## CHARGEMENT DES DONNÉES ##########
#importance sampling
path <- "~/Documents/Data/Winter/WEGE/"
fnames <- list.files(path,pattern="*906439*")
fnames[[5]] <- "TX-m12d1_UNCLE-min-animsa_cal3_TX0.5meth1-897290-1951-2021.Rdat"
fnames[[6]] <- "TX-m12d1_UNCLE-min-animsa_cal3_TX0meth1-940039-1951-2021.Rdat"
fnames[[7]] <- "TX-m&_UNCLE-min-animsa_cal3_TX0.5meth1-947773-1972-2021.Rdat"
fnames[[8]] <- "TX-m1d1_l30_UNCLE-min-animsa_cal3_TX0.5meth1-950187-30d_1951-2021.Rdat"
fnames[[9]] <- "TX-m1d1_l30_UNCLE-min-animsa_cal3_TX0meth1-952730-30d_1951-2021.Rdat"
# "TX-m12d1L90_UNCLE-min-animsa_cal3_TX0.5meth1-971442-1951-2021.Rdat" 

############
# Plot des série temporelles obs - sta - dyn de 1951 à 2020
############

# name <- "TX-m12d1_UNCLE-min-animsa_cal3_TX0.5meth1-897290-1951-2021.Rdat"
ymin <- 1951
ymax <- 2020
mstart <- get_param(fnames[[1]],1)
dstart <- get_param(fnames[[1]],2)
lsim <- 90

dev.off()
#load(paste(path,fnames[[5]],sep=""))
par(mfcol=c(2,3),mar=c(3,3,2,2))
plot_SWG(path,fnames[[1]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[1]],50,64))     #1851-1999 without 1963
plot_SWG(path,fnames[[3]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[3]],50,64))     #1951-2021 without 1963
plot_SWG(path,fnames[[2]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[2]],50,58))     #1951-1999
plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[4]],50,58))     #1951-2021
plot_SWG(path,fnames[[5]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[5]],50,58))     #1951-2021 autre simulation
plot_SWG(path,fnames[[6]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[6]],48,56))     #1951-2021 sans importance sampling
plot_SWG(path,fnames[[7]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[7]],48,56))     #1972-2021

gsub(".*m([0-9]+)d([0-9]+)L([0-9]+).*cal([0-9].?[0-9]?)_TX([0-9].?[0-9]?)meth([0-9])-([0-9]*).*","\\7",
     "TX-m12d1L90_UNCLE-min-animsa_cal3_TX0.5meth1-971442-1951-2021.Rdat")
get_param(fnames[[1]],7)


plot_SWG(path,fnames[[5]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[5]],50,58))

#COmparaison avec et sans 1963
par(mfcol=c(1,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[3]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[3]],50,64))     # Sans 1963
plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[4]],50,58))     # Avec 1963

#Comparaison et sans importance sampling
par(mfcol=c(1,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[6]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[6]],48,56),-2,7.5)     #1951-2021 sans importance sampling
plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[4]],50,58),-2,7.5)     #1951-2021 avec Importance sampling

#Comparaison 1950-1999 et 1972-2021
par(mfcol=c(1,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[2]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[2]],50,58))     #1950-1999
plot_SWG(path,fnames[[7]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[7]],50,58))     #1972-2021

## Calcul des 2à meilleures analogues sur la nouvelle période (sans NA)
path <- "~/Documents/Thebse/Data/Winter/WEGE/"
fnames <- list.files(path,pattern="*954601")
yinf <- -4
ysup <- 8
par(mfcol=c(2,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[1]],ymin,ymax,mstart,dstart,substring(fnames[[1]],54,68),yinf,ysup)     #1851-1999 without 1963
plot_SWG(path,fnames[[2]],ymin,ymax,mstart,dstart,substring(fnames[[2]],54,62),yinf,ysup)     #1951-1999
plot_SWG(path,fnames[[3]],ymin,ymax,mstart,dstart,substring(fnames[[3]],54,68),yinf,ysup)     #1951-2021 without 1963
plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart,substring(fnames[[4]],54,62),yinf,ysup)     #1951-2021

##### SUR 30 jours (Janvier) ####
dev.off()
mstart <- 01
ymax <- 2021
yinf <- -4
ysup <- 8
plot_SWG(path,fnames[[8]],ymin,ymax,mstart,dstart,substring(fnames[[8]],57,65))     #Simulations des janvier (30 jours)
plot_SWG(path,fnames[[9]],ymin,ymax,mstart,dstart,substring(fnames[[9]],57,65))     #Simulations des janvier (30 jours)

# Comparaison avec et sans importance sampling pour 30 j
par(mfcol=c(1,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[9]],ymin,ymax,mstart,dstart,substring(fnames[[9]],55,63),yinf,ysup)     #Simulations des janvier (30 jours)
plot_SWG(path,fnames[[8]],ymin,ymax,mstart,dstart,substring(fnames[[8]],57,65),yinf,ysup)     #Simulations des janvier (30 jours)


########
### Comparaison des distributions avec et sans 1963
########
#### Avec et sans 1963 ####
load(paste(path,fnames[[4]],sep=""))
means_1951_2021 <- as.data.frame(cbind(unlist(simu.dyn$l.T.mean),unlist(lapply(simu.dyn$l.X.mean,mean))))
load(paste(path,fnames[[3]],sep=""))
means_1951_2021_w1963 <- as.data.frame(cbind(unlist(simu.dyn$l.T.mean),unlist(lapply(simu.dyn$l.X.mean,mean))))
#test de Kolmogorov-Sirnov
k1 <- ks.test(means_1951_2021[,2],means_1951_2021_w1963[,2])  # Test de Kolmogorov-Smirnov

#plot_histo(means_1951_2021)
#plot_histo(means_1951_2021_w1963)
means <- as.data.frame(rbind(cbind(vals=means_1951_2021[,2],group="1951-2021"),cbind(vals=means_1951_2021_w1963[,2],group="without 1963")))
means$vals <- as.numeric(means$vals)
means$grp.mean<- ave(means$vals, means$group)

#### Avec analogues de 1950-1999 et 1972-2021 ####
load(paste(path,fnames[[2]],sep=""))
means_1951_1999 <- as.data.frame(cbind(unlist(simu.dyn$l.T.mean),unlist(lapply(simu.dyn$l.X.mean,mean))))
load(paste(path,fnames[[7]],sep=""))
means_1972_2021 <- as.data.frame(cbind(unlist(simu.dyn$l.T.mean),unlist(lapply(simu.dyn$l.X.mean,mean))))
#test de Kolmogorov-Sirnov
k2 <- ks.test(means_1951_1999[,2],means_1972_2021[,2])  # Test de Kolmogorov-Smirnov

means_1972_2021 <- as.data.frame(cbind(unlist(simu.dyn$l.T.mean),unlist(lapply(simu.dyn$l.X.mean,mean))))
means <- as.data.frame(rbind(cbind(vals=means_1951_1999[,2],group="1951-1999"),cbind(vals=means_1972_2021[,2],group="1972-2021")))
means$vals <- as.numeric(means$vals)
means$grp.mean<- ave(means$vals, means$group)

#### Histogrammes ####
p <- ggplot(means, aes(x=vals, color=group)) + geom_histogram(position="dodge",alpha=0.5,fill='white')+
  theme_linedraw()+geom_vline(data=means, aes(xintercept=grp.mean, color=group),linetype="dashed")+
  theme(legend.position="top",legend.title=element_blank())+labs(x="Temperature (°C)",y="Count")+
  annotate("text", x = -0.2, y = 8,label = "D = 0.37143\np-value = 0.0001089")
p
#test de Kolmogorov-Sirnov

##### SUR 30 jours (Janvier) ####
dev.off()
mstart <- 01
ymax <- 2021
yinf <- -4
ysup <- 8
lsim <- 30
plot_SWG(path,fnames[[8]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[8]],55,63))     #Simulations des janvier (30 jours)
plot_SWG(path,fnames[[9]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[9]],55,63))     #Simulations des janvier (30 jours)

# Comparaison avec et sans importance sampling pour 30 j
par(mfcol=c(1,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[9]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[9]],55,63),yinf,ysup)     #Simulations des janvier (30 jours)
plot_SWG(path,fnames[[8]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[8]],57,65),yinf,ysup)     #Simulations des janvier (30 jours)



#########
## Calcul des 20 meilleures analogues sur la nouvelle période (sans NA)
#########
path <- "~/Documents/Data/Winter/WEGE/"
fnames <- list.files(path,pattern="*954601")
ymin <- 1951
ymax <- 2020
mstart <- 12
dstart <- 01
yinf <- -2
ysup <- 7
lsim <- 90
par(mfcol=c(2,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[1]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[1]],54,68),yinf,ysup)     #1951-1999 without 1963
plot_SWG(path,fnames[[2]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[2]],54,62),yinf,ysup)     #1951-1999
plot_SWG(path,fnames[[3]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[3]],54,68),yinf,ysup)     #1951-2021 without 1963
plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart,lsim,substring(fnames[[4]],54,62),yinf,ysup)     #1972-2021


##########
# Calcul de la carte moyenne des simulations
##########
load(paste(path,fnames[[4]],sep="")) # 1951-2021
load(paste(path,fnames[[3]],sep="")) # 1951-2021 sans 1963
load(paste(path,fnames[[2]],sep="")) # 1951-1999
load(paste(path,fnames[[7]],sep="")) # 1972-2021
load(paste(path,fnames[[6]],sep="")) # 1951-2021 sans ImpSam

days_sim <- as.data.frame(do.call(rbind,simu.dyn$l.X$`2006`))
days_sim[,1] <- as.Date(as.character(days_sim[,1]),format="%Y%m%d")
# days_sim <- days_sim[,c(2,1)]

p <- ggplot(days_sim,aes(x=days_sim[,1]))
p + geom_histogram(color="black", fill="snow3",bins=71) +
      theme_linedraw()+
      theme(legend.position = "None",legend.title = element_blank())

## anas of all years ####
day_sim_all <- as.data.frame(do.call(rbind,lapply(simu.dyn$l.X, function(y) do.call(rbind,y))))
day_sim_all[,1] <- as.Date(as.character(day_sim_all[,1]),format="%Y%m%d")
# days_sim <- days_sim[,c(2,1)]
day_sim_all$y.sim <- as.numeric(format(day_sim_all$t.sim,"%Y"))

p <- ggplot(day_sim_all,aes(x=day_sim_all[,3]))
p + geom_histogram(color="black", fill="snow3",bins=71) +
  theme_linedraw()+
  theme(legend.position = "None",legend.title = element_blank())

#comp entre les deux périodes 1950-1999 et 1972-2021
# years_1951_1999 <- as.numeric(day_sim_all[,3])
# years_1972_2021 <- as.numeric(day_sim_all[,3])
years_1951_1999 <- as.numeric(days_sim[,1])
years_1972_2021 <- as.numeric(days_sim[,1])
years <- as.data.frame(rbind(cbind(vals=years_1951_1999,group="1951-1999"),cbind(vals=years_1972_2021,group="1972-2021")))
years$vals <- as.numeric(years$vals)
p <- ggplot(years, aes(x=vals, color=group,fill=group)) + geom_histogram(position = "identity",alpha=0.2,bins=72)+
  theme_linedraw()+ theme(legend.position="top",legend.title=element_blank())+labs(x="Year",y="Number of analogs")
p

####### simulations avec exclusion de l'année en cours #########
path <- "~/Documents/Data/Winter/WEGE/"
fnames <- list.files(path,pattern="*971442*")
load(paste(path,fnames[[1]],sep=""))

yymin <- as.numeric(args[[7]])
yymax <- as.numeric(args[[8]])
mstart <- as.numeric(args[[4]])
dstart <- as.numeric(args[[5]])
lsim <- as.numeric(args[[3]])
yinf <- -3    # bornes inférieure et supérieures de l'axe y
ysup <- 7

par(mfcol=c(1,2),mar=c(3,3,2,2))
plot_SWG(path,fnames[[1]],ymin,ymax,mstart,dstart,lsim,get_param(fnames[[1]],8),yinf,ysup)     #1951-2021
plot_SWG(path,fnames[[2]],ymin,ymax,mstart,dstart,lsim,get_param(fnames[[2]],8),yinf,ysup)     #1951-2021 without 1963


load("~/Documents/Data/Winter/WEGE/TX-m12d1L90_UNCLE-min-animsa_cal3_TX0.5meth4--1960-1962_wcyear.Rdat")

#histogram de répartition temporelle des analogues pour les simulatiosn d'unae année
days_sim <- as.data.frame(do.call(rbind,simu.dyn$l.X$`1961`))
days_sim[,1] <- as.Date(as.character(days_sim[,1]),format="%Y%m%d")
# days_sim <- days_sim[,c(2,1)]

p <- ggplot(days_sim,aes(x=days_sim[,1]))
p + geom_histogram(color="black", fill="snow3",bins=71) +
  theme_linedraw()+
  theme(legend.position = "None",legend.title = element_blank())

# histogramme des dates d'analogues dans les simulations en affichant l'année de l'hiver et en retirnat les premiers jours des simulations
load(paste(path,fnames[[1]],sep=""))
load(paste(path,fnames[[2]],sep=""))

df_sim <- as.data.frame(do.call(rbind,lapply(simu.dyn$l.X$`1964`,function(sim) cbind(sim,c(1:nrow(sim))))))
df_sim$t.sim <- as.Date(as.character(df_sim$t.sim),format="%Y%m%d")
df_sim$year <- as.numeric(format(df_sim$t.sim,"%Y"))
df_sim$year[format(df_sim$t.sim,"%m")=="12"] <- df_sim$year[format(df_sim$t.sim,"%m")=="12"]+1
df_sim <- df_sim[df_sim$V3 != 1,] #enlève les premiers jours des simulations (01-12-yy)

p <- ggplot(df_sim,aes(x=df_sim[,4]))
p + geom_histogram(color="black", fill="snow3",bins=71) +
  theme_linedraw()+
  theme(legend.position = "None",legend.title = element_blank())

## position de l'analogue dans la simulation en fonction de la date
# load(paste(path,fnames[[1]],sep=""))
ggplot(df_sim,aes(x=year,y=V3))+geom_point()
df_sim_year <- df_sim[df_sim$year==1964,]
ggplot(df_sim_year,aes(x=V3))+ geom_histogram(color="black", fill="snow3",bins=61)

# count_date_year <- aggregate(t.sim ~ V3, data=df_sim_year,FUN=length)
count_date_year <- df_sim_year %>% count(V3)


############
# Fréquence de chaque jour analogue
###########
x <- as.Date(1:26176, origin = "1950-01-01")
x <- cut(x, breaks = "day")
dates_frq <- cbind(as.data.frame(x),0)

## Tabulate
tab <- table(cut(days_sim[,1], 'day'))
## Format
dates_frq_part <- data.frame(Date=format(as.Date(names(tab)),"%Y-%m-%d"),
           Frequency=as.vector(tab))
dates_frq[dates_frq$x %in% dates_frq_part$Date,2] <- dates_frq_part[,2]

#write netcdf file
library(ncdf4)
ncname <- "~/Documents/These/Data/Winter/era5_t2m_daily_fr.nc"

# open a netCDF file
ncin <- nc_open(ncname)
time <- ncvar_get(ncin, "time")
tunits <-  ncatt_get(ncin, "time","units")
lon <- ncvar_get(ncin, "lon")
lat <- ncvar_get(ncin, "lat", verbose = F)
nc_close(ncin)

filename="nb_ana_1963.nc"
xvals <- seq(-20, 30, 1)
yvals <- seq(30.5, 70, 1) 
nx <- length(xvals)
ny <- length(yvals)
lon2 <- ncdim_def("longitude", "degrees_east", xvals)
lat2 <- ncdim_def("latitude", "degrees_north", yvals)

time2 <- ncdim_def("Time",tunits$value, time)
var_temp <- ncvar_def("count", "",list(lon2,lat2,time2), NA) 

data <- rep(dates_frq[,2],each=nx*ny)

ncnew <- nc_create(filename, list(var_temp))
ncvar_put(ncnew, var_temp, data, start=c(1,1,1), count=c(nx,ny,time2$len))

nc_close(ncnew)

# Verification
library(rasterVis)
out <- raster("time.nc")
levelplot(out, margin=FALSE)


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


########### FIN DU SCRIPT #####################


