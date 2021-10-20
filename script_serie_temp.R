library(ncdf4)
library(ggplot2)
library(lubridate)
library(dplyr)

source("fct_plot.R")
source("fct_ncdf.R")
source("fct_submean.R")

###### Load data ######
path <- "~/Documents/These/Data/Winter/"

#Total precipitation data 
load("./data/era5_tp_DJFmean_fr.RData")
#Temperature at 2m
load("./data/era5_t2m_DJFmean_fr.RData")

# df_tp$date <- as.numeric(df_tp$date)
# df_t2m$date <- as.numeric(df_t2m$date)

#plot
plot_serie_temp(df_tp,ylegend = "Total precipitation [mm]")
plot_serie_temp(df_t2m,ylegend = "2m temperature [°C]")

plot_2y(df_t2m,df_tp,max(df_tp[,2])/max(df_t2m[,2]),title = "DJF mean - France",
        xlegend = "Time",ylegend1="2m temperature [°C]",ylegend2="Total precipiat ion [mm]")

##### Quantiles #####
name_qt <- "era5_t2m_DJF_sum5pctl_ymean.txt"
df_qt5 <- read.table(file = paste(path,name_qt,sep=""), header = FALSE)
df_qt5$V1 <- as.Date(df_qt5$V1,format="%Y-%m-%d")

plot_serie_temp(df_qt5,ylegend ="Ndays with T under 5 percentile")

##### Sous-moyenne (max sur 30 jours, 10 jours, 3 jours) #####
# nd <- 30  #input utilisateur
# df_t2m$date <- as.Date(df_t2m$date,format="%Y-%m-%d")
df_t2m$date <- format(df_t2m$date,"%Y")
colnames(df_t2m)[2] <- "temp"
# df_t2m$date <- as.numeric(df_t2m$date)
df_mindate <- rbind(cbind(df_t2m,"n_days"=90,"index"=as.Date(paste(df_t2m$date,"-01-01",sep=""),format="%Y-%m-%d")),
                    cbind(ndays_min(3,path),"n_days"=3,"index"=ndays_minidx(3,path)[,2]),
                    cbind(ndays_min(10,path),"n_days"=10,"index"=ndays_minidx(10,path)[,2]),
                    cbind(ndays_min(30,path),"n_days"=30,"index"=ndays_minidx(30,path)[,2]))
#df_mindate <- df_mindate[,c(1,3,2,4)]
df_mindate_3min <- select_minvalues(df_mindate,temp,n_days,3)
df_mindate_3min$n_days <- as.factor(df_mindate_3min$n_days)
# save(df_mindate,file="./data/t2m_DJF_minbyperiodduration")
plot_submean(df_mindate_3min)

#Sélection des dates du XXIe siècle
df_mindate_XXI <- df_mindate[df_mindate$date>1999,]
df_mindate_XXI_3min <- select_minvalues(df_mindate_XXI,temp,n_days,3)
#plot des dates


#### Corrélation GMST ####
load("./data/gmst.RData")
plot_serie_temp(data_gmst)
df_gmst_t2m <- as.data.frame(cbind(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2]))
colnames(df_gmst_t2m) <- c("GMST","t2m")
plot_serie_temp(df_gmst_t2m)
cor(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2])
cor(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2])

df_t2m <- df_t2m[t!=1963,]
