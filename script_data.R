library(ncdf4)
library(reshape2)

source("fct_ncdf.R")

###### set directory ######
path <- "~/Documents/These/Code/Winter/"
setwd(path)

#### Analogue data ####
df_ana <- read.table("../../Data/Winter/ana_z500_1_-20.30.30.70.txt",header=TRUE) #fichier d'analogues avec date/dates analogues/distance analogues/correlation analogues
df_ana[, cols <- grep("^date", names(df_ana))] <- lapply(df_ana[, cols <- grep("^date", names(df_ana))],   #conversion des colonnes dates en format date
                                                         function(col) as.Date(as.character(col), format = "%Y%m%d")) 
save(df_ana,file="./data/ana_z500_-20.30.30.70.RData")
#Filtration entre deux dates
df_ana_20102011 <- df_ana[df_ana$date >= "2010-12-21" & df_ana$date <= "2011-03-21",]
save(df_ana_20102011,file="./data/df_ana_20102011.txt")
#dates
dates <- as.data.frame(df_ana$date)
colnames(dates) <- c("date")
save(dates,file="./data/dates_19500101_20210831.RData")

##### Total precipitation data ####
# yearmean of DJF
df_tp <- read.table("../../Data/Winter/era5_tp_DJFmean_fr.txt",header=FALSE)
colnames(df_tp) <- c("date","tp")
df_tp$date <- as.Date(df_tp$date,format="%Y-%m-%d")
#df_tp$date <- format(as.Date(df_tp$date,format="%Y-%m-%d"),"%Y")
save(df_tp,file="./data/era5_tp_DJFmean_fr.RData")
#daily values
df_tp_daily <- read.table("../../Data/Winter/era5_tp_daily_fr.txt",header=FALSE)
colnames(df_tp_daily) <- c("date","tp")
df_tp_daily$date <- as.Date(df_tp_daily$date,format="%Y-%m-%d")
save(df_tp_daily,file="./data/era5_tp_daily_fr.RData")
#anomalie par point de grille
df_tp_anomalie <- read.table("../../Data/Winter/era5_tp_DJFmean_fr_anomaliepdg.txt",header=FALSE)
colnames(df_tp_anomalie) <- c("date","tp")
df_tp_anomalie$date <- as.Date(df_tp_anomalie$date,format="%Y-%m-%d")
save(df_tp_anomalie,file="./data/era5_tp_DJFmean_fr_anomaliepdg.RData")

#### Temperature EOBS ####
# yearmean of DJF
df_tg <- read.table("../../Data/Winter/submeans_eobs/eobs_tg_DJFmean_fr.txt",header=FALSE)
colnames(df_tg) <- c("date","tg")
#df_tg$date <- format(as.Date(df_tg$date,format="%Y-%m-%d"),"%Y")
df_tg$date <- as.Date(df_tg$date,format="%Y-%m-%d")
df_tg <- df_tg[df_tg$date>"1950-11-01",]
save(df_tg,file="./data/eobs_tg_DJFmean_fr.RData")
#daily values
df_tg_daily <- read.table("../../Data/Winter/eobs_tg_daily_fr.txt",header=FALSE)
colnames(df_tg_daily) <- c("date","tg")
df_tg_daily$date <- as.Date(df_tg_daily$date,format="%Y-%m-%d")
save(df_tg_daily,file="./data/eobs_tg_daily_fr.RData")


#### Temperature ERA5 ####
# yearmean of DJF
df_t2m <- read.table("../../Data/Winter/era5_t2m_DJFmean_fr.txt",header=FALSE)
colnames(df_t2m) <- c("date","t2m")
#df_t2m$date <- format(as.Date(df_t2m$date,format="%Y-%m-%d"),"%Y")
df_t2m$date <- as.Date(df_t2m$date,format="%Y-%m-%d")
df_t2m$t2m <- df_t2m$t2m -273.15
df_t2m <- df_t2m[df_t2m$date>"1950-11-01",]
save(df_t2m,file="./data/era5_t2m_DJFmean_fr.RData")
#daily values
df_t2m_daily <- read.table("../../Data/Winter/era5_t2m_daily_fr.txt",header=FALSE)
colnames(df_t2m_daily) <- c("date","t2m")
df_t2m_daily$date <- as.Date(df_t2m_daily$date,format="%Y-%m-%d")
df_t2m_daily$t2m <- df_t2m_daily$t2m -273.15
save(df_t2m_daily,file="./data/era5_t2m_daily_fr.RData")
#anomalie par point de grille
df_t2m_anomalie <- read.table("../../Data/Winter/era5_t2m_DJFmean_fr_anomaliepdg.txt",header=FALSE)
colnames(df_t2m_anomalie) <- c("date","t2m")
df_t2m_anomalie$date <- as.Date(df_t2m_anomalie$date,format="%Y-%m-%d")
save(df_t2m_anomalie,file="./data/era5_t2m_DJFmean_fr_anomaliepdg.RData")
#sous-moyennes sur 3, 10, 30et 90 jours
for (n in c(3,10,30)) {
  name <- paste("df_t2m_",n,"submean",sep="")
  assign(name, read.table(paste("../../Data/Winter/era5_t2m_DJF_",n,"min.txt",sep="")))
}
colnames(df_t2m_3submean) <- c("date","t2m")
colnames(df_t2m_10submean) <- c("date","t2m")
colnames(df_t2m_30submean) <- c("date","t2m")
df_t2m_3submean$date <- as.Date(df_t2m_3submean$date,format="%Y-%m-%d")
df_t2m_10submean$date <- as.Date(df_t2m_10submean$date,format="%Y-%m-%d")
df_t2m_30submean$date <- as.Date(df_t2m_30submean$date,format="%Y-%m-%d")
df_t2m_3submean$t2m <- df_t2m_3submean$t2m-273.15
df_t2m_10submean$t2m <- df_t2m_10submean$t2m-273.15
df_t2m_30submean$t2m <- df_t2m_30submean$t2m-273.15
df_t2m_3submean <- df_t2m_3submean[df_t2m_3submean$date>"1950-11-01",]
df_t2m_10submean <- df_t2m_10submean[df_t2m_10submean$date>"1950-11-01",]
df_t2m_30submean <- df_t2m_30submean[df_t2m_30submean$date>"1950-11-01",]
save(df_t2m_3submean,file="./data/era5_t2m_DJF3mean.RData")
save(df_t2m_10submean,file="./data/era5_t2m_DJF10mean.RData")
save(df_t2m_30submean,file="./data/era5_t2m_DJF30mean.RData")

## Calcul des moyennes glissantes
df_t2m_90mean <- df_t2m_daily
df_t2m_90mean$t2m <- c(rep(NA,44),running_mean(df_t2m_daily$t2m,90),rep(NA,45))
save(df_t2m_90mean,file="./data/era5_t2m_90_mean.RData")

df_t2m_60mean <- df_t2m_daily
df_t2m_60mean$t2m <- c(rep(NA,29),running_mean(df_t2m_daily$t2m,60),rep(NA,30))
save(df_t2m_60mean,file="./data/era5_t2m_60_mean.RData")

df_t2m_30mean <- df_t2m_daily
df_t2m_30mean$t2m <- c(rep(NA,14),running_mean(df_t2m_daily$t2m,30),rep(NA,15))
save(df_t2m_30mean,file="./data/era5_t2m_30_mean.RData")

df_t2m_10_mean <- df_t2m_daily
df_t2m_10_mean$t2m <- c(rep(NA,4),running_mean(df_t2m_daily$t2m,10),rep(NA,5))
save(df_t2m_10_mean,file="./data/era5_t2m_10_mean.RData")

df_t2m_3_mean <- df_t2m_daily
df_t2m_3_mean$t2m <- c(rep(NA,1),running_mean(df_t2m_daily$t2m,3),rep(NA,1))
save(df_t2m_3_mean,file="./data/era5_t2m_10_mean.RData")

#### Snowfall ####
# yearmean of DJF
df_sf <- read.table("../../Data/Winter/era5_sf_DJFmean_fr.txt",header=FALSE)
colnames(df_sf) <- c("date","sf")
#df_sf$date <- format(as.Date(df_sf$date,format="%Y-%m-%d"),"%Y")
df_sf$date <- as.Date(df_sf$date,format="%Y-%m-%d")
df_sf$sf <- df_sf$sf*1000             # convert from m to mm
save(df_sf,file="./data/era5_sf_DJFmean_fr.RData")
#daily values
df_sf_daily <- read.table("../../Data/Winter/era5_sf_DJF_daily_fr.txt",header=FALSE)
colnames(df_sf_daily) <- c("date","sf")
df_sf_daily$date <- as.Date(df_sf_daily$date,format="%Y-%m-%d")
df_sf_daily$sf <- df_sf_daily$sf*1000
save(df_sf_daily,file="./data/era5_sf_daily_fr.RData")
#anomalie par point de grille
df_sf_anomalie <- read.table("../../Data/Winter/era5_sf_DJFmean_fr_anomaliepdg.txt",header=FALSE)
colnames(df_sf_anomalie) <- c("date","sf")
df_sf_anomalie$date <- as.Date(df_sf_anomalie$date,format="%Y-%m-%d")
df_sf_anomalie$sf <- df_sf_anomalie$sf*1000
save(df_sf_anomalie,file="./data/era5_sf_DJFmean_fr_anomaliepdg.RData")

#### GMST ####
data_gmst <- read.table("../../Data/Winter/GMST.dat",fill=TRUE)
#yearly
data_gmst_yearly <- data_gmst[seq(1, nrow(data_gmst), 2),c(1,14)]    # pour chaque année sélectionne la première ligne (température) et les première (année) et dernière (moyenne annuelle) colonnes 
colnames(data_gmst_yearly) <- c("date","temp")
save(data_gmst_yearly,file="./data/gmst_yearly.RData")
#monthly
data_gmst_monthly <- data_gmst[seq(1, nrow(data_gmst), 2),c(1:13)]    # pour chaque année sélectionne la première ligne (température) et les première (année) et dernière (moyenne annuelle) colonnes 
colnames(data_gmst_monthly) <- c("date",1:12)
data_gmst_monthly <- melt(data_gmst_monthly, id.vars=c("date"))       # wide to long, mets les mois dans une colonne
colnames(data_gmst_monthly) <- c("year","month","temp")
data_gmst_monthly$month <- as.numeric(data_gmst_monthly$month)
save(data_gmst_monthly,file="./data/gmst_monthly.RData")


