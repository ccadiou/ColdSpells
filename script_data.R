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

#### Temperature ####
# yearmean of DJF
df_t2m <- read.table("../../Data/Winter/era5_t2m_DJFmean_fr.txt",header=FALSE)
colnames(df_t2m) <- c("date","t2m")
#df_t2m$date <- format(as.Date(df_t2m$date,format="%Y-%m-%d"),"%Y")
df_t2m$date <- as.Date(df_t2m$date,format="%Y-%m-%d")
df_t2m$t2m <- df_t2m$t2m -273.15
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


