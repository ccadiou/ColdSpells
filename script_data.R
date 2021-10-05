library(ncdf4)

source("fct_ncdf.R")

###### set directory ######
path <- "~/Documents/These/Code/Winter/"
setwd("~/Documents/These/Code/Winter/")

#### Analogue data ###
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
save(dates,file="./data/dates_19500101_20210831")

##### Total precipitation data ####
# yearmean of DJF
name_tp <- "../../era5_tp_DJF_fr_mean"
nc_tp <- getNcFile(path,name_tp)  #load file
date <- c(1950:2021)                #make time vector
df_tp <- data.frame(cbind(date,as.vector(ncvar_get(nc_tp,"tp"))))  #create data frame
save(df_tp,file="./data/tp_DJF_19502021_fr.RData")
#daily values
name_tp_daily <- "../../era5_tp_daily_fr"
nc_tp_daily <- getNcFile(path,name_tp_daily)  #load file
df_tp_daily <- data.frame(cbind(date=dates,as.vector(ncvar_get(nc_tp_daily,"tp"))))  #create data frame
save(df_tp_daily,file="./data/tp_daily_fr.RData")

#### Temperature ####
# yearmean of DJF
name_t2m <- "../../era5_t2m_DJF_fr_mean"  
nc_t2m <- getNcFile(path,name_t2m)
df_t2m <- data.frame(cbind(date,as.vector(ncvar_get(nc_t2m,"t2m"))-273.15)) #data frame de la série temporelle et conversion kelvin <-  celsius
save(df_t2m,file="./data/t2m_DJF_19502021_fr.RData")
#daily values
name_t2m_daily <- "../../era5_t2m_daily_fr"
nc_t2m_daily <- getNcFile(path,name_t2m_daily)  #load file
df_t2m_daily <- data.frame(cbind(date=dates,as.vector(ncvar_get(nc_t2m_daily,"t2m"))-273.15))  #create data frame
colnames(df_t2m_daily)[[2]] <- "temp"
save(df_t2m_daily,file="./data/t2m_daily_fr.RData")

#### GMST ####
##### Comparaison avec GMST #####
data_gmst <- read.table("../../Data/Winter/GMST.dat",fill=TRUE)
data_gmst <- data_gmst[seq(1, nrow(data_gmst), 2),c(1,14)]
colnames(data_gmst) <- c("date","temp")
save(data_gmst,file="./data/gmst.RData")

