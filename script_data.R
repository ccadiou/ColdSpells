library(ncdf4)

source("fct_ncdf.R")

###### Load data ######
path <- "~/Documents/These/Data/Winter/"
##### Total precipitation data ####
name_tp <- "era5_tp_DJF_fr_mean"
nc_tp <- getNcFile(path,name_tp)  #load file
date <- c(1950:2021)                 #make time vector
df_tp <- data.frame(cbind(date,as.vector(ncvar_get(nc_tp,"tp"))))  #create data frame
save(df_tp,file="./data/tp_DJF_19502021_fr.RData")

#Temperature at 2m
name_t2m <- "era5_t2m_DJF_fr_mean"  
nc_t2m <- getNcFile(path,name_t2m)
df_t2m <- data.frame(cbind(date,as.vector(ncvar_get(nc_t2m,"t2m"))-273.15)) #data frame de la série temporelle et conversion kelvin <-  celsius
save(df_t2m,file="./data/t2m_DJF_19502021_fr.RData")

#Analogue data
df_ana <- read.table("../../Data/Winter/ana_z500_1_-20.30.30.70.txt",header=TRUE) #fichier d'analogues avec date/dates analogues/distance analogues/correlation analogues
df_ana[, cols <- grep("^date", names(df_ana))] <- lapply(df_ana[, cols <- grep("^date", names(df_ana))],   #conversion des colonnes dates en format date
                                                         function(col) as.Date(as.character(col), format = "%Y%m%d")) 
save(df_ana,file="./data/ana_z500_-20.30.30.70.RData")

