library(ncdf4)
library(ggplot2)
library(lubridate)

source("fct_plot.R")
source("fct_ncdf.R")
source("fct_submean.R")

###### Load data ######
path <- "~/Documents/These/Data/Winter/"

#Total precipitation data
t <- c(1950:2021)
name_tp <- "era5_tp_DJF_fr_mean"
nc_tp <- getNcFile(path,name_tp)
df_tp <- data.frame(cbind(t,as.vector(ncvar_get(nc_tp,"tp"))))
#Temperature at 2m
name_t2m <- "era5_t2m_DJF_fr_mean"  
nc_t2m <- getNcFile(path,name_t2m)
df_t2m <- data.frame(cbind(t,as.vector(ncvar_get(nc_t2m,"t2m"))-273.15)) #data frame de la série temporelle et conversion kelvin <-  celsius

#plot
plot_serie_temp(df_tp,ylegend = "Total precipitation [mm]")
plot_serie_temp(df_t2m,ylegend = "2m temperature [°C]")

plot_2y(df_t2m,df_tp,max(df_tp[,2])/max(df_t2m[,2]),title = "DJF mean - France",
        xlegend = "Time",ylegend1="2m temperature [°C]",ylegend2="Total precipiation [mm]")


##### Quantiles #####
name_qt <- "era5_t2m_DJF_sum5pctl_ymean.txt"
df_qt5 <- read.table(file = paste(path,name_qt,sep=""), header = FALSE)
df_qt5$V1 <- as.Date(df_qt5$V1,format="%Y-%m-%d")

plot_serie_temp(df_qt5,ylegend ="Ndays with T under 5 percentile")

##### Sous-moyenne (max sur 30 jours, 10 jours, 3 jours) #####
nd <- 30  #input utilisateur
df_mindate <- rbind(cbind(ndays_min(3,path),"n_days"=3,"index"=ndays_minidx(3,path)[,2]),
                    cbind(ndays_min(10,path),"n_days"=10,"index"=ndays_minidx(10,path)[,2]),
                    cbind(ndays_min(30,path),"n_days"=30,"index"=ndays_minidx(30,path)[,2]))
df_mindate <- df_mindate[,c(1,3,2,4)]
save(df_mindate,file="./data/t2m_DJF_minbyperiodduration")



