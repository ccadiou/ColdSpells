library(ncdf4)
library(ggplot2)
library(lubridate)

source("fct_plot.R")
source("fct_ncdf.R")

###### Load data ######
path <- "~/Documents/These/Data/Winter/"
#Total precipitation data
name_tp <- "era5_tp_DJF_fr_mean"
nc_tp <- getNcFile(path,name_tp)

#Temperature at 2m
name_t2m <- "era5_t2m_DJF_fr_mean"  
nc_t2m <- getNcFile(path,name_t2m)

#Time
t <- c(1950:2021)

df_t2m <- data.frame(cbind(t,as.vector(ncvar_get(nc_t2m,"t2m"))-273.15)) #data frame de la série temporelle et conversion kelvin <-  celsius
df_tp <- data.frame(cbind(t,as.vector(ncvar_get(nc_tp,"tp"))))

#plot
plot_serie_temp(df_t2m)
plot_serie_temp(df_tp)

plot_2y(df_t2m,df_tp,max(df_tp[,2])/max(df_t2m[,2]),title = "DJF mean - France",
        xlegend = "Time",ylegend1="2m temperature [°C]",ylegend2="Total precipiation [mm]")
