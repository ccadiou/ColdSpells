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



#### Corrélation GMST ####
load("./data/gmst.RData")
plot_serie_temp(data_gmst)
df_gmst_t2m <- as.data.frame(cbind(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2]))
colnames(df_gmst_t2m) <- c("GMST","t2m")
plot_serie_temp(df_gmst_t2m)
cor(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2])
cor(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2])

df_t2m <- df_t2m[t!=1963,]
