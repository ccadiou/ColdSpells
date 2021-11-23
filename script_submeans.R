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
load("./data/era5_tp_DJFmean_fr_anomaliepdg.RData")
#Temperature at 2m
load("./data/era5_t2m_DJFmean_fr.RData")
load("./data/era5_t2m_DJFmean_fr_anomaliepdg.RData")
#Snow fall
load("./data/era5_sf_DJFmean_fr.RData")
load("./data/era5_sf_DJFmean_fr_anomaliepdg.RData")

# View(df_tp)
#_________________________________________________________________________________________________________
###### Sous-moyenne (max sur 30 jours, 10 jours, 3 jours) ######
## Sélection de la variable
var <- "sf"
n <- 10
if (var=="t2m"){df_var <- df_t2m;df_var_anomalie <- df_t2m_anomalie;var_label <- "Temperature (°C)";extreme <- "min"}
if (var=="tp"){df_var <- df_tp;df_var_anomalie <- df_tp_anomalie;var_label <- "Total precipitation (mm)";extreme <- "max"}
if (var=="sf"){df_var <- df_sf;df_var_anomalie <- df_sf_anomalie;var_label <- "Total snowfall (mm)";extreme <- "max"}
df_var$date <- format(df_var$date,"%Y")
colnames(df_var)[2] <- "var"
colnames(df_var_anomalie)[2] <- "var"
df_var_anomalie$date <- format(as.Date(df_var_anomalie$date,format="%Y-%m-%d"),"%Y")
# df_var$date <- as.numeric(df_var$date)


### Sur la période 1950-2021 ###
df_extdate <- rbind(cbind(ndays_min(30,path,var,extreme),"n_days"=30),
                    cbind(ndays_min(10,path,var,extreme),"n_days"=10),
                    cbind(ndays_min(3,path,var,extreme),"n_days"=3),
                    cbind(df_var,"n_days"=90))
df_extdate_n <- select_extremes(df_extdate,n_days,extreme,n)
df_extdate_n$n_days <- factor(df_extdate_n$n_days, levels=c(3,10,30,90))

#plot
# df_extdate_n$date <- factor(df_extdate_n$date,levels=levels(as.factor(df_extdate$date)))
df_extdate_n$date <- as.numeric(as.character(df_extdate_n$date))
# plot_submean(df_extdate_n,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")
df_extdate_n$n_days <- as.numeric(as.character(df_extdate_n$n_days))
write.table(df_extdate_n,paste("./data/",var,"_DJF_minbyrange.txt",sep=""),sep="\t",row.names=FALSE)


plot_submean_bubble(df_extdate_n,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")
#multiplot t2m et tp
# p_t2m <- plot_submean_bubble(df_extdate_n,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")
# p_sf <- plot_submean_bubble(df_extdate_n,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")
grid.arrange(p_sf, p_t2m, ncol=1, nrow = 2)

#_____________________________________________________________________________________________________________________
##### Sous-moyennes à parti de l'anomalie par point de grille #####
df_extdate <- rbind(cbind(ndays_min(30,path,var,extreme,anomalie=TRUE),"n_days"=30),
                    cbind(ndays_min(10,path,var,extreme,anomalie=TRUE),"n_days"=10),
                    cbind(ndays_min(3,path,var,extreme,anomalie=TRUE),"n_days"=3),
                    cbind(df_var_anomalie,"n_days"=90))
df_extdate_n <- select_extremes(df_extdate,n_days,extreme,10)
df_extdate_n$n_days <- factor(df_extdate_n$n_days, levels=c(3,10,30,90))
df_extdate_n$n_days <- as.numeric(as.character(df_extdate_n$n_days))
df_extdate_n$date <- as.numeric(as.character(df_extdate_n$date))

#plot
plot_submean_bubble(df_extdate_n,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")







