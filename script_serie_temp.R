library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)  # grid.arrange function for pultiplot
library(ggpubr)     # ggarange function for multiplot


source("fct_plot.R")
source("fct_ncdf.R")
source("fct_submean.R")

###### Load data ######
path <- "~/Documents/These/Data/Winter/"

#Temperature at 2m
load("./data/era5_t2m_DJFmean_fr.RData")
#Total precipitation data 
load("./data/era5_tp_DJFmean_fr.RData")

# df_tp$date <- as.numeric(df_tp$date)
# df_t2m$date <- as.numeric(df_t2m$date)

#plot
plot_serie_temp(df_tp,ylegend = "Total precipitation (mm)")
plot_serie_temp(df_t2m,ylegend = "2m temperature (°C)",trend=TRUE,date_low=as.Date("1949-01-01"),date_high=as.Date("2021-03-01"),n.breaks=5)
#calcul du coefficient directuer de la regréssion
# df_t2m_year <- df_t2m
# df_t2m_year$date <- as.numeric(format(df_t2m$date,"%Y"))
# lm(df_t2m_year$t2m~df_t2m_year$date)

plot_2y(df_t2m,df_tp,max(df_tp[,2])/max(df_t2m[,2]),title = "DJF mean - France",
        xlegend = "Time",ylegend1="2m temperature (°C)",ylegend2="Total precipiation (mm)")

##### Sous-moyennes #####
load("./data/era5_t2m_DJF3mean.RData")
load("./data/era5_t2m_DJF10mean.RData")
load("./data/era5_t2m_DJF30mean.RData")
p3 <- plot_serie_temp(df_t2m_3submean,ylegend = "2m temperature (°C)")
p10 <- plot_serie_temp(df_t2m_10submean,ylegend = "2m temperature (°C)")
p30 <- plot_serie_temp(df_t2m_30submean,ylegend = "2m temperature (°C)")
p90 <- plot_serie_temp(df_t2m,ylegend = "2m temperature (°C)")
# grid.arrange(p90,p30,p10,p3, ncol=2, nrow = 2)
ggarrange(p90,p30,p10,p3, ncol = 2, nrow=2, labels = c("a)","b)","c)","d)"))

#Histogramme
plot_histo(df_t2m,xlegend="Temperature (C°)",y.n.breaks=11,y.expand=c(0,0),y.limits=c(0,11))

t2m_sans1963 <- df_t2m[format(df_t2m$date,"%Y")!=1963,2]
t2m_1963 <- df_t2m[format(df_t2m$date,"%Y")==1963,2]
library(fitdistrplus)
FIT <- fitdist(t2m_sans1963, "norm")    ## note: it is "norm" not "normal"
class(FIT)
# [1] "fitdist"

plot(FIT)

n_sd_1963 <- (FIT$estimate[[1]]-t2m_1963)/FIT$estimate[[2]]
n_sd_1963

##### Quantiles #####
name_qt <- "era5_t2m_DJF_sum5pctl_ymean.txt"
df_qt5 <- read.table(file = paste(path,name_qt,sep=""), header = FALSE)
df_qt5$V1 <- as.Date(df_qt5$V1,format="%Y-%m-%d")

plot_serie_temp(df_qt5,ylegend ="Ndays with T under 5 percentile")


#### Corrélation GMST ####
load("./data/gmst_monthly.RData")
data_gmst_winter <- data_gmst_monthly[data_gmst_monthly$month < 3 | data_gmst_monthly$month>11,]
data_gmst_winter[data_gmst_winter$month==12,1] <- data_gmst_winter[data_gmst_winter$month==12,1]+1
data_gmst_winter_mean <- aggregate(data_gmst_winter$temp, list(data_gmst_winter$year), FUN=mean) 
colnames(data_gmst_winter_mean) <- c("date","t2m")
data_gmst_winter_mean <- data_gmst_winter_mean[data_gmst_winter_mean$date<2021,]
plot_serie_temp(data_gmst_winter_mean)

df_t2m_filter <- df_t2m[format(df_t2m$date,"%Y")<2021,]
df_gmst_t2m <- as.data.frame(cbind(data_gmst_winter_mean[data_gmst_winter_mean$date %in% format(df_t2m_filter$date,"%Y"),2],df_t2m_filter[,2]))
colnames(df_gmst_t2m) <- c("GMST","t2m")
plot_serie_temp(df_gmst_t2m,line=FALSE)
cor(df_gmst_t2m[,1],df_gmst_t2m[,2])

# En enlevant 1963
df_t2m_filter <- df_t2m_filter[format(df_t2m_filter$date,"%Y") != 1963,]
