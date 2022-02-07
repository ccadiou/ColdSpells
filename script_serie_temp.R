library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)  # grid.arrange function for pultiplot
library(ggpubr)     # ggarange function for multiplot
library(igraph)     # for the running mean


source("fct_plot.R")
source("fct_ncdf.R")
source("fct_submean.R")

###### Load data ######
path <- "~/Documents/Data/Winter/"

#### Seasonal cycle
df_seacyc <- read.table(paste(path,"era5_t2m_DJF_fr_seacyc.txt",sep=""))
df_seacyc$V1 <- as.Date(df_seacyc$V1)
df_seacyc$V2 <- df_seacyc$V2-273.15     #convert from K to °Ccoméd
colnames(df_seacyc) <- c("date","t2m")

#PLOT
df_seacyc <- df_seacyc[order(as.Date(df_seacyc$date)),]       #order
df_seacyc <- df_seacyc[df_seacyc$date > "2020-11-30" & df_seacyc$date < "2021-03-01",] #remove "2020-02-29"
ggplot(data=df_seacyc, aes(x=date,y=t2m)) +geom_line()+ theme_linedraw()+
  scale_x_date(date_breaks = "1 month",date_minor_breaks = "5 days",date_labels = "%b")

# df <- df_seacyc
# plot_serie_temp(df_seacyc,ylegend = "Temperature (°C)",date_low="2020-12-01",date_high="2021-02-28")


#1963
load("./data/era5_t2m_daily_fr.RData")
df_t2m_1963 <- df_t2m_daily[df_t2m_daily$date > "1962-11-30" & df_t2m_daily$date < "1963-03-01",]
df_t2m_1963 <- df_t2m_1963[order(as.Date(df_t2m_1963$date)),]
df_t2m_1963$date <- df_seacyc$date

df_t2m_1956 <- df_t2m_daily[df_t2m_daily$date > "1955-11-30" & df_t2m_daily$date <= "1956-02-28",]
df_t2m_1956 <- df_t2m_1956[order(as.Date(df_t2m_1956$date)),]
df_t2m_1956$date <- df_seacyc$date

# filter winter dates for each year
df_t2m_year <- lapply(c(1951:2021),function(y) cbind.data.frame(df_t2m_daily[df_t2m_daily$date > paste(y-1,"-11-30",sep="") &
                                                                 df_t2m_daily$date <= paste(y,"-02-28",sep=""),],gp=y))
df_t2m_year <- lapply(df_t2m_year,function(df) df[order(as.Date(df$date)),])
#calculate running mean for each year
df_t2m_means <- lapply(df_t2m_year,function(df) cbind.data.frame(date=df_seacyc$date,    #cbind.data.frame conserve le format date
                                                      t2m=c(NA,NA,NA,running_mean(df[,2],7),NA,NA,NA),gp=df[,3]))
View(df_t2m_means[[1]])

#aggregate
df_all_7mean <- do.call(rbind,df_t2m_means)

df_seas <- rbind(cbind(df_t2m_1963,gp="1963"),cbind(df_t2m_1956,gp="1956")) #cbind(df_seacyc,gp="mean"),


df_t2m_1963_7mean <- df_t2m_1963
df_t2m_1963_7mean$t2m <- c(NA,NA,NA,running_mean(df_t2m_1963[,2],7),NA,NA,NA)

df_t2m_1956_7mean <- df_t2m_1956
df_t2m_1956_7mean$t2m <- c(NA,NA,NA,running_mean(df_t2m_1956[,2],7),NA,NA,NA)

df_seacyc_7mean <- df_seacyc
df_seacyc_7mean$t2m <- c(NA,NA,NA,running_mean(df_seacyc[,2],7),NA,NA,NA)


df_7mean <- rbind(cbind(df_seacyc_7mean,gp="Seasonal cycle"),cbind(df_t2m_1963_7mean,gp="1963"),cbind(df_t2m_1956_7mean,gp="1956"))

df_all <- rbind(cbind(df_7mean,type="7 days mean"),cbind(df_seas,type="daily"))


p <- ggplot(data=df_all, aes(x=date,y=t2m,linetype=type,color=gp)) + geom_line()+ theme_linedraw()+
  scale_x_date(date_breaks = "1 month",date_minor_breaks = "10 days",date_labels = "%m")#,limits=c(as.Date(date_low),as.Date(date_high)))
p

df_7mean_sc <- rbind(cbind(df_seacyc_7mean,gp="Seasonal cycle"),df_all_7mean)
df_7mean_sc$color <- ifelse(df_7mean_sc$gp =="Seasonal cycle", 'black', 'grey')
# df_7mean_sc[df_7mean_sc$gp=="Seasonal cycle",][,"color"] <- 'blue'
df_7mean_sc[df_7mean_sc$gp==1956,][,"color"] <- 'red'
df_7mean_sc[df_7mean_sc$gp==1963,][,"color"] <- 'green'
df_7mean_sc[df_7mean_sc$gp==1985,][,"color"] <- 'blue'
df_7mean_sc[df_7mean_sc$gp==1987,][,"color"] <- 'violet'
df_7mean_sc$color <- as.factor(df_7mean_sc$color)
df_7mean_sc$color <- factor(df_7mean_sc$color, levels = c("grey", "red", "green","blue","violet","black"))
# df_7mean_sc <- df_7mean_sc[order(match(df_7mean_sc$color,levels(df_7mean_sc$color))),]

p <- ggplot(data=df_7mean_sc, aes(x=date,y=t2m,color=color,group=gp)) + geom_line()+ theme_linedraw()+
  scale_x_date(date_breaks = "1 month",date_minor_breaks = "10 days",date_labels = "%b")+#,limits=c(as.Date(date_low),as.Date(date_high)))
  scale_color_manual(values=levels(df_7mean_sc$color),labels=c("1951-2021", "1956", "1963","1985","1987","Seasonal cycle"),name='Years')
p


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
