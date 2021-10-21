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

##### Sous-moyenne (max sur 30 jours, 10 jours, 3 jours) #####
# nd <- 30  #input utilisateur
# df_t2m$date <- as.Date(df_t2m$date,format="%Y-%m-%d")
var="tp"
if (var=="t2m"){df_var <- df_t2m;var_label <- "Temperature (°C)";extreme <- "min"}
if (var=="tp"){df_var <- df_tp;var_label <- "Total precipitation (mm)";extreme <- "max"}
df_var$date <- format(df_var$date,"%Y")
colnames(df_var)[2] <- "var"
# df_var$date <- as.numeric(df_var$date)
df_extdate <- rbind(cbind(ndays_min(30,path,var,extreme),"n_days"=30,"index"=ndays_minidx(30,path,var,extreme)[,2]),
                    cbind(ndays_min(10,path,var,extreme),"n_days"=10,"index"=ndays_minidx(10,path,var,extreme)[,2]),
                    cbind(ndays_min(3,path,var,extreme),"n_days"=3,"index"=ndays_minidx(3,path,var,extreme)[,2]),
                    cbind(df_var,"n_days"=90,"index"=as.Date(paste(df_var$date,"-01-01",sep=""),format="%Y-%m-%d")))
#df_extdate <- df_extdate[,c(1,3,2,4)]
# df_extdate$n_days <- as.factor(df_extdate$n_days)
df_extdate_3 <- select_extremes(df_extdate,var,n_days,extreme,3)
# df_extdate_3 <- df_extdate_3[c(10:12,1:3,4:6,7:9),]
df_extdate_3$n_days <- factor(df_extdate_3$n_days, levels=c(90,30,10,3))

# save(df_extdate,file="./data/t2m_DJF_minbyperiodduration")
plot_submean(df_extdate_3,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")

#Sélection des dates du XXIe siècle
df_extdate_XXI <- df_extdate[df_extdate$date>1999,]
df_extdate_XXI_3 <- select_extremes(df_extdate_XXI,var,n_days,extreme,3)
# df_extdate_XXI_3 <- df_extdate_XXI_3[c(10:12,1:3,4:6,7:9),]   #change l'odre des lignes pour que l'affichage se fasse dans le bon ordre lors du plot
df_extdate_XXI_3$n_days <- factor(df_extdate_XXI_3$n_days, levels=c(90,30,10,3))    #change l'ordre des facteurs pour que la légende s'affiche dans le bon ordre
#plot des dates
df_submeans <- rbind(cbind(df_extdate_XXI_3min,"period"="XXI"),cbind(df_extdate_3min,"period"="XX"))
df_submeans <- df_submeans[!(df_submeans$date>"1999" & df_submeans$period=="XX"),]   #enlève les doublons si l'une des années du XXIe fait partie des record sur 1950-2021 #uniquement pour tp, évite le doublon expliqué ci-dessus
plot_submean(df_submeans,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")

#### Corrélation GMST ####
load("./data/gmst.RData")
plot_serie_temp(data_gmst)
df_gmst_t2m <- as.data.frame(cbind(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2]))
colnames(df_gmst_t2m) <- c("GMST","t2m")
plot_serie_temp(df_gmst_t2m)
cor(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2])
cor(data_gmst[data_gmst$date %in% df_t2m$t,2],df_t2m[,2])

df_t2m <- df_t2m[t!=1963,]
