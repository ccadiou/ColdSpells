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

# View(df_tp)
#_________________________________________________________________________________________________________
###### Sous-moyenne (max sur 30 jours, 10 jours, 3 jours) ######
#Sélection de la variable
var="t2m"
if (var=="t2m"){df_var <- df_t2m;df_var_anomalie <- df_t2m_anomalie;var_label <- "Temperature (°C)";extreme <- "min"}
if (var=="tp"){df_var <- df_tp;df_var_anomalie <- df_tp_anomalie;var_label <- "Total precipitation (mm)";extreme <- "max"}
df_var$date <- format(df_var$date,"%Y")
colnames(df_var)[2] <- "var"
colnames(df_var_anomalie)[2] <- "var"
df_var_anomalie$date <- format(as.Date(df_var_anomalie$date,format="%Y-%m-%d"),"%Y")
# df_var$date <- as.numeric(df_var$date)


### Sur la période 1950-2021 ###
df_extdate <- rbind(cbind(ndays_min(30,path,var,extreme),"n_days"=30,"index"=ndays_minidx(30,path,var,extreme)[,2]),
                    cbind(ndays_min(10,path,var,extreme),"n_days"=10,"index"=ndays_minidx(10,path,var,extreme)[,2]),
                    cbind(ndays_min(3,path,var,extreme),"n_days"=3,"index"=ndays_minidx(3,path,var,extreme)[,2]),
                    cbind(df_var,"n_days"=90,"index"=as.Date(paste(df_var$date,"-01-01",sep=""),format="%Y-%m-%d")))
df_extdate_3 <- select_extremes(df_extdate,var,n_days,extreme,10)
# df_extdate_3 <- df_extdate_3[c(10:12,1:3,4:6,7:9),]
df_extdate_3$n_days <- factor(df_extdate_3$n_days, levels=c(90,30,10,3))

#plot
# df_extdate_3$date <- factor(df_extdate_3$date,levels=levels(as.factor(df_extdate$date)))
df_extdate_3$date <- as.numeric(as.character(df_extdate_3$date))
plot_submean(df_extdate_3,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")
 # save(df_extdate3,file="./data/t2m_DJF_minbyperiodduration")
df_extdate_3$n_days <- as.numeric(as.character(df_extdate_3$n_days))
# write.table(df_extdate_3,"./data/t2m_DJF_minbyrange.txt",sep="\t",row.names=FALSE)

plot_submean_bubble(df_extdate_3,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")




#multiplot t2m et tp
# p_tp <- plot_submean(df_extdate_3,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")
# p_t2m <- plot_submean(df_extdate_3,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")
# multiplot(p_tp, p_t2m,cols=1)

#### Sur le période 2000-2021 ####
df_extdate_XXI <- df_extdate[df_extdate$date>1999,]
df_extdate_XXI_3 <- select_extremes(df_extdate_XXI,var,n_days,extreme,3)
df_extdate_XXI_3 <- df_extdate_XXI_3[c(10:12,1:3,4:6,7:9),]   #change l'odre des lignes pour que l'affichage se fasse dans le bon ordre lors du plot
df_extdate_XXI_3$date <- as.numeric(df_extdate_XXI_3$date)
df_extdate_XXI_3$n_days <- factor(df_extdate_XXI_3$n_days, levels=c(90,30,10,3))    #change l'ordre des facteurs pour que la légende s'affiche dans le bon ordre

#plot
df_submeans <- rbind(cbind(df_extdate_XXI_3,"period"="XXI"),cbind(df_extdate_3,"period"="XX"))
df_submeans <- df_submeans[!(df_submeans$date>"1999" & df_submeans$period=="XX"),]   #enlève les doublons si l'une des années du XXIe fait partie des records sur 1950-2021 
plot_submean(df_submeans,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")

##plot avec t2m et tp
df_t2m_ext3 <- df_submeans
# df_tp_ext3 <- df_extdate_3
df_tp_t2m_ext3 <- rbind(cbind(df_t2m_ext3,"var_name"="t2m"),cbind(df_tp_ext3,"var_name"="tp"))
plot_submean_group(df_tp_t2m_ext3,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")
#_____________________________________________________________________________________________________________________
##### Sous-moyennes à parti de l'anomalie par point de grille #####
df_extdate <- rbind(cbind(ndays_min(30,path,var,extreme,anomalie=TRUE),"n_days"=30,"index"=ndays_minidx(30,path,var,extreme,anomalie=TRUE)[,2]),
                    cbind(ndays_min(10,path,var,extreme,anomalie=TRUE),"n_days"=10,"index"=ndays_minidx(10,path,var,extreme,anomalie=TRUE)[,2]),
                    cbind(ndays_min(3,path,var,extreme,anomalie=TRUE),"n_days"=3,"index"=ndays_minidx(3,path,var,extreme,anomalie=TRUE)[,2]),
                    cbind(df_var_anomalie,"n_days"=90,"index"=as.Date(paste(df_var_anomalie$date,"-01-01",sep=""),format="%Y-%m-%d")))
df_extdate_3 <- select_extremes(df_extdate,var,n_days,extreme,3)
# df_extdate_3 <- df_extdate_3[c(10:12,1:3,4:6,7:9),]
df_extdate_3$n_days <- factor(df_extdate_3$n_days, levels=c(90,30,10,3))

#plot
# save(df_extdate,file="./data/t2m_DJF_minbyperiodduration")
df_extdate_3[which(duplicated(df_extdate_3$index)),4] <- df_extdate_3[which(duplicated(df_extdate_3$index)),4]+1   #différencie les doublons dont la date de départ est la même
plot_submean(df_extdate_3,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")

#### Sur le période 200-2021 ####
df_extdate_XXI <- df_extdate[df_extdate$date>1999,]
df_extdate_XXI_3 <- select_extremes(df_extdate_XXI,var,n_days,extreme,3)
# df_extdate_XXI_3 <- df_extdate_XXI_3[c(10:12,1:3,4:6,7:9),]   #change l'odre des lignes pour que l'affichage se fasse dans le bon ordre lors du plot
df_extdate_XXI_3$n_days <- factor(df_extdate_XXI_3$n_days, levels=c(90,30,10,3))    #change l'ordre des facteurs pour que la légende s'affiche dans le bon ordre
#plot des dates
df_submeans <- rbind(cbind(df_extdate_XXI_3,"period"="XXI"),cbind(df_extdate_3,"period"="XX"))
df_submeans <- df_submeans[!(df_submeans$date>"1999" & df_submeans$period=="XX"),]   #enlève les doublons si l'une des années du XXIe fait partie des records sur 1950-2021 
df_submeans[which(duplicated(df_submeans$index)),4] <- df_submeans[which(duplicated(df_submeans$index)),4]+1   #différencie les doublons dont la date de départ est la même
plot_submean(df_submeans,xlabel = "Date",ylabel = var_label,legend_title = "Time range\n(days)")

  

# load("./data/t2m_DJF_minbyperiodduration")
# View(df_mindate)






