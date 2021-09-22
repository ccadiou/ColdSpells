library(ggplot2)

source("fct_AnaWG.R")

########## INPUT UTILISATEUR ##########
d0 <- as.Date("20101201", format = "%Y%m%d") #date de départ u générateur
nj <- 90                                       #nombre de jours de la simulation


########## CHARGEMENT DES DONNÉES ##########
#Données analogues
# file.exists("./data/ana_z500_-20.30.30.70.RData")
load("./data/ana_z500_-20.30.30.70.RData")
#Données température
load("./data/t2m_daily_fr.RData")
#Données totla precipitation
load("./data/tp_daily_fr.RData")


########## Variables globales #########
n_ana <- (ncol(df_ana)-1)/3


########## Création des différents df (dates, dist et corr) ##########
df_ana_date <- df_ana[,c(1:(n_ana+1))]
df_ana_dis <- df_ana[,c(1,(n_ana+2):(2*n_ana+1))]
df_ana_cor <- df_ana[,c(1,(2*n_ana+2):(3*n_ana+1))]


########## AnaWG -Ana Wheather Generator ##########
random_anas <- sample(n_ana,nj,replace=TRUE)

#Initialisation
d <- d0
days <- data.frame(date = as.Date(character()))
days[1,1] <- d
for (i in 2:nj){
  d <- df_ana_date[df_ana_date$date==d+1,random_anas[[i]]]
  days[i,1] <- as.Date(d)
}

df_temp_generator <- merge(days,df_t2m_daily,by="date",sort=FALSE)
dates_20102011 <- as.data.frame(as.Date(seq(d0, d0+nj-1, by="days"),format="%Y-%m-%d"))
df_generator <- as.data.frame(cbind(dates_20102011,df_temp_generator[,2]))
df_generator[,2] <- df_generator[,2]-273.12
plot_serie_temp(df_generator)

df_tp_DJF2010 <- df_tp_daily[df_tp_daily$date>"2010-11-30" & df_tp_daily$date<"2011-03-01",]
plot_serie_temp(df_tp_DJF2010)


plot_2y(df_generator,df_tp_DJF2010,1)
# test <- df_t2m[df_t2m$date %in% days$date,]









