library(ggplot2)
library(dplyr)

source("fct_AnaWG.R")
source("fct_plot.R")

########## INPUT UTILISATEUR ##########
d0 <- as.Date("20101201", format = "%Y%m%d") #date de départ u générateur
nj <- 30                                    #nombre de jours de la simulation
nsim <- 1000                                 #nombre de répétition de la simulation


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
#### Génération d'une simulation ####
# random_anas <- sample(n_ana,nj,replace=TRUE)
# 
# #Initialisation
# d <- d0
# days <- data.frame(date = as.Date(character()))
# days[1,1] <- d
# #Calcul de la séquence
# for (i in 2:nj){
#   d <- df_ana_date[df_ana_date$date==d+1,random_anas[[i]]]
#   days[i,1] <- as.Date(d)
# }
# 
# df_temp_generator <- merge(days,df_t2m_daily,by="date",sort=FALSE)
# dates_seq <- as.data.frame(as.Date(seq(d0, d0+nj-1, by="days"),format="%Y-%m-%d"))
# df_generator <- as.data.frame(cbind(dates_seq,df_temp_generator[,2]))
# df_generator[,2] <- df_generator[,2]-273.12
# plot_serie_temp(df_generator)
# 
# df_tp_DJF2010 <- df_tp_daily[df_tp_daily$date>"2010-11-30" & df_tp_daily$date<"2011-03-01",]
# plot_serie_temp(df_tp_DJF2010)
# 
# plot_2y(df_generator,df_tp_DJF2010,1)

#### Ensemble de simulations ####
df_sim_list <- AnaWG(df_ana,df_t2m_daily,nj,nsim)
# df_sim2 <- AnaWG2(df_ana,df_t2m_daily,nj,nsim)

df_sim <- bind_rows(df_sim_list,.id="sim")
plot_list(df_sim_list)



########## Comparaison avec l'événement ##########
df_sim_mean <- as.data.frame(sapply(df_sim_list,function(df) mean(df[,2])))
colnames(df_sim_mean)[1] <- "y"
#boxplot
df_t2m_20102011 <- df_t2m_daily[df_t2m_daily$date>=d0 & df_t2m_daily$date<=d0+nj,]
val_20102011 <- as.data.frame(mean(df_t2m_20102011[,2]))
colnames(val_20102011)[1] <- "y"

plot_box(df_sim_mean,val_20102011)

#display all time series
df_sim_obs <- rbind(cbind(df_sim,source="Simulations"),cbind(sim=0,df_t2m_20102011,source="Observations"))
df_sim_obs$source <- as.factor(df_sim_obs$source)
df_sim_obs$source = with(df_sim_obs, factor(source, levels = rev(levels(source))))
plot_list_obs(df_sim_obs_2)

df_sim_obs$sim <- as.numeric(df_sim_obs$sim)
df_sim_obs_2 <- df_sim_obs[df_sim_obs$sim<5,]


load("../../Data/WEGE_ERA5_2000-2021.RData")


load("../../Data/Winter/WEGE_ERA5_1950-2021.Rdat")




