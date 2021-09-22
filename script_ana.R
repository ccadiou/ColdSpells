library(ggplot2)

########## INPUT UTILISATEUR ##########
d0 <- as.Date("20101201", format = "%Y%m%d") #date de départ u générateur
nj <- 30                                        #nombre de jours de la simulation


########## CHARGEMENT DES DONNÉES ##########
#Données analogues
df_ana <- read.table("../../Data/Winter/ana_z500_1_-20.30.30.70.txt",header=TRUE) #fichier d'analogues avec date/dates analogues/distance analogues/correlation analogues
df_ana[, cols <- grep("^date", names(df_ana))] <- lapply(df_ana[, cols <- grep("^date", names(df_ana))],   #conversion des colonnes dates en format date
                                                             function(col) as.Date(as.character(col), format = "%Y%m%d")) 
load("./data/ana_z500_-20.30.30.70.RData")
help(file.exists("./data/ana_z500_-20.30.30.70.RData")
#Données température



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
days <- data.frame(Date = as.Date(character()))
days[1,1] <- d
for (i in 2:nj){
  d <- df_ana_date[df_ana_date$date==d+1,random_anas[[i]]]
  days[i,1] <- as.Date(d)
}

test <- 
Data1 = data.frame(accession_number = c('a','b','c','d','e','f'), values =c('1','3','4','2','3','12'))
