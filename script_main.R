library(ggplot2)

########## INPUT UTILISATEUR ##########
date0 <- as.Date("20101201", format = "%Y%m%d")



########## CHARGEMENT ET FORMATAGE DES DONNÉES ##########
#Chargement des données
data_ana <- read.table("../../Data/Winter/ana_z500_1_-20.30.30.70.txt",header=TRUE)
#Conversion des colonnes dates en format date
data_ana[, cols <- grep("^date", names(data_ana))] <- lapply(data_ana[, cols <- grep("^date", names(data_ana))],
                                                             function(col) as.Date(as.character(col), format = "%Y%m%d"))




