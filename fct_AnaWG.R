Ana <- function(dfAna,dfVar,nDays){
  nAna <- (ncol(dfAna)-1)/3
  
  ########## Récuparation de la partie date du fihier ana ##########
  dfDates <- dfAna[,c(1:(nAna+1))]
  
  ########## Création de ##########
  randomAnas <- sample(n_ana,nj,replace=TRUE)
  
  #Initialisation
  d <- d0
  days <- data.frame(date = as.Date(character()))
  days[1,1] <- d
  for (i in 2:nj){
    d <- df_ana_date[dfDates$date==d+1,randomAnas[[i]]]
    days[i,1] <- as.Date(d)
  }
  df_generator <- merge(days,df_t2m,by="date")
  
}

AnaWG <- function(dfAna,dfVar,nDays,nRep){
  test <- lapply(c(1:nRep),Ana,dfAna,dfVar,nDays)
  
}
