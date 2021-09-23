Ana <- function(dfAna,dfVar,nDays){
  nAna <- (ncol(dfAna)-1)/3
  1.0775391
  ########## Récuparation de la partie date du fihier ana ##########
  dfDates <- dfAna[,c(1:(nAna+1))]
  
  ########## Création du vecteur aléatoire ##########
  randomAnas <- sample(n_ana,nj,replace=TRUE)
  
  #Initialisation
  d <- d0
  days <- data.frame(date = as.Date(character()))
  days[1,1] <- d
  #Simulation ittérative
  for (i in 2:nj){
    d2 <- dfDates[dfDates$date==d+1,randomAnas[[i]]]
    while (d2>"2021-08-30") {   #si la date est le 31/08/2021 (pas de lendemain), on prend un autre jour analogue
      d2 <- dfDates[dfDates$date==d+1,sample(1:20,1)]
    }
    d <- d2
    days[i,1] <- as.Date(d)
  }
  
  #Récupération des valeurs de T correspondante et mise en forme
  dfTempGenerator <- merge(days,dfVar,by="date",sort=FALSE)
  datesSeq <- as.data.frame(as.Date(seq(d0, d0+nj-1, by="days"),format="%Y-%m-%d"))
  dfGenerator <- as.data.frame(cbind(datesSeq,dfTempGenerator[,2]))
  # dfGenerator[,2] <- dfGenerator[,2]-273.12
  colnames(dfGenerator) <- c("date","temp")
  return(dfGenerator)
}

Ana2 <- function(dfAna,dfVar,nDays){
  nAna <- (ncol(dfAna)-1)/3
  
  ########## Récuparation de la partie date du fihier ana ##########
  dfDates <- dfAna[,c(1:(nAna+1))]
  
  ########## Création du vecteur aléatoire ##########
  randomAnas <- sample(n_ana,nj,replace=TRUE)
  
  #Initialisation
  d <- d0
  days <- data.frame(date = as.Date(character()))
  # days[1,1] <- d
  #Simulation ittérative
  for (i in 1:nj){
    d2 <- dfDates[dfDates$date==d,randomAnas[[i]]]
    while (d2>"2021-08-30") {   #si la date est le 31/08/2021 (pas de lendemain), on prend un autre jour analogue
      d2 <- dfDates[dfDates$date==d,sample(1:20,1)]
    }
    d <- d2
    days[i,1] <- as.Date(d)
    d <- d+1
  }
  
  #Récupération des valeurs de T correspondante et mise en forme
  dfTempGenerator <- merge(days,dfVar,by="date",sort=FALSE)
  datesSeq <- as.data.frame(as.Date(seq(d0, d0+nj-1, by="days"),format="%Y-%m-%d"))
  dfGenerator <- as.data.frame(cbind(datesSeq,dfTempGenerator[,2]))
  # dfGenerator[,2] <- dfGenerator[,2]-273.12
  colnames(dfGenerator) <- c("date","temp")
  return(dfGenerator)
}

AnaWG <- function(dfAna,dfVar,nDays,nRep){
  return(lapply(c(1:nRep),function(i) Ana(dfAna,dfVar,nDays)))
  # return(lapply(c(1:nRep),Ana,dfAna=dfAna,dfVar=dfVar,nDays=nDays))
}

AnaWG2 <- function(dfAna,dfVar,nDays,nRep){
  return(lapply(c(1:nRep),function(i) Ana2(dfAna,dfVar,nDays)))
  # return(lapply(c(1:nRep),Ana,dfAna=dfAna,dfVar=dfVar,nDays=nDays))
}
