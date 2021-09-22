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
    d <- df_ana_date[df_ana_date$date==d+1,random_anas[[i]]]
    days[i,1] <- as.Date(d)
  }
  
  df_temp_generator <- merge(days,df_t2m_daily,by="date",sort=FALSE)
  dates_seq <- as.data.frame(as.Date(seq(d0, d0+nj-1, by="days"),format="%Y-%m-%d"))
  df_generator <- as.data.frame(cbind(dates_seq,df_temp_generator[,2]))
  # df_generator[,2] <- df_generator[,2]-273.12
  colnames(df_generator) <- c("date","temp")
  return(df_generator)
}

AnaWG <- function(dfAna,dfVar,nDays,nRep){
  return(lapply(c(1:nRep),function(i) Ana(dfAna,dfVar,nDays)))
}
