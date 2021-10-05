##### Sous-moyenne (max sur 30 jours, 10 jours, 3 jours) #####
ndays_min <- function(nDays,path){
  nameNDays <- paste("era5_t2m_DJF_",nDays,"min.txt",sep="")
  dfNDays <- read.table(file = paste(path,nameNDays,sep=""), header = FALSE)
  dfNDays$V1 <- format(as.Date(dfNDays$V1,format="%Y-%m-%d"),"%Y")
  dfNDays$V2 <- dfNDays$V2-273.15
  colnames(dfNDays) <- c("date","temp")
  return(dfNDays)
}

ndays_minidx <- function(nDays,path){
  nameIndex <- paste("era5_t2m_DJF_",nDays,"minidx.txt",sep="")
  dfIndex <- read.table(file = paste(path,nameIndex,sep=""), header = FALSE)
  dfIndex$V1 <- as.Date(dfIndex$V1,format="%Y-%m-%d")
  dfIndex$V2 <- dfIndex$V1-75+dfIndex$V2*nDays
  dfIndex$V1 <- format(dfIndex$V1,"%Y")
  colnames(dfIndex) <- c("date","index")
  return(dfIndex)
}
