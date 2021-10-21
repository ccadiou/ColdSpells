##### Sous-moyenne (max sur 30 jours, 10 jours, 3 jours) #####
ndays_min <- function(nDays,path,var_name,func){
  nameNDays <- paste("era5_",var_name,"_DJF_",nDays,func,".txt",sep="")
  dfNDays <- read.table(file = paste(path,nameNDays,sep=""), header = FALSE)
  dfNDays$V1 <- format(as.Date(dfNDays$V1,format="%Y-%m-%d"),"%Y")
  if (var_name=="t2m"){dfNDays$V2 <- dfNDays$V2-273.15}
  colnames(dfNDays) <- c("date","var")
  return(dfNDays)
}

ndays_minidx <- function(nDays,path,var_name,func){
  nameIndex <- paste("era5_",var_name,"_DJF_",nDays,func,"idx.txt",sep="")
  dfIndex <- read.table(file = paste(path,nameIndex,sep=""), header = FALSE)
  dfIndex$V1 <- as.Date(dfIndex$V1,format="%Y-%m-%d")
  dfIndex$V2 <- dfIndex$V1-75+dfIndex$V2*nDays
  dfIndex$V1 <- format(dfIndex$V1,"%Y")
  colnames(dfIndex) <- c("date","index")
  return(dfIndex)
}

#Selection des n plus petites valeurs de variable par group
select_extremes <- function(df,variable,group,func="min",n=3){
  variable <- enquo(variable)
  group <- enquo(group)
  df_new <- df %>%                                      # Top N lowest values by group
    group_by(!!group)
  if (func=="min"){df_new <- df_new %>%
    arrange(!!variable)}
  else if(func=="max"){df_new <- df_new %>%
    arrange(desc(!!variable))}
  else {print("Enter a valid function: min or max"):return(1)}
  return(df_new %>% slice(1:n))
}
