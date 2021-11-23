##### Sous-moyenne (max sur 30 jours, 10 jours, 3 jours) #####
ndays_min <- function(nDays,path,var_name,func,anomalie=FALSE){
  if (anomalie) {anomalie_txt <- "_anomaliepdg"} else {anomalie_txt <- ""}
  nameNDays <- paste("era5_",var_name,"_DJF_",nDays,func,anomalie_txt,".txt",sep="")
  dfNDays <- read.table(file = paste(path,nameNDays,sep=""), header = FALSE)
  dfNDays$V1 <- format(as.Date(dfNDays$V1,format="%Y-%m-%d"),"%Y")
  if (var_name=="t2m" & !anomalie){dfNDays$V2 <- dfNDays$V2-273.15}
  if (var_name=="sf"){dfNDays$V2 <- dfNDays$V2*1000}
  colnames(dfNDays) <- c("date","var")
  return(dfNDays)
}

ndays_minidx <- function(nDays,path,var_name,func,anomalie=FALSE){
  if (anomalie) {anomalie_txt <- "_anomaliepdg"} else {anomalie_txt <- ""}
  nameIndex <- paste("era5_",var_name,"_DJF_",nDays,func,"idx",anomalie_txt,".txt",sep="")
  dfIndex <- read.table(file = paste(path,nameIndex,sep=""), header = FALSE)
  dfIndex$V1 <- as.Date(dfIndex$V1,format="%Y-%m-%d")
  dfIndex$V2 <- dfIndex$V1-75+dfIndex$V2*nDays
  dfIndex$V1 <- format(dfIndex$V1,"%Y")
  colnames(dfIndex) <- c("date","index")
  return(dfIndex)
}

#Selection des n plus petites valeurs de variable par group
select_extremes <- function(df,group,func="min",n=3){
  # variable <- enquo(variable)
  # group <- enquo(group)
  df_new <- df %>%                                      # Top N lowest values by group
    group_by({{group}})
  if (func=="min"){df_new <- df_new %>%
    arrange(var)}
  else if(func=="max"){df_new <- df_new %>%
    arrange(desc(var))}
  else {print("Enter a valid function: min or max"):return(1)}
  return(df_new %>% slice(1:n))
}

# #retroune l
# select_n_extremes(path,var_name,n,func,anomalie=FALSE) <- function(){
#   if (var_name=="t2m"){df_var <- df_t2m;df_var_anomalie <- df_t2m_anomalie;var_label <- "Temperature (°C)";extreme <- "min"}
#   if (var_name=="tp"){df_var <- df_tp;df_var_anomalie <- df_tp_anomalie;var_label <- "Total precipitation (mm)";extreme <- "max"}
#   df_var$date <- format(df_var$date,"%Y")
#   colnames(df_var)[2] <- "var"
#   colnames(df_var_anomalie)[2] <- "var"
#   df_var_anomalie$date <- format(as.Date(df_var_anomalie$date,format="%Y-%m-%d"),"%Y")
#   # df_var$date <- as.numeric(df_var$date)
#   
#   
#   ### Sur la période 1950-2021 ###
#   df_extdate <- rbind(cbind(ndays_min(30,path,var,extreme),"n_days"=30),
#                       cbind(ndays_min(10,path,var,extreme),"n_days"=10),
#                       cbind(ndays_min(3,path,var,extreme),"n_days"=3),
#                       cbind(df_var,"n_days"=90))
#   df_extdate_3 <- select_extremes(df_extdate,n_days,extreme,10)
#   # df_extdate_3 <- df_extdate_3[c(10:12,1:3,4:6,7:9),]
#   df_extdate_3$n_days <- factor(df_extdate_3$n_days, levels=c(90,30,10,3))
#   
#   #plot
#   # df_extdate_3$date <- factor(df_extdate_3$date,levels=levels(as.factor(df_extdate$date)))
#   df_extdate_3$date <- as.numeric(as.character(df_extdate_3$date))
# }