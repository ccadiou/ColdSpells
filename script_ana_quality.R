library(dplyr)
library(ggplot2)

path <- "~/Documents/These/Data/Winter/"
fnames <- list.files(path,pattern="ana_z500*")
anas <- lapply(fnames,function(f) read.table(paste(path,f,sep=""),header=TRUE))
anas_dist <- lapply(anas,function(ana) as.data.frame(mapply( FUN = function(x,y){ ifelse(is.na(y),NA,x)},ana[,22:41],ana[,2:21])))
ana_dist <- rbind(cbind(date=anas[[1]][,1],anas_dist[[1]],group="1950-1999_NA"),
                  cbind(date=anas[[2]][,1],anas_dist[[2]],group="1950-1999"),
                  cbind(date=anas[[3]][,1],anas_dist[[3]],group="w1963_NA"),
                  cbind(date=anas[[4]][,1],anas_dist[[4]],group="w1963"),
                  cbind(date=anas[[5]][,1],anas_dist[[5]],group="1972-2021_NA"),
                  cbind(date=anas[[6]][,1],anas_dist[[6]],group="1972-2021"),
                  cbind(date=anas[[7]][,1],anas_dist[[7]],group="1950-2021"))

ana_dist_mean <- data.frame(date=ana_dist[,1], quality=rowMeans(ana_dist[,-c(1,22)],na.rm=TRUE),group=ana_dist$group)
ana_dist_mean$date <- as.Date(as.character(ana_dist_mean$date),format = "%Y%m%d")
ana_dist_mean_winter <- ana_dist_mean[as.numeric(strftime(ana_dist_mean$date, "%m")) %in% c(1:2,12),]


p <- ggplot(ana_dist_mean_winter, aes(x=group,y=quality)) + 
  geom_boxplot()+
  stat_summary(fun=mean, geom="point",shape=18, size=3)+
  # labs(title = title, x = xlabel, y = ylabel)+
  theme_classic()
p
