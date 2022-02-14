library(dplyr)
library(ggplot2)
library(plyr)
library(reshape2)
library(igraph)   # pour le graphe de réseau

#### -------------------------------------------------------------------------------------------------------------------------------------------
#### Répartition des analogues entre les périodes 1951-1999 et 1972-2021
#### -------------------------------------------------------------------------------------------------------------------------------------------
path <- "~/Documents/Data/Winter/"
anas_5199 <- read.table(paste(path,"ana_z500_1_-20.30.30.70_1950-1999.txt",sep=""),header=TRUE)
dates_5199 <- stack(anas_5199[,2:21])
colnames(dates_5199) <- c("date","id")
dates_5199$date <- as.Date(as.character(dates_5199$date),format="%Y%m%d")
dates_5199 <- dates_5199[format(dates_5199$date,"%m") %in% c('12','1','2'),]
dates_5199$year <- as.numeric(format(dates_5199$date,"%Y"))
dates_5199$year[format(dates_5199$date,"%m") %in% c('11',"12")] <- 
  dates_5199$year[format(dates_5199$date,"%m") %in% c('11',"12")]+1     # shift year of the end f the year to consider winter

# p_5199 <- ggplot(data=dates_5199,aes(x=year))+
#   geom_histogram(color="black", fill="snow3",bins=49) +
#   theme_linedraw()+
#   theme(legend.position = "None",legend.title = element_blank())
# p_5199

anas_7221 <- read.table(paste(path,"ana_z500_1_-20.30.30.70_1972-2021.txt",sep=""),header=TRUE)
dates_7221 <- stack(anas_7221[,2:21])
colnames(dates_7221) <- c("date","id")
dates_7221$date <- as.Date(as.character(dates_7221$date),format="%Y%m%d")
dates_7221 <- dates_7221[format(dates_7221$date,"%m") %in% c('12','1','2'),]
dates_7221$year <- as.numeric(format(dates_7221$date,"%Y"))
dates_7221$year[format(dates_7221$date,"%m") %in% c('11',"12")] <- 
  dates_7221$year[format(dates_7221$date,"%m") %in% c('11',"12")]+1

# p_7221 <- ggplot(data=dates_7221,aes(x=year))+
#   geom_histogram(color="black", fill="snow3",bins=49) +
#   theme_linedraw()+
#   theme(legend.position = "None",legend.title = element_blank())
# p_7221

# grid.arrange(p_5199,p_7221, ncol = 2 )

dates_subPeriods <- rbind(cbind(dates_5199,group="1951-1999"),cbind(dates_7221,group="1972-2021"))

# both on the same histogram
p <- ggplot(dates_subPeriods, aes(x=year, color=group,fill=group)) + geom_histogram(position = "identity",alpha=0.2,bins=71)+
  theme_linedraw()+ theme(legend.position="top",legend.title=element_blank())+labs(x="Year",y="Number of analogs")
p

#### -------------------------------------------------------------------------------------------------------------------------------------------
## Qualité des analogues
#### -------------------------------------------------------------------------------------------------------------------------------------------
path <- "~/Documents/Data/Winter/"
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



#### -------------------------------------------------------------------------------------------------------------------------------------------
### Tentative graphe de réseaux des analogues (inabouti)
#### -------------------------------------------------------------------------------------------------------------------------------------------

anas <- as.data.frame(read.table("~/Documents/Data/Winter/ana_z500_1_-20.30.30.70.txt",header=TRUE))
# anas[,1:21] <- as.character(anas[,1:21])
anas[,  cols <- grep("^date", names(anas))] <- lapply(anas[, cols <- grep("^date", names(anas))], as.character)
anas[,  cols <- grep("^date", names(anas))] <-lapply(anas[, cols <- grep("^date", names(anas))], as.Date, format = "%Y%m%d")

# reshape data from wide to long
anas_long <-  reshape(anas, direction="long", idvar=c("date"),
                      varying = list(paste("date.an",as.character(c(1:20)),sep=""),
                                     paste("dis",as.character(c(1:20)),sep=""), 
                                     paste("cor",as.character(c(1:20)),sep="")), 
                      v.names = c("date.an","dis","cor"))

# get rid of row names and remove colmun time
row.names(anas_long)=NULL
anas_long <- anas_long[,-c(which(names(anas_long)=="time"))]
anas_1963 <- anas_long[(anas_long$date>as.Date("1962-11-31") & anas_long$date<as.Date("1963-03-01")),]

# create the network object
colnames(anas_long)[which(names(anas_long)=="cor")] <- "weight"
network <- graph_from_data_frame(d=anas_long, directed=FALSE) 

# plot it
plot(network,vertex.label="",vertex.shape="none")#,layout=layout.fruchterman.reingold, main="fruchterman.reingold")


