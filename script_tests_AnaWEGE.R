library("plyr")
library("reshape2")
library("igraph")

#########
### Graphes de réseaux des analogues
#########

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

