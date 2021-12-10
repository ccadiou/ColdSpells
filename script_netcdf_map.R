
source("~/Documents/These/Code/Winter/fct_ncmap.R")
source("fct_plotmap.R")
source("fct_plot.R")
library(ncdf4)

##### Plot anomalie + courbes de geopt _____________________________________________________________________________________________________________________________#####
# set path and filename
ncpath <- "~/Documents/These/Data/Winter/submeans_z500_largefield/"

ncnames <- list.files(ncpath)
ncname <- paste(ncpath,ncnames[[1]],sep="")
# open a netCDF file
ncin <- nc_open(ncname)
lon <- ncvar_get(ncin, "lon")
lat <- ncvar_get(ncin, "lat", verbose = F)
nc_close(ncin)

ncs <- lapply(ncnames,function(f) nc_to_array(ncpath,f,"z500"))

ncpath_anomalie <- "~/Documents/These/Data/Winter/submeans_z500_anomalie_largefield/"
ncnames_anomalie <- list.files(ncpath_anomalie) 
ncs_anomalie <- lapply(ncnames_anomalie,function(f) nc_to_array(ncpath_anomalie,f,"z500"))

dev.off()
par(mfcol=c(3,3))#,mar=c(-1,0,0,0))
lapply(c(1:9),function(i) plot_ano.c(ncs[[i]],ncs_anomalie[[i]]))

dev.off()
par(mfcol=c(3,3))
lapply(c(11:19),function(i) plot_ano.c(ncs[[i]],ncs_anomalie[[i]]))

dev.off()
par(mfcol=c(3,3))
lapply(c(21:29),function(i) plot_ano.c(ncs[[i]],ncs_anomalie[[i]]))

dev.off()
min <- min(unlist(ncs[31:39]))
max <- max(unlist(ncs[31:39]))
par(mfcol=c(3,3))
lapply(c(31:39),function(i) plot_ano.c(ncs[[i]],ncs_anomalie[[i]],min,max,title=substring(ncnames[[i]],14,34)))


plot_ano.c(ncs[[33]],ncs_anomalie[[33]],min,max,title=substring(ncnames[[33]],14,34))


###########
#Ncdf maps of few files withour anomalie
ncpath <- "~/Documents/These/Data/Winter/z500_specific_dates/"

ncnames <- list.files(ncpath)
nctitles <- paste(substr(ncnames,13,16),"-",substr(ncnames,17,18),"-",substr(ncnames,19,20),sep="")
nctitles[[1]] <- paste(nctitles[[1]],"(ref)")
ncname <- paste(ncpath,ncnames[[1]],sep="")
# open a netCDF file
ncin <- nc_open(ncname)
lon <- ncvar_get(ncin, "lon")
lat <- ncvar_get(ncin, "lat", verbose = F)
nc_close(ncin)

ncs <- lapply(ncnames,function(f) nc_to_array(ncpath,f,"z500"))
min <- min(unlist(ncs))
max <- max(unlist(ncs))

image.cont(lon,lat,ncs[[1]],titre=ncnames[[1]],transpose=FALSE,zlev=seq(min,max,length=11))

dev.off()
par(mfcol=c(1,4))#,mar=c(-1,0,0,0))
lapply(c(1:4),function(i) image.cont(lon,lat,ncs[[i]],transpose=FALSE,zlev=seq(min,max,length=100),mar=c(8,3,3,1),titre=nctitles[[i]],legend=TRUE,ylab="",xlab=""))
