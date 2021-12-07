
source("~/Documents/These/Code/Winter/fct_ncmap.R")

##### Plot anomalie + courbes de geopt _____________________________________________________________________________________________________________________________#####
library(ncdf4)
source("fct_plotmap.R")
# set path and filename
ncpath <- "~/Documents/These/Data/Winter/submeans_z500_largefield/"
test <- nc_to_array(ncpath,"era5_z500_90_1953-01-01_1953-03-31.nc","z500")
ncname <- paste(ncpath,"era5_z500_90_1953-01-01_1953-03-31.nc",sep="")
# open a netCDF file
ncin <- nc_open(ncname)
lon <- ncvar_get(ncin, "lon")
lat <- ncvar_get(ncin, "lat", verbose = F)
nc_close(ncin)

ncnames <- list.files(ncpath)
ncs <- lapply(ncnames,function(f) nc_to_array(ncpath,f,"z500"))

ncpath_anomalie <- "~/Documents/These/Data/Winter/submeans_z500_anomalie_largefield/"
ncnames_anomalie <- list.files(ncpath_anomalie) 
ncs_anomalie <- lapply(ncnames_anomalie,function(f) nc_to_array(ncpath_anomalie,f,"z500"))

dev.off()
par(mfcol=c(3,3))#,mar=c(-1,0,0,0))
lapply(c(1:9),function(i) plot_z500(ncs[[i]],ncs_anomalie[[i]]))

dev.off()
par(mfcol=c(3,3))
lapply(c(11:19),function(i) plot_z500(ncs[[i]],ncs_anomalie[[i]]))

dev.off()
par(mfcol=c(3,3))
lapply(c(21:29),function(i) plot_z500(ncs[[i]],ncs_anomalie[[i]]))

dev.off()
min <- min(unlist(ncs[31:39]))
max <- max(unlist(ncs[31:39]))
par(mfcol=c(3,3))
lapply(c(31:39),function(i) plot_z500(ncs[[i]],ncs_anomalie[[i]],min,max,title=substring(ncnames[[i]],14,34)))


plot_z500(ncs[[33]],ncs_anomalie[[33]],min,max,title=substring(ncnames[[33]],14,34))


nc_to_array <- function(path,fname,var){
  require(ncdf4)
  ncname <- paste(path,fname,sep="")
  # open a netCDF file
  ncin <- nc_open(ncname)
  lon <- ncvar_get(ncin, "lon")
  lat <- ncvar_get(ncin, "lat", verbose = F)
  nc_array <- ncvar_get(ncin, var)
  nc_close(ncin)
  return(nc_array)
}


plot_z500 <- function(array,array_anomalie,min,max,title){
  image.cont.ano(lon,lat,array_anomalie,mar=c(1,1,1,1),titre=title,legend=FALSE,transpose = FALSE,zlev=seq(min,max,length=11))
  image.cont.c(lon,lat,array,mar=c(1,1,1,1),transpose = FALSE,add=TRUE,titre=title) 
}




