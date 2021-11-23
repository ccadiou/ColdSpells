
ncpath <- "~/Documents/These/Data/Winter/submeans_z500/"
source("~/Documents/These/Code/Winter/fct_ncmap.R")

##### Plot anomalie + courbes de geopt _____________________________________________________________________________________________________________________________#####
library(ncdf4)
source("fct_plotmap.R")
# set path and filename
ncpath <- "~/Documents/These/Data/Winter/submeans_z500_largefield/"
# ncnames <- list.files(ncpath) 
ncname <- paste(ncpath,"era5_z500_10_1956-02-09_1956-02-18.nc",sep="")
dname <- "z500"
# open a netCDF file
ncin <- nc_open(ncname)
lon <- ncvar_get(ncin, "lon")
lat <- ncvar_get(ncin, "lat", verbose = F)
nc_array <- ncvar_get(ncin, dname)
nc_close(ncin)

ncpath_anomalie <- "~/Documents/These/Data/Winter/submeans_z500_anomalie_largefield/"
# ncnames <- list.files(ncpath) 
ncname_anomalie <- paste(ncpath_anomalie,"era5_z500_10_1956-02-09_1956-02-18_anomalie.nc",sep="")
# open a netCDF file
ncin_anomalie <- nc_open(ncname_anomalie)
lon <- ncvar_get(ncin_anomalie, "lon")
lat <- ncvar_get(ncin_anomalie, "lat", verbose = F)
nc_array_anomalie <- ncvar_get(ncin_anomalie, dname)
nc_close(ncin)

image.cont.ano(lon,lat,nc_array_anomalie,titre="test",transpose = FALSE)
image.cont.c(lon,lat,nc_array,titre="test",transpose = FALSE,add=TRUE)

