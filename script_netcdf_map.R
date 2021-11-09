
ncpath <- "~/Documents/These/Data/Winter/submeans_z500/"
source("~/Documents/These/Code/Winter/fct_ncmap.R")

###### raster plot avec ou sans interpolation __________________________________________________________________________________________________________________####
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
# library(ggplot2) # package for plotting
library(maptools) # for adding countries


ncpath <- "~/Documents/These/Data/Winter/submeans_z500/"
ncnames <- list.files(ncpath)
ncnames <- ncnames[c(4:6,1:3,7:13)]   #replace dans l'ordre des sous-moyennes(3,10,30,90)
nc_arrays <- lapply(ncnames,function(f) nc_to_array(ncpath,f,"z500"))
zmin <- min(unlist(nc_arrays))
# zmax <- max(unlist(nc_arrays))

#multiplot de toutes les cartes z500
dev.off()
par(mfrow=c(5,3),mar=c(2,2.5,3,0))
plot_list <- lapply(ncnames,function(f) plot_nc_raster_from_file(ncpath,f,"z500",zmin,zmax))

plot_nc_raster_from_file(ncpath,"era5_z500_DJFmean.nc","z500",zmin,zmax)

##### Avec les fonctions de pascal _____________________________________________________________________________________________________________________________#####
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

ncpath_anomalie <- "~/Documents/These/Data/Winter/submeans_z500_anomalie_largefield/"
# ncnames <- list.files(ncpath) 
ncname_anomalie <- paste(ncpath_anomalie,"era5_z500_10_1956-02-09_1956-02-18_anomalie.nc",sep="")
# open a netCDF file
ncin_anomalie <- nc_open(ncname_anomalie)
lon <- ncvar_get(ncin_anomalie, "lon")
lat <- ncvar_get(ncin_anomalie, "lat", verbose = F)
nc_array_anomalie <- ncvar_get(ncin_anomalie, dname)

image.cont.ano(lon,lat,nc_array_anomalie,titre="test",transpose = FALSE)
image.cont.c(lon,lat,nc_array,titre="test",transpose = FALSE,add=TRUE)


pretty(nc_array_anomalie,10)



