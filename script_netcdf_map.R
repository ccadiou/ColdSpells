
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

#pour essayer de mettre les titres
x<-1:10
par(mar=c(2.5,2.5,1,1))
layout(matrix(c(1,2,3,4,1,5,3,6),ncol=2),heights=c(1,3,1,3))
plot.new()
text(0.5,0.5,"First title",cex=2,font=2)
plot(x)
plot.new()
text(0.5,0.5,"Second title",cex=2,font=2)
hist(x)
boxplot(x)
barplot(x)
