
ncpath <- "~/Documents/These/Data/Winter/submeans_z500/"
source("~/Documents/These/Code/Winter/fct_ncmap.R")


##### image.plot pas smooth_________________________________________________________________________________________________________________________________________________####
library(ncdf4)
library(sp)
library(maptools)
library(fields)         # this package has image.plot 
library(RColorBrewer)

# set path and filename

ncnames <- list.files(ncpath) 
ncname <- "era5_z500_10_1956-02-09_1956-02-19"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "z500"

# open a netCDF file
ncin <- nc_open(ncfname)
#get variables
lon <- ncvar_get(ncin, "lon")
lat <- ncvar_get(ncin, "lat", verbose = F)
# lat <- rev(lat)
t <- ncvar_get(ncin, "time")

slp_array <- ncvar_get(ncin, dname)
# slp_array <- slp_array[,dim(lat):1]
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(slp_array)

nc_close(ncin)

border <- max(abs(min(slp_array)),abs(max(slp_array))) # set border to center color scale

palette = brewer.pal(11,'RdBu')
#Display an register image
# png(filename="slp.png",bg="white")
dev.new()
image.plot(lon, lat, slp_array,
             # main = "Sea Level Pressure",
             xlab = "Longitude",
             ylab = "Latitude",
             legend.lab = "SLP anomalies [Pa]",
             # legend.line = 2.5,
             lwd=0,
             # breaks=seq(-border,border,length.out=12), # center color scale
             # col = rev(viridis(200)))  # To get the higher emission values darker we invert it
             col = rev(palette)) # To get the higher emission values darker we invert it
# rcolorbrewer# col = rev(Spectral(200)))  # To get the higher emission values darker we invert it
data(wrld_simpl)  #displays countries' borders
plot(wrld_simpl, add = TRUE)
# mtext("Subtitle")  # To put a sub-title
# dev.off()
p

###### raster plot avec ou sans interpolation __________________________________________________________________________________________________________________####
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(ggplot2) # package for plotting
library(maptools) # for adding countries
library(maps)
library(sp)
library(rgdal)

ncpath <- "~/Documents/These/Data/Winter/submeans_z500/"
ncnames <- list.files(ncpath)
ncnames <- ncnames[c(4:6,1:3,7:12)]
nc_arrays <- lapply(ncnames,function(f) nc_to_array(ncpath,f,"z500"))
zmin <- min(unlist(nc_arrays))
zmax <- max(unlist(nc_arrays))

#multiplot de toutes les cartes z500
dev.off()
par(mfrow=c(4,3),mar=c(3,3,1,1))
plot_list <- lapply(ncnames,function(f) plot_nc_raster_from_file(ncpath,f,"z500",zmin,zmax))

## test
dev.off()
par(mfrow=c(4,3))#,mar=c(0.5,0.5,1,1))
plot_list <- lapply(ncnames,function(f) plot_nc_raster_from_file(ncpath,f,"z500",zmin,zmax))
dev.off()
plot_nc_raster_from_file(ncpath,"era5_z500_10_1956-02-09_1956-02-19.nc","z500",zmin,zmax)

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


#project lcc
# crs.lonlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # +lat_1=12.190 +lat_0=40 +lon_0=-97 +lat_2=45
# 
# # reproject to lcc
# rast.ext <- projectExtent(r, crs.lonlat)
# rast.lonlat <- projectRaster(r, rast.ext)
# #world spatial polygons
# world <- map(fill = TRUE, col="transparent", plot=FALSE)
# IDs <- sapply(strsplit(world$names, ":"), function(x) x[1])
# world <- map2SpatialPolygons(
#   world, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
# world.lonlat <- spTransform(world, CRSobj = crs.lcc)

#
test <- raster(paste(ncpath,"era5_z500_10_1956-02-09_1956-02-19.nc",sep=""),varname="z500")
plot(test)
rgeos::set_RGEOS_CheckValidity(2L)
eu <- crop(wrld_simpl,extent(-20,30,30,70))

nc_data <- nc_open(paste(ncpath,"era5_z500_10_1956-02-09_1956-02-19.nc",sep=""))

#get variables
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
time <- ncvar_get(nc_data, "time")
nc.array <- ncvar_get(nc_data, "z500") # store the data in a 3-dimensional array
nc_close(nc_data) 

#convert to raster ,nrows=max(lon)-min(lon)+1, ncols=max(lat)-min(lat)+1
crs.lcc <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
r <- raster(t(nc.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) #crs=crs.latlon)
r <- flip(r, direction='y')
#plot
dev.off()
plot(r)
# r_test <- projectRaster(r,crs=crs(wrld_simpl),res=1)
plot(r,col=colorRampPalette(colors=c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7',
                                     '#fddbc7','#f4a582','#d6604d','#b2182b','#67001f'))(255),
     zlim=c(zmin,zmax), legend=FALSE)
# map(add=TRUE)
# plot(world.lcc, add = TRUE)

rgeos::set_RGEOS_CheckValidity(2L)
data(wrld_simpl, package="maptools")
eu_simpl <- crop(wrld_simpl,extent(-20,30,30,70))
plot(eu_simpl,add=TRUE)
# worldmap <- fortify(wrld_simpl)

## Load raster package and an example SpatialPolygonsDataFrame
library(raster) 
rgeos::set_RGEOS_CheckValidity(2L)
data("wrld_simpl", package="maptools")

## Crop to the desired extent, then plot
out <- crop(wrld_simpl, extent(-20,30,30,70))
plot(out)#, col="khaki", bg="azure2")

##### ggplot2, interpolation en amont__________________________________________________________________________________________________________________________________####

library(ggplot2)
library(akima)
library(raster)
library(maptools) # for adding countries
nc_brick <- brick(ncfname)
tes <- nc_brick[[1]]
spdf <- as(tes, "SpatialPixelsDataFrame")
mydf <- as.data.frame(spdf)
colnames(mydf) <- c("value", "x", "y")
gdat <- interp2xyz(mydf2, data.frame = TRUE)
mydf2 <- with(mydf, interp(x = x, 
                           y = y, 
                           z = value,
                           xo = seq(min(x), max(x), length = 400),
                           duplicate = "mean"))
gdat <- interp2xyz(mydf2, data.frame = TRUE)

ggplot(data = gdat, aes(x = x, y = y, z = z)) + 
  geom_tile(aes(fill = z)) + 
  # stat_contour(aes(fill = ..level..), geom = "polygon", binwidth = 0.007) +
  # geom_contour(color = "white") +
  geom_path(data = worldmap, aes(x = long, y = lat,group=group), inherit.aes = FALSE) + #contour des pays
  scale_x_continuous(limits = c(-20,30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(30,70), expand = c(0, 0)) +
  # scale_fill_gradientn(colors = c("white", "lightblue", "yellow", "red", "purple")) +
  scale_fill_distiller(palette='RdBu')#+
  # scale_fill_continuous(type="spectral")+
  # coord_equal() +
  # theme_bw()

plot_nc(getwd(),"era5_z500_10_1956-02-09_1956-02-19.nc","z500")

data(wrld_simpl)
worldmap <- fortify(wrld_simpl)
cfname <- paste(ncpath, ncname,".nc",sep="")
nc_df <- as.data.frame(as(brick(ncfname)[[1]],"SpatialPixelsDataFrame"))
colnames(nc_df) <- c("value", "x", "y")
nc_df2 <- with(nc_df, interp(x = x, y = y, z = value, xo = seq(min(x), max(x), length = 400),duplicate = "mean"))
gdat <- interp2xyz(nc_df2, data.frame = TRUE)
xmin <- min(nc_df$x)
xmax <- max(nc_df$x)
ymin <- min(nc_df$y)
ymax <- max(nc_df$y)
zmin <- min(nc_df$value)
zmax <- max(nc_df$value)
p <- ggplot(data = gdat, aes(x = x, y = y, z = z)) + 
  geom_tile(aes(fill = z)) + 
  # stat_contour(aes(fill = ..level..), geom = "polygon", binwidth = 0.007) + #contour des champs de z
  # geom_contour(color = "white") +
  geom_path(data = worldmap, aes(x = long, y = lat,group=group), inherit.aes = FALSE) + #contour des pays
  scale_x_continuous(limits = c(xmin,xmax), expand = c(0, 0)) +
  scale_y_continuous(limits = c(ymin,ymax), expand = c(0, 0)) +
  # scale_fill_continuous(expand = c(0, 0))+m
  # scale_fill_gradientn(colors = c("white", "lightblue", "yellow", "red", "purple")) +
  scale_fill_distiller(palette='RdBu',expand=c(0,0))#+

# coord_equal() +
# theme_bw()
p








