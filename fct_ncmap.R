plot_nc <- function(path,fname,varname){
  require(ggplot2)
  require(akima)
  require(raster)
  require(maptools)
  
  ncfname <- paste(path, fname, sep="")
  nc_df <- as.data.frame(as(brick(ncfname)[[1]],"SpatialPixelsDataFrame"))
  colnames(nc_df) <- c("value", "x", "y")
  nc_df2 <- with(nc_df, interp(x = x, y = y, z = value, xo = seq(min(x), max(x), length = 400),duplicate = "mean"))
  gdat <- interp2xyz(nc_df2, data.frame = TRUE)
  p <- ggplot(data = gdat, aes(x = x, y = y, z = z)) + 
    geom_tile(aes(fill = z)) + 
    # stat_contour(aes(fill = ..level..), geom = "polygon", binwidth = 0.007) + #contour des champs de z
    # geom_contour(color = "white") +
    geom_path(data = worldmap, aes(x = long, y = lat,group=group), inherit.aes = FALSE) + #contour des pays
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_z_continous(expand=c(0,0))+
    # scale_fill_gradientn(colors = c("white", "lightblue", "yellow", "red", "purple")) +
    scale_fill_distiller(palette='RdBu')#+
    # coord_equal() +
    # theme_bw()
  return(p)
}
nc_to_array<- function(path,fname,varname){
  require(ncdf4) # package for netcdf manipulation
  nc_data <- nc_open(paste(path,fname,sep=""))
  
  #get variables
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  # time <- ncvar_get(nc_data, "time")
  nc.array <- ncvar_get(nc_data, varname) # store the data in a 3-dimensional array
  nc_close(nc_data) 
  
  return(nc.array)
}

plot_nc_raster_from_file <- function(path,fname,varname,varmin,varmax){
  require(ncdf4) # package for netcdf manipulation
  require(raster) # package for raster manipulation
  require(maptools) # for adding countries
  #load data
  nc_data <- nc_open(paste(path,fname,sep=""))
  
  #get variables
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  nc.array <- ncvar_get(nc_data, varname) # store the data in a 3-dimensional array
  nc_close(nc_data) 
  
  #convert to raster
  r <- raster(t(nc.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))# +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction='y')
  #plot
  # > substr("era5_z500_10_1956-02-09_1956-02-19.nc",6,34)
  # [1] "z500_10_1956-02-09_1956-02-19"
  # > substr("era5_z500_10_1956-02-09_1956-02-19.nc",14,34)
  # [1] "1956-02-09_1956-02-19"
  plot(r,col=colorRampPalette(colors=c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7',
                                                        '#fddbc7','#f4a582','#d6604d','#b2182b','#67001f'))(255),
       zlim=c(varmin,varmax), legend=FALSE,main=substr(fname,6,34))
  rgeos::set_RGEOS_CheckValidity(2L)
  data(wrld_simpl, package="maptools")
  eu_simpl <- crop(wrld_simpl,extent(min(lon),max(lon),ymn=min(lat),ymx=max(lat)))  #for countries borders
  plot(eu_simpl,add=TRUE)
}

plot_array_raster <- function(array,varmin,varmax){
  require(raster) # package for raster manipulation
  require(maptools) # for adding countries
  #load data
  data(wrld_simpl) #for countries borders
  nc_data <- nc_open(paste(path,fname,sep=""))
  
  #get variables
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  ndvi.array <- ncvar_get(nc_data, varname) # store the data in a 3-dimensional array
  nc_close(nc_data) 
  
  #convert to raster
  r <- raster(t(ndvi.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat"))# +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction='y')
  #plot
  plot(r,col=colorRampPalette(colors=c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7',
                                       '#fddbc7','#f4a582','#d6604d','#b2182b','#67001f'))(255),
       zlim=c(varmin,varmax), xlab = "Longitude",ylab = "Latitude",legend.lab = "z500 [Pa]",)
  plot(wrld_simpl, add = TRUE)
}
