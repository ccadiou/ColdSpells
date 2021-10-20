library(ncdf4)
library(sp)
library(maptools)
# library(viridis)        # this package has nice colormap palettes
library(fields)         # this package has image.plot 
library(RColorBrewer)

# set path and filename
ncpath <- "~/Documents/Etudes/Ponts 3A/IPSL/Code/"
ncname <- "era5_msl_fr_anomalie"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "msl"

# open a netCDF file
ncin <- nc_open(ncfname)
# print(ncin)

#get variables
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
# head(lon)

lat <- ncvar_get(ncin, "lat", verbose = F)
lat <- rev(lat)
nlat <- dim(lat)
# head(lat)

# print(c(nlon, nlat))

t <- ncvar_get(ncin, "time")
nt <- dim(t)
# head(t)

y <- focal(slp_array, w=matrix(1, 5, 5), mean)
plot(slp_array,interpolate=TRUE)

slp_array <- ncvar_get(ncin, dname)
slp_array <- slp_array[,dim(lat):1]
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(slp_array)

nc_close(ncin)

border <- max(abs(min(slp_array)),abs(max(slp_array))) # set border to center color scale
test <- disaggregate(slp_array, 5)

palette = brewer.pal(11,'RdBu')
#Display an register image
# png(filename="slp.png",bg="white")
image.plot(lon, lat, slp_array,
           # main = "Sea Level Pressure",
           xlab = "Longitude",
           ylab = "Latitude",
           legend.lab = "SLP anomalies [Pa]",
           # legend.line = 2.5,
           lwd=0,
           breaks=seq(-border,border,length.out=12), # center color scale
           # col = rev(viridis(200)))  # To get the higher emission values darker we invert it
           col = rev(palette)) # To get the higher emission values darker we invert it
           # rcolorbrewer# col = rev(Spectral(200)))  # To get the higher emission values darker we invert it
data(wrld_simpl)  #displays countries' borders
plot(wrld_simpl, add = TRUE)
# mtext("Subtitle")  # To put a sub-title
# dev.off()


library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))


