library(ncdf4)
library(sp)
library(maptools)
library(viridis)        # this package has nice colormap palettes
library(fields)         # this package has image.plot 

# set path and filename
ncpath <- "~/Documents/Etudes/Ponts 3A/IPSL/Code/"
ncname <- "slp_natl_anomalie"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "slp"

# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

#get variables
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "lat", verbose = F)
lat <- rev(lat)
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))

t <- ncvar_get(ncin, "time")
nt <- dim(t)
head(t)

slp_array <- ncvar_get(ncin, dname)
slp_array <- slp_array[,dim(lat):1]
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(slp_array)

nc_close(ncin)

#Display an register image
png(filename="slp.png",bg="white")
image.plot(lon, lat, slp_array,
           main = "Sea Level Pressure",
           xlab = "Longitude",
           ylab = "Latitude",
           legend.lab = "Pa",
           legend.line = 2.5,
           col = rev(viridis(200)))  # To get the higher emission values darker we invert it
data(wrld_simpl)  #displays countries' borders
plot(wrld_simpl, add = TRUE)
# mtext("Subtitle")  # To put a sub-title
dev.off()
