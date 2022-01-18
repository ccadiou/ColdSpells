
# source("~/Documents/These/Code/Winter/fct_ncmap.R")
source("~/Documents/These/Code/Winter/fct_plotmap.R")
source("fct_plot.R")
library(ncdf4)

##########
# Plot anomalie + courbes de geopt 
##########
###### Comparaison obs/simulations #######
# Simulations 1963
path_sim <- "~/Documents/These/Data/Winter/"
ncname_sim <- "z500_1963_sim.nc"
ncin_sim <- nc_open(paste(path_sim,ncname_sim,sep=""))
lon_sim <- ncvar_get(ncin_sim, "lon")
lat_sim <- ncvar_get(ncin_sim, "lat", verbose = F)
nc_close(ncin_sim)
nc_sim <- nc_to_array(path_sim,ncname_sim,"z500")

ncname_anomalie_sim <- "z500_1963_sim_anomalie.nc"
nc_anomalie_sim <- nc_to_array(path_sim,ncname_anomalie_sim,"z500")

max <- max(nc_anomalie_sim)
min <- -max
plot_ano.c(lon_sim,lat_sim,nc_sim/100,nc_anomalie_sim,min(nc_anomalie_sim),max(nc_anomalie_sim),title="1963_sims")

# Réanalyses 1963
path <- "~/Documents/These/Data/Winter/submeans_z500_largefield/"
ncname <- "era5_z500_90_1962-12-01_1963-02-28.nc"
ncin <- nc_open(paste(path,ncname,sep=""))
lon <- ncvar_get(ncin, "lon")
lat <- ncvar_get(ncin, "lat", verbose = F)
nc_close(ncin)
nc <- nc_to_array(path,ncname,"z500")

ncpath_anomalie <- "~/Documents/These/Data/Winter/submeans_z500_anomalie_largefield/"
ncname_anomalie <- "era5_z500_90_1962-12-01_1963-02-28_anomalie.nc"
nc_anomalie <- nc_to_array(ncpath_anomalie,ncname_anomalie,"z500")

plot_ano.c(lon,lat,nc/100,nc_anomalie,min(nc_anomalie),max(nc_anomalie),title="1963_sims")


#plot both
par(mfcol=c(1,2))
plot_ano.c(lon_sim,lat_sim,nc_sim/100,nc_anomalie_sim,min,max,title="1963_WEGE")
plot_ano.c(lon,lat,nc/100,nc_anomalie,min,max,title="1963_ERA5")


### Set of files, 10 records by time range #### 
# path and filename
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

dev.off() #Minimum sur 3 jours
min <- min(unlist(ncs[21:29]))
max <- max(unlist(ncs[21:29]))
par(mfcol=c(3,3))
lapply(c(21:29),function(i) plot_ano.c(lon,lat,ncs[[i]]/100,ncs_anomalie[[i]],
                                       min,max,
                                       title=toString(as.numeric(substring(ncnames[[i]],14,17))-1)))
dev.off() #Minimum sur 10 jours
min <- min(unlist(ncs[1:9]))
max <- max(unlist(ncs[1:9]))
par(mfcol=c(3,3))#,mar=c(-1,0,0,0))
lapply(c(1:9),function(i) plot_ano.c(lon,lat,ncs[[i]]/100,ncs_anomalie[[i]],
                                       min,max,
                                       title=toString(as.numeric(substring(ncnames[[i]],14,17))-1)))
dev.off() #Minimum sur 30 jours
min <- min(unlist(ncs[11:19]))
max <- max(unlist(ncs[11:19]))
par(mfcol=c(3,3))
lapply(c(11:19),function(i) plot_ano.c(lon,lat,ncs[[i]]/100,ncs_anomalie[[i]],
                                       min,max,
                                       title=toString(as.numeric(substring(ncnames[[i]],14,17))-1)))
dev.off() #Minimum sur 90 jours
min <- min(unlist(ncs[31:39]))
max <- max(unlist(ncs[31:39]))
par(mfcol=c(3,3))
lapply(c(31:39),function(i) plot_ano.c(lon,lat,ncs[[i]]/100,ncs_anomalie[[i]],
                                       min,max,
                                       title=toString(as.numeric(substring(ncnames[[i]],14,17))-1)))

###########
#Ncdf maps of few files without anomalie
###########
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
