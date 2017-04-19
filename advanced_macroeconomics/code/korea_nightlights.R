## Plot night-lights Korea

# Load libraries
library(RCurl)
library(R.utils)
library(rgdal)
library(raster)

# Set parameters for download
url_radianceCalibrated="ftp://ftp.ngdc.noaa.gov/DMSP/web_data/x_rad_cal/rad_cal.tar"
calibratedLights="rad_cal.tar"
hiResTif="world_avg.tif"

# Download and unpack
download.file(url_radianceCalibrated,calibratedLights,mode="wb")
untar(calibratedLights)
gunzip(paste(hiResTif, '.gz', sep=''))

# Create raster
hiResLights=raster("world_avg.tif")       

# Adjust coordinates       
xmax(hiResLights) = 180
ymin(hiResLights) = -90

# Download maps for North- and South-Korea
prk=getData('GADM', country="PRK", level=0)
kor=getData('GADM', country="KOR", level=0)

# Get extent for data
extent(prk)
extent(kor)
e<-extent(123,131,33,44)

# Crop data
r=crop(hiResLights,e)

# Plot
par(mar=c(0,0,0,0))
plot(r,col=grey.colors(30),axes=FALSE,box=FALSE,bty="n",legend=FALSE)
plot(kor,border="White",add=T)
plot(prk,border="White",add=T)
