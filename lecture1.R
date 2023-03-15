print('hello')
mynumber <- 12
mymassege <- 'hello'

class(mynumber)
class(mymassege)

#IMPORT LIBRARIES ####
library(terra)

#### OPEN MANUAL OF COMMAND ----
?terra::align
?terra::rast

# Create a SpatRaster from sctatch
x <- rast(nrows=108, ncols=21, xmin=0, xmax=10)

# Create a SpatRaster from a file
f <- system.file("ex/elev.tif", package = "terra")
r <- rast(f)
plot(r)
terra::plot(r)

#### READ DTM RASTER ####
r <- terra::rast("mySRTM.tif")
terra::plot(r)

