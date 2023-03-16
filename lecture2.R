library(terra)

myVariables <- terra::rast("data/variables.tif")
myVariables
terra::plot(myVariables)
terra::plot(myVariables$ndvi)
terra::plot(myVariables[[4]])
terra::plot(myVariables[['ndvi']])

originalNames <- names(myVariables)
names(myVariables) <- c('A','B','C','D')
names(myVariables) <- originalNames

terra::plot((myVariables$ndvi/1000)-1)

# rescaling variables according
meanTemp <- myVariables$MeanTempX100/100
stdDevTemp <- myVariables$StdDevTempX100/100
ndvi <- (myVariables$ndvi/1000)-1

myVarRescale <- c(myVariables$Elevation+0, # trick to don't loss data
                  meanTemp,
                  stdDevTemp,
                  ndvi)
terra::plot(myVarRescale)


terra::crs(myVarRescale)
myVarRescaleReprojected <- terra::project(myVarRescale, 'epsg:3035')
terra::plot(myVarRescaleReprojected) #from lat/long to meters

# saving
# pdf("varPlot.pdf")
# terra::plot(myVarRescale)
# terra::hist(myVarRescale$MeanTempX100)
# dev.off()
# 
# png("ndviPlot.png", width = 2400, height = 1800, res=400)
# terra::hist(myVarRescale$ndvi)
# dev.off()

# table conversion
vector.raster.ndvi <- terra::values(myVarRescale$ndvi)
vector.raster.elevation <- myVarRescale$Elevation[]

vector.raster.matrix <- myVarRescale[] #transform the whole band stack into matrix
vector.raster.table <- as.data.frame(myVarRescale[]) #turn it into a dataframe

# correlation and linear modelling
correlation.table <- cor(vector.raster.matrix)
lm.fit <- lm(vector.raster.table$MeanTempX100~vector.raster.table$Elevation) # x is dependent variable and y are the independent variable
lm.fit
summary(lm.fit)


png('correlationPlot.png', width = 2000, height = 2000, res = 300)
 smoothScatter( vector.raster.table$Elevation, vector.raster.table$MeanTempX100)
 abline(lm.fit, lwd=2, col='red')
dev.off()

#### sampling ----
library(autocart)
?autocart

nsamples <- 2000
sampled.grid <- terra::spatSample(myVarRescale, 
                           size = nsamples, 
                           method = 'random', #if we put regular, we can't give a specific number of nsamples
                           cells = T)


sampled.grid$longitude <- terra::xFromCell(myVarRescale, sampled.grid$cell)
sampled.grid$latitude  <- terra::yFromCell(myVarRescale, sampled.grid$cell)

response <- as.matrix(sampled.grid$ndvi) # what to predict
predictors <- sampled.grid
predictors$ndvi <- NULL

locations <- as.matrix(cbind(sampled.grid$longitude, sampled.grid$latitude))

numtraining <- round(0.5 * nsamples)
training_index <- rep(FALSE, nsamples) # create a large vector full of false
training_index[1:numtraining] <- TRUE # give value of TRUE to all the rows used for treaining and FALSE for testing
training_index <- sample(training_index) # 

train_response <- response[training_index]
test_response <- response[!training_index]
train_predictors <- predictors[training_index, ]
test_predictors <- predictors[!training_index, ]
train_locations <- locations[training_index, ]
test_locations <- locations[!training_index, ]
