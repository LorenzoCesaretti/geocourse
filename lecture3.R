#### lead data ----
library(terra)
myVariables <- terra::rast("data/variables.tif")
meanTemp <- myVariables$MeanTempX100/100
stdDevTemp <- myVariables$StdDevTempX100/100
ndvi <- (myVariables$ndvi/1000)-1

myVarRescale <- c(myVariables$Elevation+0, # trick to don't loss data
                  meanTemp,
                  stdDevTemp,
                  ndvi)
terra::plot(myVarRescale)

#### sampling ----
library(autocart)
??autocart

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

#internal parameter spatial autocorrelation (distance dependent)
alpha <- 0.60 
beta <- 0.20
my_control <- autocartControl(distpower = 2)

# build the model
ndvi_model <- autocart(train_response,    
                       train_predictors, 
                       train_locations, 
                       alpha, beta, 
                       my_control)


# predict the values using for test the model
test_predictions <- predictAutocart(ndvi_model,
                                    test_predictors) 

residuals <- test_response - test_predictions

# RMSE
sqrt(mean(residuals^2))

hist(residuals)

plot(test_response, test_predictions)
abline(0,1, col='red', lwd=2)

# ### predict a new map of ndvi
# 
# test_predictions <- predictAutocart(ndvi_model,
#                                     test_predictors) 
# 
# myVarRescale$longitude <- terra::xFromCell(myVarRescale, 1:terra::())
# myVarRescale$latitude  <- terra::yFromCell(myVarRescale, sampled.grid$cell)
# 
# 
# myVarRescaleSmall <- terra::aggregate(myVarRescale, 10)
# terra::res(myVarRescaleSmall)
# 
# predicted_raster <- predictAutocart(ndvi_model, 
#                                     myVarRescaleSmall[])
# 
