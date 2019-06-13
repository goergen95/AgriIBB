# script to set up a random forest model based on forward-feature selection and cross-validation
library(rgdal)
library(carte)
library(doParallel)
library(CAST)
library(splitstackshape)
# specifiy number of cores for parallell processing
cores = 7

# read in needed files
points = readOGR("../results/shapes/random_points.shp")
predNames = readRDS("../results/prediction/predNames.rds")

# stratified random sample 50 percent of points for training and validation
# based on region and activity status
# set seed is used to ensure reproducibility
set.seed(834289348)
training = as.data.frame(stratified(points@data,c("region","active"),0.5))
testing = points@data[-training$id,]

# using the CAST package to achieve space-dependend folds for the cross-validation (leave-area-out)
index = CAST::CreateSpacetimeFolds(training,spacevar = "region",k=length(unique(training$region)),seed=320543)#
tc = caret::trainControl(method="cv",number=length(unique(training$region)),classProbs = TRUE, index=index$index,indexOut=index$indexOut)

# ensure random forest is done as classification
training$active = make.names(training$active)
testing$active = make.names(testing$active)

# finally training the model, also with parallel processing to increase speed of computations
cl =  parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
rfModel = CAST::ffs(training[,predNames], training$active, method = "rf", withinSE = FALSE,importance = TRUE, trControl  = tc, metric = "Kappa")
stopCluster(cl)



pred = predict(rfModel,testing)
caret::confusionMatrix(pred,as.factor(testing$active))


saveRDS(rfModel,file="../results/prediction/rfModel.rds")




