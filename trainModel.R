# script to set up a random forest model based on forward-feature selection and cross-validation
library(rgdal)
library(carte)
library(doParallel)
library(CAST)
# specifiy number of cores for parallell processing
cores = 7

# read in needed files
points = readOGR("../results/shapes/random_points.shp")
predNames = readRDS("../results/prediction/predNames.rds")

# randomly sample 50 percent of points for training and validation
# set seed is used to ensure reproducibility
N0 = length(points@data$active[points@data$active==0]) # number of inactive pixels
N1 = length(points@data$active[points@data$active==1]) # number of active pixels
set.seed(834289348)
smp0 = sample(N0,0.5*N0) # randomly select 50 percent of inactive pixels
set.seed(082345907234)
smp1 = sample(N1,0.5*N1) # randomly select 50 percent of active pixels

# spliting the data in training and testing
training0 = points@data[which(points@data$active==0),][smp0,2:ncol(points@data)]
testing0 = points@data[which(points@data$active==0),][-smp0,2:ncol(points@data)]
training1 = points@data[which(points@data$active==1),][smp1,2:ncol(points@data)]
testing1= points@data[which(points@data$active==1),][-smp1,2:ncol(points@data)]

training = rbind(training0,training1)
testing = rbind(testing0,testing1)

# using the CAST package to achieve space-dependend folds for the cross-validation
index = CAST::CreateSpacetimeFolds(training,spacevar = "region",k=10,seed=320543)
tc = caret::trainControl(method="cv",number=10,classProbs = TRUE, index=index$index,indexOut=index$indexOut)

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




