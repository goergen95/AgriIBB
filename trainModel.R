# script to set up a random forest model based on forward-feature selection and cross-validation
library(rgdal)
library(carte)
library(doParallel)
library(CAST)

# read in prediction data stored in spatial point data frame
points = readOGR("../results/shapes/random_points.shp")

predNames = readRDS("../results/prediction/predNames.rds")

# random sample 50 percent of points for training and validation
N0 = length(points@data$active[points@data$active==0])
N1 = length(points@data$active[points@data$active==1])
smp0 = sample(N0,0.5*N0)
smp1 = sample(N1,0.5*N1)



training0 = points@data[which(points@data$active==0),][smp0,2:ncol(points@data)]
testing0 = points@data[which(points@data$active==0),][-smp0,2:ncol(points@data)]

training1 = points@data[which(points@data$active==1),][smp1,2:ncol(points@data)]
testing1= points@data[which(points@data$active==1),][-smp1,2:ncol(points@data)]

training = rbind(training0,training1)
testing = rbind(testing0,testing1)

index = CAST::CreateSpacetimeFolds(training,spacevar = "region",k=10,seed=320543)


tc = caret::trainControl(method="cv",number=5,classProbs = TRUE, index=index$index,indexOut=index$indexOut)


training$active = make.names(training$active)
testing$active = make.names(testing$active)
# training the model

cl =  parallel::makeCluster(7)
doParallel::registerDoParallel(cl)
rfModel = CAST::ffs(training[,predNames], training$active, method = "rf", withinSE = FALSE,importance = TRUE, trControl  = tc, metric = "Kappa")
stopCluster(cl)



pred = predict(rfModel,testing)
caret::confusionMatrix(pred,as.factor(testing$active))


saveRDS(rfModel,file="../results/prediction/rfModel.rds")




