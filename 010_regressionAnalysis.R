# this scripts creates plots and calculates statistics for the analysis of 
# land use activity in the IBB
library(raster)
library(rgdal)
library(ggplot2)


# read in needed files
regions = readOGR("../results/shapes/regions.shp")
years=2003:2016
tmpNames = paste("tmp",years,sep="")
prcNames = paste("prc",years,sep="")
activeNames = paste("ac_",years,sep="")
inactiveNames=paste("in_",years,sep="")
fields = readOGR("../results/shapes/fields.shp")


namesEmpty = regions$regions[regions$cropAre==0]
regionNames = unique(regions@data$regions)


resultsPREC = data.frame(region=regions@data$regions,variable=rep("prec",35),coef=rep(0,35),rsquared=rep(0,35),pvalue=rep(0,35))
for (region in regionNames){
  if(region %in% namesEmpty) next
  print(region)
  mod = lm(as.numeric(regions@data[regions$regions==region,activeNames]) ~ as.numeric(regions@data[regions$regions==region,prcNames]))
  s = summary(mod)
  coef = s$coefficients[2,1]
  rsquared = round(s$r.squared,3)
  pvalue = s$coefficients[2,4]
  resultsPREC[resultsPREC$region==region,3:5] = c(coef,rsquared,pvalue)
  
}
resultsPREC = na.omit(resultsPREC)
resultsPREC = resultsPREC[-which(resultsTMP$coef==0),]

resultsTMP = data.frame(region=regions@data$regions,variable=rep("temp",35),coef=rep(0,35),rsquared=rep(0,35),pvalue=rep(0,35))
for (region in regionNames){
  if(region %in% namesEmpty) next
  print(region)
  mod = lm(as.numeric(regions@data[regions$regions==region,activeNames]) ~ as.numeric(regions@data[regions$regions==region,tmpNames]))
  s = summary(mod)
  coef = s$coefficients[2,1]
  rsquared = round(s$r.squared,3)
  pvalue = s$coefficients[2,4]
  resultsTMP[resultsTMP$region==region,3:5] = c(coef,rsquared,pvalue)
  
}

resultsTMP = na.omit(resultsTMP)
resultTMP = resultsTMP[-which(resultsTMP$coef==0),]

# large scale regression analysis
ALL = colSums(regions@data[,5:63],na.rm=TRUE)
KAZ = colSums(regions@data[regions$ISO=="KAZ",5:63],na.rm = T)
CHN = colSums(regions@data[regions$ISO=="CHN",5:63],na.rm = T)

global = data.frame(region=c("all","all","KAZ","CHN","KAZ","CHN"),variable=c("prec","temp","prec","prec","temp","temp"),coef=rep(0,6),rsquared=rep(0,6),pvalue=rep(0,6))

mod = lm(as.numeric(ALL[activeNames]) ~ as.numeric(ALL[prcNames]))
s = summary(mod)
global$coef[1] = s$coefficients[2,1]
global$rsquared[1]= round(s$r.squared,3)
global$pvalue[1]= s$coefficients[2,4]

mod = lm(as.numeric(ALL[activeNames]) ~ as.numeric(ALL[tmpNames]))
s = summary(mod)
global$coef[2] = s$coefficients[2,1]
global$rsquared[2]= round(s$r.squared,3)
global$pvalue[2]= s$coefficients[2,4]

mod = lm(as.numeric(KAZ[activeNames]) ~ as.numeric(KAZ[prcNames]))
s = summary(mod)
global$coef[3] = s$coefficients[2,1]
global$rsquared[3]= round(s$r.squared,3)
global$pvalue[3]= s$coefficients[2,4]

mod = lm(as.numeric(CHN[activeNames]) ~ as.numeric(CHN[prcNames]))
s = summary(mod)
global$coef[4] = s$coefficients[2,1]
global$rsquared[4]= round(s$r.squared,3)
global$pvalue[4]= s$coefficients[2,4]

mod = lm(as.numeric(KAZ[activeNames]) ~ as.numeric(KAZ[tmpNames]))
s = summary(mod)
global$coef[5] = s$coefficients[2,1]
global$rsquared[5]= round(s$r.squared,3)
global$pvalue[5]= s$coefficients[2,4]

mod = lm(as.numeric(CHN[activeNames]) ~ as.numeric(CHN[tmpNames]))
s = summary(mod)
global$coef[6] = s$coefficients[2,1]
global$rsquared[6]= round(s$r.squared,3)
global$pvalue[6]= s$coefficients[2,4]

results = rbind(global,resultsPREC,resultTMP)
results = results[-which(results$coef==0),]
write.csv(results,file="../results/regression/stats_results.csv")

