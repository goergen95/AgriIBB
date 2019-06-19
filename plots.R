library(rgdal)
library(ggplot2)

regions = readOGR("../results/shapes/regions.shp")
points = readOGR("../results/shapes/random_points.shp")
NDVI = stack(list.files("../results/savG/layered/",pattern="2016",full.names = T))
map2016 = raster("../results/prediction/activitiy_2016.tif")

chn = colSums(regions@data[regions$ISO=="CHN",34:47])
kaz = colSums(regions@data[regions$ISO=="KAZ",34:47])
data = as.data.frame(t(rbind(chn,kaz)))
data$errorCplus = data$chn + 146925
data$errorCminus = data$chn - 146925
data$dates = 2003:2016


active = ggplot(data=data)+
  geom_line(aes(x=dates, y=chn/1000), color="red",size=1.4)+
  geom_line(aes(x=dates, y=kaz/1000), color="blue",size=1.4)+
  geom_errorbar(aes(x=dates,ymin=(chn-146925)/1000, ymax=(chn+146925)/1000), width=.2, color="red")+
  geom_errorbar(aes(x=dates,ymin=(kaz-146925)/1000, ymax=(kaz+146925)/1000), width=.2, color="blue")+
  labs(y="active area (in 1000 acres)",
         x="year")+
  theme_minimal(base_size = 18)

png("../results/plots/activity_plot.png",width=1800,height=900,res=100)
active
dev.off()

temp = colMeans(regions@data[,5:18],na.rm=TRUE)
prec = colMeans(regions@data[,19:32],na.rm=TRUE)
climate = as.data.frame(t(rbind(temp,prec)))
climate$year = 2003:2016


climatePlot = ggplot(data=climate)+
  geom_bar(aes(x=year,y=prec),fill="blue",stat = "identity")+
  geom_line(aes(x=year,y=temp*80),color="red",size=1.5)+
  scale_y_continuous(sec.axis = sec_axis(~./80, name = "Temperature (°C)"))+
  labs(y = "Percipitation Sums (mm)",
       x = "year")+
  theme_minimal(base_size = 18)

png("../results/plots/climate.png",width=1800,height=900,res=100)
climatePlot
dev.off()


active = map2016
active[active==1] = NA
inactive = map2016
inactive[inactive==2] = NA
inactive[!is.na(inactive)] = 1

activeNDVI = NDVI[active]
inactiveNDVI = NDVI[inactive]

activeMeans = colMeans(points@data[points$active==1,4:26])
inactiveMeans = colMeans(points@data[points$active==0,4:26])

data =  as.data.frame(t(rbind(activeMeans,inactiveMeans)))
data$SDactive = as.vector(apply(activeNDVI,2,sd,na.rm=TRUE))
data$SDinactive = as.vector(apply(inactiveNDVI,2,sd,na.rm=TRUE))
data$DOY = seq(001,353,16)

active_pixels = ggplot(data=data)+
  geom_line(aes(x=DOY,y=activeMeans/10000,group=1),color="olivedrab3",size=1.4)+
  geom_line(aes(x=DOY,y=(activeMeans+SDactive)/10000,group=1),linetype=2,color="olivedrab3",size=1.5)+
  geom_line(aes(x=DOY,y=(activeMeans-SDactive)/10000,group=1),linetype=2,color="olivedrab3",size=1.5)+
  labs(y="NDVI value",x="Day of the year 2016")+
  theme_minimal(base_size = 18)+
  ylim(0,1)

png("../results/plots/active_pixels.png",1800,900)
active_pixels
dev.off()

inactive_pixels = ggplot(data=data)+
  geom_line(aes(x=DOY,y=inactiveMeans/10000,group=1),color="indianred4",size=1.4)+
  geom_line(aes(x=DOY,y=(inactiveMeans+SDactive)/10000,group=1),linetype=2,color="indianred4",size=1.5)+
  geom_line(aes(x=DOY,y=(inactiveMeans-SDactive)/10000,group=1),linetype=2,color="indianred4",size=1.5)+
  labs(y="NDVI value",x="Day of the year 2016")+
  theme_minimal(base_size = 18)+
  ylim(0,1)

png("../results/plots/inactive_pixels.png",1800,900)
inactive_pixels
dev.off()


prec = na.omit(regions@data[,c(1,19:32)])
active = na.omit(regions@data[,c(1,34:47)])
prec = prec[-which(active$ac_2003==0),]
active=active[-which(active$ac_2003==0),]
plot(prec,active)

