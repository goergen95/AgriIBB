library(rgdal)
library(ggplot2)



regions = readOGR("../results/shapes/regions.shp")
points = readOGR("../results/shapes/random_points.shp")


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


active = colMeans(points@data[points$active==1,4:26])
inactive = colMeans(points@data[points$active==0,4:26])

data =  as.data.frame(t(rbind(active,inactive)))
data$SDactive = as.vector(apply(points@data[points$active==1,4:26],2,sd,na.rm=TRUE))
data$SDinactive = as.vector(apply(points@data[points$active==0,4:26],2,sd,na.rm=TRUE))
data$DOY = seq(001,353,16)

active_pixels = ggplot(data=data)+
  geom_line(aes(x=DOY,y=active/10000,group=1),color="olivedrab3",size=1.4)+
  geom_line(aes(x=DOY,y=(active+SDactive)/10000,group=1),linetype=2,color="olivedrab3",size=1.5)+
  geom_line(aes(x=DOY,y=(active-SDactive)/10000,group=1),linetype=2,color="olivedrab3",size=1.5)+
  labs(y="NDVI value",x="Day of the year 2016")+
  theme_minimal(base_size = 18)+
  ylim(0,1)

png("../results/plots/active_pixels.png",1800,900)
active_pixels
dev.off()

inactive_pixels = ggplot(data=data)+
  geom_line(aes(x=DOY,y=inactive/10000,group=1),color="indianred4",size=1.4)+
  geom_line(aes(x=DOY,y=(inactive+SDactive)/10000,group=1),linetype=2,color="indianred4",size=1.5)+
  geom_line(aes(x=DOY,y=(inactive-SDactive)/10000,group=1),linetype=2,color="indianred4",size=1.5)+
  labs(y="NDVI value",x="Day of the year 2016")+
  theme_minimal(base_size = 18)+
  ylim(0,1)

png("../results/plots/inactive_pixels.png",1800,900)
inactive_pixels
dev.off()
