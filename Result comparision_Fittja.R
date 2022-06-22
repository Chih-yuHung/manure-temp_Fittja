library(ggplot2)
library(tidyverse)
library(dplyr)
library(hydroGOF) #NSE 

#Sweden project-Fittja
#To compare my simulation result to the measured data

#output to an excel file
Envir.daily<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/2. Method/input/daily env input_Fittja_June15.csv",header=T)
temp<-(Envir.daily$AirTmax1+Envir.daily$AirTmin1)/2 #Air Temp.avg
temp<-temp[c(1:365,1:1095)]
#observed data
obs.Fittja<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/temp.Fittja.daily.csv",header=T) 
obs.Fittja<-obs.Fittja[rep(c(1:365),4),]
#simulated data before calibration
sim.Fittja.og<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/Fittja2022-06-22.csv",header=T) 
#simulated data after calibration and modification
sim.Fittja<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/with shade/Fittja2022-06-22.csv",header=T)

#Draw the last year only
temp<-temp[1096:1460]
obs.Fittja<-obs.Fittja[c(1096:1460),]
sim.Fittja.og<-sim.Fittja.og[c(731:1095),]
sim.Fittja<-sim.Fittja[c(1096:1460),]

par(mfrow=c(3,1),mar=c(2,4,2,1))
#For air temperature and soil temperature
# plot(temp,type="l",xaxt='n',col="gray",ylim=c(-15,30)) #Air temperature
# lines(result.m$Stemp50,col="red") #measured soil temperature at 50 cm
# lines(result.m$temp0.5,col="red",lwd=2,lty=2) # measured manure at 0.5 m
# lines(result.adjust$temp0.5.sim,col="blue",lwd=2,lty=2) # simulate manure at 0.5 m
# legend(10,29,c("Air temp","Soil temp at 0.5m","Tm-measured at 0.5m","Tm-model at 0.5m")
#        ,col=c("grey","red","red","blue")
#        ,lty=c(1,1,2,2),lwd=2,ncol=4,bty="n")
# Axis(side=1, at=c(16,130,209,324,381,495,574,689,746,860,939,1054,1111,1225,1304,1419)
#      ,labels=rep(c("7/1","10/12","1/1","4/25"),4))


#For measured manure temperature
plot(temp,type="l",xaxt='n',col="gray",ylim=c(-15,30)) #Air temperature
lines(obs.Fittja$temp.avg,type="l") #manure avg. obs temperature
lines(sim.Fittja.og$Temperature.C,type="l",col="blue",lwd=2) #without shade calibration
lines(sim.Fittja$Temperature.C,col="red",lwd=2)
legend(10,29,c("Tair","Tm-meausred","Tm-model og","Tm-model shade"),col=c("grey","black","blue","red")
       ,lty=1,lwd=2,ncol=2,bty="n")
Axis(side=1, at=c(1,93,185,277)
     ,labels=c("5/1","8/1","11/1","2/1"))


#For measured manure temperature with cover (avg.)
plot(temp,type="l",xaxt='n',col="gray",ylim=c(-15,30)) #Air temperature
lines(obs.cover$temp.avg,type="l") #manure avg. obs temperature
lines(sim.cover$Temperature.C,col="red",lwd=2)
legend(10,29,c("Tair","Tm-meausred","Tm-model working"),col=c("grey","black","red")
       ,lty=1,lwd=2,ncol=4,bty="n")
Axis(side=1, at=c(16,130,209,324,381,495,574,689,746,860,939,1054,1111,1225,1304,1419)
     ,labels=rep(c("7/1","10/12","1/1","4/25"),4))



#For plots measured manure temperature (0.5)
plot(temp,type="l",xaxt='n',col="gray",ylim=c(-15,30),lwd=2,xlab="Date") #Air temperature
lines(result.m$temp0.5,col="orange",lwd=2) # measured manure at 0.5 m
lines(result.adjust$temp0.5.sim,col="blue",lwd=2,lty=2) # simulate manure at 0.5 m
legend(0,29,c("Air temp","Tm-measured at 0.5m ","Tm-model at 0.5m"),col=c("grey","orange","blue")
       ,lty=1,lwd=2,ncol=4,bty="n")
Axis(side=1, at=c(16,130,209,324,381,495,574,689,746,860,939,1054,1111,1225,1304,1419)
     ,labels=rep(c("7/1","10/12","1/1","4/25"),4))

plot(result.adjust1.0$Temperature.C,type="l",xaxt='n',col="gray",ylim=c(0,35),xlab="Date") #manure avg. obs temperature
lines(result.adjust1.3$Temperature.C,type="l",col="blue") #without shade calibration
lines(result.adjust0.7$Temperature.C,col="black",lwd=2)




#obtain a stat table
source("C:/AAFC/Project 3_Sweden/2. Method/R/stat output.R")

#For manure depth
plot(result.adjust$Depth.cm,type="l",ylim=c(0,350),xaxt='n',col="red",xlab="Date")
lines(result.m$Depth*100,col="black")
legend(0,360,c("Depth-measured","Depth-model"),col=c("black","red")
       ,lty=1,lwd=2,ncol=2,bty="n")
Axis(side=1, at=c(16,130,209,324,381,495,574,689,746,860,939,1054,1111,1225,1304,1419)
     ,labels=rep(c("7/1","10/12","1/1","4/25"),4))


########################################
##A plot to compare Ta, Tm-avg. Tm-0.5m, Tm-1.5, and Tm-2.5
#A Temperature
plot(temp,type="l",col="grey")
lines(result.m$temp.avg,col="red",lwd=2)
lines(result.m$temp0.5,col="orange",lwd=2)
lines(result.m$temp1.5,col="green",lwd=2,lty=2)
lines(result.m$temp2.5,col="blue",lwd=2,lty=2)
legend(0,20,c("Ta","Tm-avg.","Tm-0.5m","Tm-1.5m","Tm-2.5m"),col=c("grey","red","orange","green","blue")
       ,lty=1,lwd=2,ncol=1,bty="n")
#B real depth of the manure thermometer
plot(result.m$Depth,type="l",col="grey",ylim=c(3,0))
lines(result.m$depth0.5,type="l",col="orange",lwd=2)
lines(result.m$depth1.5,type="l",col="green",lwd=2)
lines(result.m$depth2.5,type="l",col="blue",lwd=2)

###########################################################################
#do a color depth plot
result<-result.m[1:365,]
#organize the data to be three columns only, date, depth and temp
DOY1<-rep(result$DOY,each=3)
Temp<-as.vector(t(result[,6:8]))
Temp[is.na(Temp)]<-runif(10,min=0,max=0.005)
Depth<-as.vector(t(result[,10:12]))
Depth[is.na(Depth)]<-runif(1,min=0,max=0.005)
manure<-as.data.frame(cbind(DOY1,Temp,Depth))
#To skip the part with measurments
manure.1<-manure[1:441,]
manure.2<-manure[505:879,]
manure.3<-manure[924:1095,]

#interpolate the temp between depth in a day
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- manure %>% 
    filter(DOY1 == target_date) %>%
    arrange(Depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$Depth, data_for_date$Temp, xout = target_depth)$y
}
# To obtain temp in mutilple days
temp_interp_depth.1<- crossing(
  # the same dates as manure.1
  tibble(DOY1 = unique(manure.1$DOY1)),
  # depths can now be any value
  tibble(manure.depth = seq(0.5, 2.5, length.out = 100))
  ) %>% 
  group_by(DOY1)%>% 
  mutate(temp1 = estimate_temp_by_date(DOY1[1], manure.depth))
  
temp_interp_depth.2<- crossing(
  # the same dates as manure.2
  tibble(DOY1 = unique(manure.2$DOY1)),
  # depths can now be any value
  tibble(manure.depth = seq(0.5, 2.5, length.out = 100))
) %>% 
  group_by(DOY1)%>% 
  mutate(temp1 = estimate_temp_by_date(DOY1[1], manure.depth))

temp_interp_depth.3<- crossing(
  # the same dates as manure.2
  tibble(DOY1 = unique(manure.3$DOY1)),
  # depths can now be any value
  tibble(manure.depth = seq(0.5, 2.5, length.out = 100))
) %>% 
  group_by(DOY1)%>% 
  mutate(temp1 = estimate_temp_by_date(DOY1[1], manure.depth))


temp_interp_depth<-rbind(temp_interp_depth.1,temp_interp_depth.2,temp_interp_depth.3)

#For air temperature
temp<-(Envir.daily$AirTmax1+Envir.daily$AirTmin1)/2 #Air Temp.avg
air_temp<-as.data.frame(cbind(c(1:365),rep(0.4,365),temp[1:365]))
colnames(air_temp)<-c("DOY1","manure.depth","temp1")
aa<-tibble(air_temp)
temp_interp_depth<-rbind(temp_interp_depth,tibble(air_temp))

ggplot(temp_interp_depth, aes(DOY1, manure.depth, fill = temp1)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 5, #  5 is the median temperature 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  ) +
  coord_cartesian(expand = FALSE)

ggplot(result.daily, aes(DOY, manure.depth, fill=manure.temp)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 15, #  5 is the median temperature 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  ) +
  coord_cartesian(expand = FALSE)
