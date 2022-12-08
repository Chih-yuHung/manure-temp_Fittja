library(ggplot2)
library(tidyverse)
library(dplyr)
library(hydroGOF) #NSE 
#Sweden project-Fittja
#To compare my simulation result to the measured data

#output to an excel file
Envir.daily<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/2. Method/input/daily env input_Fittja_May1.csv",header=T)
temp<-((Envir.daily$AirTmax1+Envir.daily$AirTmin1)/2)[731:1095] #Air Temp.avg

#observed data
result<-"C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/"
obs.Fittja<-read.csv(paste(result,"temp.Fittja.daily.csv",sep=""),header=T) 

#simulated data before calibration
sim.Fittja.og<-read.csv(paste(result,"Fittja/original/",Location,Sys.Date(),".csv",sep=""),header=T) 
SR.og<-sim.Fittja.og$total.radiation[1096:1460]/12/277.77778
#simulated data after calibration and modification
sim.Fittja<-read.csv(paste(result,"Fittja/with shade/",Location,Sys.Date(),".csv",sep=""),header=T)
SR<-sim.Fittja$total.radiation[1096:1460]/12/277.77778
#Draw the last year only
sim.Fittja.og<-sim.Fittja.og[c(1096:1460),]
sim.Fittja<-sim.Fittja[c(1096:1460),]

#For measured manure temperature
png(file=paste(result,"Fittja/figures/",Location,Sys.Date(),".png",sep="")
    ,width=800, height =600)
par(mar=c(3,4,2,4))
plot(temp,type="l",xaxt='n',col="green",ylim=c(-15,30)
     ,xlab="Date",ylab="Temperature",las=1) #Air temperature
lines(obs.Fittja$temp.avg,type="l") #manure avg. obs temperature
lines(sim.Fittja.og$Temperature.C,type="l",col="blue",lwd=2) #without shade calibration
lines(sim.Fittja$Temperature.C,col="red",lwd=2)
legend(10,-10,c("Tair","Tm meausrement","Tm model","Tm revised model"
                ,"solar irradiation","solar irradiation revised model")
       ,col=c("green","black","blue","red","lightblue","red")
       ,lty=c(1,1,1,1,2),lwd=2,ncol=2,bty="n")
lines(SR.og,col="lightblue",lty="dashed")
lines(SR,col="red",lty="dashed")
Axis(side=1, at=c(1,93,185,277),las=1
     ,labels=c("May 1, 2020","Aug. 1, 2020","Nov. 1, 2020","Feb. 1, 2021"))
arrows(69,-5,69,0) #removal dates
arrows(155,-5,155,0)
arrows(183,-5,183,0)
arrows(347,-5,347,0)
Axis(side=4, at=c(0,5,10,15,20,25,30)
     ,labels=c("0","5","10","15","20","25","30"))
mtext("Solar irradiation (MJ /m2)",side=4,line = 2.5)
text(20,28,"Örsundsbro (OR) site")
dev.off()

#For manure depth
png(file=paste(result,"Fittja/figures/",Location,Sys.Date(),"_depth.png",sep="")
    ,width=800, height =600)
plot(sim.Fittja$Depth.cm,type="l"
     ,ylim=c(0,350),xaxt='n'
     ,col="black",xlab="Date"
     ,ylab="Depth (cm)")
#retrieved from measurement data
points(c(1,49,116,130,174,301,307),
       c(67,242,310,190,73,280,230))
Axis(side=1, at=c(1,93,185,277)
     ,labels=c("May 1, 2020","Aug. 1, 2020","Nov. 1, 2020","Feb. 1, 2021"))
dev.off()


#obtain a stat table
source("stat output.R")
