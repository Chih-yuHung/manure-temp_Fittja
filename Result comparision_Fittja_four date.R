#This is to know the vertical temperature distribution
#of measured and simulated temperature
#observed data
obs.Fittja<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/temp.Fittja.daily.csv",header=T) 
#simulated data after calibration and modification
sim.Fittja<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/with shade/Fittja_fourdate2022-07-17.csv",header=T)
#simulated data
plot(sim.Fittja[,8],sim.Fittja[,7],xlim=c(-10,30),
     ylim=c(0,3.5),las=1,pch=5,
     xlab="Temperature(°C)",
     ylab="Manure Depth (m)")
for(i in c(0:2)) {
points(sim.Fittja[,2*i+2],sim.Fittja[,2*i+1],pch=i)
}

#Observation data
points(obs.Fittja[1,6:8],obs.Fittja[1,c(10:12)],pch=15)
points(obs.Fittja[93,6:8],obs.Fittja[93,c(10:12)],pch=16)
points(obs.Fittja[185,6:8],obs.Fittja[185,c(10:12)],pch=17)
points(obs.Fittja[277,6:8],obs.Fittja[277,c(10:12)],pch=18)

legend(-10,3.4,c("May 1","Aug 1","Nov 1","Feb 1"),
       pch=c(0,1,2,5),ncol=2,bty="n")
