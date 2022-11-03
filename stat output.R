#This script calculate a table for measured and simulate data.
#Site: Fittja,i.e. Orsundsbro(OR) site.
#Four stats: RMSE, D, R2 and bias
#five time period: annual, summer, fall, winter, spring. 
#Spring: 122-171 (May 1, 2020-June 19,2020), Summer: 172-265 (June 20,2020,Sept 21)
#Fall: 266-355 (Sept 22, 2020-Dec 20,2020), Winter:356-58 (Dec 21,2020-Feb 27, 2021)


obs.nocover  <-read.csv("temp.no.daily.csv",header=T) #measured manure temperature and soil temperature
obs.cover    <-read.csv("temp.cover.daily.csv",header=T) #measured manure temperature and soil temperature
sim.og       <-read.csv("without cover/before calibration/Arlanda_noshade_before calibration.csv",header=T)[731:1074,] #measured manure temperature and soil temperature
sim.og.c     <-read.csv("with cover/before calibration/Arlanda_noshade_before calibration.csv",header=T)[731:1074,] #simulated manure temperature-with with cover
# sim.nocover  <-read.csv(paste("without cover/Arlanda",Sys.Date(),"-1.csv",sep=""),header=T)[1096:1439,]#simulated manure temperature with cover
# sim.cover    <-read.csv(paste("with cover/Arlanda",Sys.Date(),"-1_with cover.csv",sep=""),header=T)[1096:1439,] #simulated manure temperature-with with cover
sim.nocover  <-read.csv(paste("without cover/Arlanda","2022-01-25-2.csv",sep=""),header=T)[1096:1439,]#simulated manure temperature with cover
sim.cover    <-read.csv(paste("with cover/Arlanda","2022-01-25-2_with cover.csv",sep=""),header=T)[1096:1439,] #simulated manure temperature-with with cover






#organize data for the last year and seasons
#No cover manure temperature
Summer.obs.nocover <-obs.nocover[166<=obs.nocover$DOY&obs.nocover$DOY<=266,]
Fall.obs.nocover   <-obs.nocover[267<=obs.nocover$DOY&obs.nocover$DOY<=356,]
Winter.obs.nocover <-rbind(obs.nocover[357<=obs.nocover$DOY,],obs.nocover[obs.nocover$DOY<=79,])
Spring.obs.nocover <-obs.nocover[80<=obs.nocover$DOY&obs.nocover$DOY<=147,]
obs.nocover.y      <-list(Summer.obs.nocover,Fall.obs.nocover,Winter.obs.nocover,Spring.obs.nocover)

#Covered manure temperature
Summer.obs.cover <-obs.cover[166<=obs.cover$DOY&obs.cover$DOY<=266,]
Fall.obs.cover   <-obs.cover[267<=obs.cover$DOY&obs.cover$DOY<=356,]
Winter.obs.cover <-rbind(obs.cover[357<=obs.cover$DOY,],obs.cover[obs.cover$DOY<=79,])
Spring.obs.cover <-obs.cover[80<=obs.cover$DOY&obs.cover$DOY<=147,]
obs.cover.y      <-list(Summer.obs.cover,Fall.obs.cover,Winter.obs.cover,Spring.obs.cover)

#Obs manure temperature, annual and summer
#Obs manure temperature, annual and summer
mean(obs.nocover$temp.avg) #8.40
mean(obs.cover$temp.avg) #9.53
mean(Summer.obs.nocover$temp.avg) #15.4
mean(Summer.obs.cover$temp.avg) #16.1

#The Tim's original model 
Summer.sim.og    <-sim.og[166<=sim.og$DOY&sim.og$DOY<=265,]
Fall.sim.og      <-sim.og[266<=sim.og$DOY&sim.og$DOY<=355,]
Winter.sim.og    <-rbind(sim.og[356<=sim.og$DOY,],sim.og[sim.og$DOY<=79,])
Spring.sim.og    <-sim.og[80<=sim.og$DOY&sim.og$DOY<=147,]
sim.og.y        <-list(Summer.sim.og,Fall.sim.og,Winter.sim.og,Spring.sim.og)

#The Tim's original model to simulte temp with cover
Summer.sim.og.c    <-sim.og.c[166<=sim.og.c$DOY&sim.og.c$DOY<=265,]
Fall.sim.og.c      <-sim.og.c[266<=sim.og.c$DOY&sim.og.c$DOY<=355,]
Winter.sim.og.c    <-rbind(sim.og.c[356<=sim.og.c$DOY,],sim.og.c[sim.og$DOY<=79,])
Spring.sim.og.c    <-sim.og.c[80<=sim.og.c$DOY&sim.og.c$DOY<=147,]
sim.og.y.c        <-list(Summer.sim.og.c,Fall.sim.og.c,Winter.sim.og.c,Spring.sim.og.c)

#The results with shade and snow albedo
Summer.sim.nocover  <-sim.nocover[166<=sim.nocover$DOY&sim.nocover$DOY<=265,]
Fall.sim.nocover    <-sim.nocover[266<=sim.nocover$DOY&sim.nocover$DOY<=355,]
Winter.sim.nocover  <-rbind(sim.nocover[356<=sim.nocover$DOY,],sim.nocover[sim.nocover$DOY<=79,])
Spring.sim.nocover  <-sim.nocover[80<=sim.nocover$DOY&sim.nocover$DOY<=147,]
sim.nocover.y       <-list(Summer.sim.nocover,Fall.sim.nocover,Winter.sim.nocover,Spring.sim.nocover)


#The results with shade and snow albedo, net cover
Summer.sim.cover   <-sim.cover[166<=sim.cover$DOY&sim.cover$DOY<=265,]
Fall.sim.cover     <-sim.cover[266<=sim.cover$DOY&sim.cover$DOY<=355,]
Winter.sim.cover   <-rbind(sim.cover[356<=sim.cover$DOY,],sim.cover[sim.cover$DOY<=79,])
Spring.sim.cover   <-sim.cover[80<=sim.cover$DOY&sim.cover$DOY<=147,]
sim.cover.y        <-list(Summer.sim.cover,Fall.sim.cover,Winter.sim.cover,Spring.sim.cover)

#Simulated manure temperauter, annual and summer
mean(sim.nocover$Temperature.C) #7.9
mean(sim.cover$Temperature.C) #9.4
mean(Summer.sim.nocover$Temperature.C)#16.4
mean(Summer.sim.cover$Temperature.C)#16.2
#A table for RMSE, d, R2 
stat.RMSE<-data.frame(RMSE=numeric()
                 ,RMSE.summer=numeric(),RMSE.fall=numeric(),RMSE.winter=numeric(),RMSE.spring=numeric()
                 ,stringsAsFactors=FALSE)
stat.D<-data.frame(D=numeric()
                 ,D.summer=numeric(),D.fall=numeric(),D.witner=numeric(),D.spring=numeric()
                 ,stringsAsFactors=FALSE)
stat.R2<-data.frame(R2=numeric() 
                 ,R2.summer=numeric(),R2.fall=numeric(),R2.winter=numeric(),R2.spring=numeric()
                 ,stringsAsFactors=FALSE) 
stat.bias<-data.frame(bias=numeric() 
                      ,bias.summer=numeric(),bias.fall=numeric(),bias.winter=numeric(),bias.spring=numeric()
                      ,stringsAsFactors=FALSE) 
#RMSE caculation only for the last year
RMSE<-function(x,y){
  sqrt(sum((x-y)^2,na.rm=TRUE)/length(x))
}

#Annual RMSE
stat.RMSE[1,1]<-RMSE(sim.og$Temperature.C , obs.nocover$temp.avg)
stat.RMSE[2,1]<-RMSE(sim.nocover$Temperature.C , obs.nocover$temp.avg)
stat.RMSE[3,1]<-RMSE(sim.og.c$Temperature.C , obs.cover$temp.avg)
stat.RMSE[4,1]<-RMSE(sim.cover$Temperature.C , obs.cover$temp.avg)


#RMSE by seasons for the model without revision, 
#without revision
for (i in 1:4) {
  stat.RMSE[1,i+1]<-RMSE(sim.og.y[[i]]$Temperature.C,obs.nocover.y[[i]]$temp.avg)
}
#with shade and snow revision/ without cover
for (i in 1:4) {
  stat.RMSE[2,i+1]<-RMSE(sim.nocover.y[[i]]$Temperature.C,obs.nocover.y[[i]]$temp.avg)
}

#without revision with cover
for (i in 1:4) {
  stat.RMSE[3,i+1]<-RMSE(sim.og.y.c[[i]]$Temperature.C,obs.cover.y[[i]]$temp.avg)
}
#with shade, snow, and net cover revision
for (i in 1:4) {
  stat.RMSE[4,i+1]<-RMSE(sim.cover.y[[i]]$Temperature.C,obs.cover.y[[i]]$temp.avg)
}

#D function
D<-function(x,y){
  ybar<-mean(y)
  1-(sum((x-y)^2)/sum((abs(x-ybar)+abs(y-ybar))^2))
}
#Annual D, index of agreement
stat.D[1,1]<-D(sim.og$Temperature.C , obs.nocover$temp.avg)
stat.D[2,1]<-D(sim.nocover$Temperature.C , obs.nocover$temp.avg)
stat.D[3,1]<-D(sim.og.c$Temperature.C , obs.cover$temp.avg)
stat.D[4,1]<-D(sim.cover$Temperature.C , obs.cover$temp.avg)


#D by seasons for the model without revision, 
#without revision
for (i in 1:4) {
  stat.D[1,i+1]<-D(sim.og.y[[i]]$Temperature.C,obs.nocover.y[[i]]$temp.avg)
}

#with shade and snow revision/ without cover
for (i in 1:4) {
  stat.D[2,i+1]<-D(sim.nocover.y[[i]]$Temperature.C,obs.nocover.y[[i]]$temp.avg)
}

#without revision with cover
for (i in 1:4) {
  stat.D[3,i+1]<-D(sim.og.y.c[[i]]$Temperature.C,obs.cover.y[[i]]$temp.avg)
}

#with shade, snow, and net cover revision
for (i in 1:4) {
  stat.D[4,i+1]<-D(sim.cover.y[[i]]$Temperature.C,obs.cover.y[[i]]$temp.avg)
}

#R2 valeus
rsq <- function(x, y) {cor(x, y) ^ 2}
#Annual R2
stat.R2[1,1]<-rsq(sim.og$Temperature.C , obs.nocover$temp.avg)
stat.R2[2,1]<-rsq(sim.nocover$Temperature.C , obs.nocover$temp.avg)
stat.R2[3,1]<-rsq(sim.og.c$Temperature.C , obs.cover$temp.avg)
stat.R2[4,1]<-rsq(sim.cover$Temperature.C , obs.cover$temp.avg)

#R2 by seasons for the model without revision, 
#without revision
for (i in 1:4) {
  stat.R2[1,i+1]<-rsq(sim.og.y[[i]]$Temperature.C,obs.nocover.y[[i]]$temp.avg)
}
#with shade and snow revision/ without cover
for (i in 1:4) {
  stat.R2[2,i+1]<-rsq(sim.nocover.y[[i]]$Temperature.C,obs.nocover.y[[i]]$temp.avg)
}
#without revision
for (i in 1:4) {
  stat.R2[3,i+1]<-rsq(sim.og.y.c[[i]]$Temperature.C,obs.cover.y[[i]]$temp.avg)
}
#with shade, snow, and net cover revision
for (i in 1:4) {
  stat.R2[4,i+1]<-rsq(sim.cover.y[[i]]$Temperature.C,obs.cover.y[[i]]$temp.avg)
}

#average bias valeus
bias <- function(x, y) {sum(x-y)/length(x)}
#Annual bias
stat.bias[1,1]<-bias(sim.og$Temperature.C , obs.nocover$temp.avg)
stat.bias[2,1]<-bias(sim.nocover$Temperature.C , obs.nocover$temp.avg)
stat.bias[3,1]<-bias(sim.og.c$Temperature.C , obs.cover$temp.avg)
stat.bias[4,1]<-bias(sim.cover$Temperature.C , obs.cover$temp.avg)

#bias by seasons for the model without revision, 
#without revision
for (i in 1:4) {
  stat.bias[1,i+1]<-bias(sim.og.y[[i]]$Temperature.C,obs.nocover.y[[i]]$temp.avg)
}
#with shade and snow revision/ without cover
for (i in 1:4) {
  stat.bias[2,i+1]<-bias(sim.nocover.y[[i]]$Temperature.C,obs.nocover.y[[i]]$temp.avg)
}
#without revision with cover
for (i in 1:4) {
  stat.bias[3,i+1]<-bias(sim.og.y.c[[i]]$Temperature.C,obs.cover.y[[i]]$temp.avg)
}
#with shade, snow, and net cover revision
for (i in 1:4) {
  stat.bias[4,i+1]<-bias(sim.cover.y[[i]]$Temperature.C,obs.cover.y[[i]]$temp.avg)
}


#Rename the rownames 
row.names(stat.RMSE)<-c("original","without cover","original with cover","with cover")
row.names(stat.D)<-c("original","without cover","original with cover","with cover")
row.names(stat.R2)<-c("original","without cover","original with cover","with cover")
row.names(stat.bias)<-c("original","without cover","original with cover","with cover")

#Put the four dataframe to a file
stat<-data.frame(row.names = rep(c("original","without cover"),4),
                 col.names = c("Annual","Summer","Fall","Winter","Spring"))              
#write the results out
library(xlsx)
write.xlsx(round(stat.RMSE,2), file=paste("stat output/",Sys.Date(),".xlsx",sep=""), sheetName="RMSE", row.names=T)
write.xlsx(round(stat.D,2), file=paste("stat output/",Sys.Date(),".xlsx",sep=""), sheetName="D", row.names=T,append =T)
write.xlsx(round(stat.R2,2), file=paste("stat output/",Sys.Date(),".xlsx",sep=""), sheetName="R2", row.names=T,append =T)
write.xlsx(round(stat.bias,2), file=paste("stat output/",Sys.Date(),".xlsx",sep=""), sheetName="bias", row.names=T,append =T)
