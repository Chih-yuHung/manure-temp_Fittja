library(imputeTS) # for NA interpolation
library(REdaS)

#To organize weather input for Sweden data, it's from Ultuna, SLU
temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/hourly data for Fittja.csv",header=T)
temp[temp=="."]=NA
temp$Temp<-as.numeric(temp$Temp)
#interpolate NA by linear
weather<-temp %>% na_interpolation(option="linear")#Start with June 15.
#So the max and min is obtained from max and min per hour not from average of hour
#Note that tapply sort my data again by following the original order in my data, i.e start from June 15.
AirTmax1<-rep(tapply(weather$Temp,weather$Date,max),3)
AirTmin1<-rep(tapply(weather$Temp,weather$Date,min),3)
AirTmin2<-c(AirTmin1[2:1095],AirTmin1[1])
AirTmax0<-c(AirTmax1[1095],AirTmax1[1:1094])

#RH
RH.6<-rep(weather$RH[weather$Hour=="600"],3)
RH.15<-rep(weather$RH[weather$Hour=="1500"],3)

#Radiation data from 2020/6/15-2021/6/14 from 
rad<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/Radiation Daily data 1992-2021 for Fittja.csv",header=T)
rad.2020<-rad[10242:10606,c(3,19)] #to obtain radiation from 2020/6/15-2021/6/14 our experiment period 
SR<-rad.2020$SOLIN
rad<-rad[16:10606,c(3,19)]#Historical radiation 1992/6/15-2021/6/14
rad$year<-substring(rad[,1],first=1,last=4)
rad$month<-substring(rad[,1],first=5,last=6)
rad$day<-substring(rad[,1],first=7,last=8)
rad$monthday<-substring(rad[,1],first=5,last=8)
rad<-rad[complete.cases(rad),]
radiation<-tapply(rad$SOLIN,rad$monthday,mean)[c(1:59,61:366)] #exclude leap day
radiation.max<-tapply(rad$SOLIN,rad$monthday,max)[c(1:59,61:366)] #exclude leap day for max potential radiation
radiation.max1<-radiation.max[c(166:365,1:165)] #to start from June 15.
# it's not linear regression between SR and SRmax
cloud<-pmin(ifelse(SR<radiation.max1,((1-SR/radiation.max1)/0.72)^(1/3.2),0),1)
#Srmax is a useless value in simulation and I made a vector for it here
Srmax<-radiation.max1


# wind data is from daily data in Ultuna
temp1<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/Radiation Daily data 1992-2021 for Fittja.csv",header=T)
wind<-rep(temp1[10242:10606,17],3) #to obtain wind speed from 2020/6/15-2021/6/14 our experiment period 
# unit, m/s at 2m so I don't need to convert to 2m later

# precipitation data is from daily in Ultuna, unit:mm
temp1<-cbind(temp1[,3],as.numeric(temp1[,19]))
precip<-rep(temp1$NED[10242:10606],3)
precip<-ifelse(precip<0,0,precip) #there are some negative precip


#combine the data together, it's daily value
aa<-as.data.frame(cbind(AirTmax1,AirTmin1,AirTmin2,AirTmax0,SR,Srmax,precip,RH.6,RH.15,wind,cloud))

#to obtain year, month, date.
DAY<-as.character(as.Date(0:1094,origin = "2020-06-15"))
DAY<-as.data.frame(matrix(unlist(strsplit(DAY,split="-")),ncol=3,byrow=TRUE))
colnames(DAY)<-c("Year","Month","Day")
DAY$DOY<-rep(c(166:365,1:165),3)

#combind together
env.input<-cbind(DAY,aa)

#Export for input
write.csv(env.input,"C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/2. Method/input/daily env input_Fittja_June15.csv",row.names = FALSE)


#Obtain yearly amplitude and daily amplitude, wind speed
yearly.amp<-(max(env.input$AirTmax1)-min(env.input$AirTmin1))/2 #24.95
#daily amplitude. It's an average of max-min /2
daily.amp<-mean((env.input$AirTmax1-env.input$AirTmin1)/2) #3.824
windspeed<-mean(env.input$wind)/2 #it assumed 50% because of obstruction of tank and buildings nearby, 1.38 
