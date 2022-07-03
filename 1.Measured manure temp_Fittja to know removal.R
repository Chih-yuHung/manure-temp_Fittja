#This file is to find out possile manur input and manure removal time
#The concept is to find arupt temperature chamge at 1.5 and 2.5 m
#Deal with the manure temperature
temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/manure temp Fittja.csv",header=T)
#Use the data until Feb 27, 2021 (2428)
temp<-temp[1:2428,]

for (i in 1:3) {
temp[,i+5]<-c(0,ifelse(abs(diff(temp[,i+1]))>2,1,0))
}

subset(temp, V6 == 1, select = Time)
subset(temp, V7 == 1, select = Time)
subset(temp, V8 == 1, select = Time)

#It looks like there is a removal event in August
#about August 13