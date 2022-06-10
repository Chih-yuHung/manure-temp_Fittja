#This part retrieves simulated manure temperature at 0.5m or < 0.5m 
#two part
if (M.depth<=0.5){
  M.temp0.5<-Final.M.Temp[30]   #The simulated M.temp is the temp at bottom
} else {
  t0.5<-seq(0,M.depth,length.out=30)
  M.temp0.5<-Final.M.Temp[which.min(abs(t0.5-0.5))]
}

Output[i,12]<-M.temp0.5-273.15  #Net solar radiation, F106:KG106