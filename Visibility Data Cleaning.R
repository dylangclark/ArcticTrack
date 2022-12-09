####Visibility Analysis and Timeseries


library(weathercan)
library(lubridate)
library(dplyr)
library(zoo)
library(plyr)


##Steps
#Develop list of communities including all available dates
#Split up communities into regional groups
#Determine what communities to use visibility data from
#Pull visibility date for selected communities
#Calculate number of days with visibility under 3km for each year and community
#Select 5th and 95th percentile of days with visibility under 3km for each region.

########Step 1: Develop list of communities including all available dates

InuitNunangatStations<-read.csv("D:/R/LandUse/ClippedStations.csv", header=TRUE)
 
INstations <- stations %>%
  filter(interval == "hour") %>%
  filter(lat > 55) %>%
  filter(start <= 1980) %>%
  filter(end >= 2010)
INstations


SID1<-INstations$station_id
SID2<-InuitNunangatStations$Station_ID
SID<-unique(c(SID1,SID2))

###NEW CODE AFTER SELECTING STATIONS
InuitNunangatStations<-read.csv("D:/R/LandUse/ClippedStations.csv", header=TRUE)
SID1<-INstations %>% filter(station_name %in% AllStations)
SID2<-InuitNunangatStations %>% filter(Name %in% AllStations)
SID<-unique(c(SID1$station_id,SID2$Station_ID))

####


for (i in SID){
  sid<-i
  print(paste0("looking for station ", sid))
  wH<-weather_dl(station_ids=sid,start = "1980-01-01",end = "2020-12-31",interval="hour",format=TRUE, quiet=TRUE)
  if(10<length(colnames(wH))){
        wH$Date<-strptime(wH$time,format="%Y-%m-%d %H:%M:%S")
        
        wH$Vis<-as.integer(as.character(wH$visib))
        
        if(10>(sum(wH[,36], na.rm=TRUE))){print("No Vis Data")
        
          }else{
        print("Aggregating data")
        wD<-ddply(wH,.(date),
                  plyr::summarize,
                    aveT=mean(temp, na.rm=TRUE),
                    maxT=max(temp, na.rm=TRUE),
                    minT=min(temp, na.rm=TRUE),
                    aveVis=mean(Vis, na.rm=TRUE),
                    minVis=min(Vis, na.rm=TRUE),
                    meanWndDir=mean(wind_dir, na.rm=TRUE, control.circular=list),
                    meanWndSpd=mean(wind_spd, na.rm=TRUE)
                    
        )
        wD[,9]<-wH[1,1]
        wD[,10]<-wH[1,2]
        wD[,11]<-wH[1,4]
        wD[,12]<-wH[1,5]
        wD[,13]<-wH[1,6]
      
      
        if(exists("Out")){Out<-rbind(Out, wD)}else{(Out<-wD)}
        print(paste0("writing data for ", sid))
        
          
          }
  }else{}
}
  
write.csv(Out,"D:/R/LandUse/VisDailyRaw2.csv")
  
  
################## Read Daily Vis Raw file
##Vis Station Table

Vis<-read.csv("D:/R/LandUse/VisDailyRaw2.csv", header=TRUE)

Stations<-(levels(as.factor(Vis$station_name)))
Years<-(2000:2020)

remove(RN,Out)
for(i in Stations){
      station<-i
      for(i in Years){
      x<-i
      print(paste0("Working on station ", station, " for year ", x))
      
    N<-Vis[which(Vis$station_name==station
                          & Vis$year == x), ]
    
    
    w<-as.numeric(colSums(is.na(N))[9])
    
    if(exists("RN")){RN<-cbind(RN, w)}else{(RN<-w)}
  }

RN<-cbind(station, RN)
if(exists("Out")){Out<-rbind(Out, RN)}else{(Out<-RN)}
remove(RN)

}

write.csv(Out, "D:/R/LandUse/VisSummaryMissing2.csv")


############### Find the high, low, and mid visibility poor years for each selected stations
### List of communities for inclusion
### Group communities into regions
### For each community within a region pull the timeseries of each month
### Create a monthly timeseries of visibility for each region and month
### Sample each timeseries.



Regions<-c("ISR","Kitikmeot","Elsmere","Kivaliq","Baffin","Nunavik","Nunatsiavut")

ISR<-c("TUKTOYAKTUK A","PAULATUK A")
Kitikmeot<-c("TALOYOAK A","GJOA HAVEN AWOS A","KUGLUKTUK A","LUPIN A")
Elsemere<-c("RESOLUTE BAY A","GRISE FIORD A")
Kivaliq<-c("ARVIAT AWOS","BAKER LAKE A","CORAL HARBOUR A","RANKIN INLET A")
Baffin<-c("ARCTIC BAY","CAPE DORSET AWOS","CLYDE AWOS","HALL BEACH AWOS","IQALUIT A","POND INLET AWOS","QIKIQTARJUAQ AWOS")
Nunavik<-c("KUUJJUAQ A","KUUJJUARAPIK A","PUVIRNITUAQ A")
Nunatsiavut<-c("MAKKOVIK A")



VisData<-read.csv("D:/R/LandUse/VisDailyRaw3.csv")
years<-unique(VisData$year)
months<-unique(VisData$month)
stations<-unique(VisData$station_name)
regions<-unique(VisData$region)


for(i in stations){
    Vdf<-VisData %>%
      filter(station_name == i)
  
  for(m in months){
    VdfM<-Vdf %>%
      filter(month == m)
    
    years<-unique(VdfM$year)

    for (y in years) {
      VdfMY<-VdfM %>%
        filter(year == y)
      
      naCount<-sum(is.na(VdfMY$aveVis))
      if(naCount > 4){
        print("Too many missing days")
      }else{
        VdfMY$aveVis<-replace(na.spline(VdfMY$aveVis), is.na(na.approx(VdfMY$aveVis, na.rm = FALSE)),NA)
        
        region<-VdfMY[1,12]
        fileN<-paste0("D:/R/LandUse/VisSeries/",region,"/",m,"/",y,"_",i,".csv")
        write.csv(VdfMY,fileN)
        print("wrote csv")
      }
      
    }
    
  }

}




