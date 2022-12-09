### Pull it all together Landuse modelling

##October 2021

###Steps
# 1) data links
# 2) Visibility look up
# 3) list all communities
# 4) list and group models and sres
# 5) function of if then statements



####  Load data

library(RNetCDF)
library(raster)
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(rgdal)
library(dplyr)
library(rgeos)
library(purrr)
library(ggplot2)
library(sf)
library(stringr)
library(readr)
library(RStoolbox)
library(PCICt)
library(ncdf4.helpers)
library(exactextractr)
library(gdata)
library(epwshiftr)
library(reshape2)
library(svMisc)
library(hdf5r)




########################### Download CMIP6 GCM data for each buffer zone
######################################################

##LoadInuit Nunanvat community data
Buffers<-readOGR(dsn="D:/GIS/Arctic/Community Buffer100km.shp")
Communities<-read.csv("D:/R/LandUse/CMIP5Data/communities.csv")
Regions<-readOGR(dsn="D:/GIS/Arctic/RegionBuffer200km.shp")


##Temp data

GCMDestT<-init_cmip6_index(
            variable="tas",
            frequency="day",
            experiment=c("ssp245","ssp585"),
            variant="r1i1p1f1",
            source=c("NorESM2-MM","CMCC-ESM2","CMCC-CM2-SR5","CESM2-WACCM","MRI-ESM2-0","BCC-CSM2-MR"),
            resolution="100 km",
            latest=F,
            activity="ScenarioMIP")


##Prcp data

GCMDestP<-init_cmip6_index(
  variable="pr",
  frequency="day",
  experiment=c("ssp245","ssp585"),
  variant="r1i1p1f1",
  source=c("NorESM2-MM","CMCC-ESM2","CMCC-CM2-SR5","CESM2-WACCM","MRI-ESM2-0","BCC-CSM2-MR"),
  resolution="100 km",
  latest=F,
  activity="ScenarioMIP")

##Wind data

GCMDestW<-init_cmip6_index(
  variable="sfcWind",
  frequency="day",
  experiment=c("ssp245","ssp585"),
  variant="r1i1p1f1",
  source=c("NorESM2-MM","CMCC-ESM2","CMCC-CM2-SR5","CESM2-WACCM","MRI-ESM2-0","BCC-CSM2-MR"),
  resolution="100 km",
  latest=F,
  activity="ScenarioMIP")

##Ice data
GCMDestI1<-init_cmip6_index(
  variable="sithick",
  frequency="day",
  experiment=c("ssp245","ssp585"),
  variant="r1i1p1f1",
  source=c("NorESM2-MM","CMCC-ESM2","CMCC-CM2-SR5","CESM2-WACCM","MRI-ESM2-0","BCC-CSM2-MR"),
  resolution="100 km",
  latest=F,
  activity="ScenarioMIP")


GCMDestI2<-init_cmip6_index(
  variable="siconc",
  frequency="day",
  experiment=c("ssp245","ssp585"),
  variant="r1i1p1f1",
  source=c("NorESM2-MM","CMCC-ESM2","CMCC-CM2-SR5","CESM2-WACCM","MRI-ESM2-0","BCC-CSM2-MR"),
  resolution="100 km",
  latest=F,
  activity="ScenarioMIP")


##################
# GCM cleaning

GCMDest<-rbind(GCMDestI1,GCMDestI2,GCMDestW,GCMDestP,GCMDestT)

write.csv(GCMDest,"D:/GIS/Climate Models/CMIP6/directory.csv")

GCMDest<-read.csv("D:/GIS/Climate Models/CMIP6/directory2.csv")

for (url in GCMDest$file_url) {
  tryCatch(download.file(url, destfile = basename(url),timeout=30,mode="wb"),
           error=function(e)print("did not work"))
}




###############################################################################
##########################################################################################



dPath<-"D:/GIS/Climate Models/CMIP6/sithick/"
setwd("D:/GIS/Climate Models/CMIP6/")
cutShpL<-readOGR(dsn="D:/GIS/Arctic/RegionBuffer200km.shp")
cutShpL<-st_as_sf(spTransform(cutShpL,CRS("+init=EPSG:4326")))
cutShpI<-readOGR(dsn="D:/GIS/Arctic/RegionBuffer300kmCut.shp")
cutShpI<-st_as_sf(spTransform(cutShpI,CRS("+init=EPSG:4326")))

GCMExtract2(dirPath = dPath,cutShpLand = cutShpL,cutShpIce = cutShpI,variName = "sithick",saveDir = "D:/GIS/Climate Models/CMIP6/")

#sfcWind

#use GCMExtract 1 for all variables except SI data. Use GCMExtract 2 for siconc and sithick

GCMExtract1<-function(dirPath,cutShpLand,cutShpIce,saveDir,variName){
  cntr<-1
  
  if(variName == "siconc" || variName=="sithick"){cutShp<-cutShpIce}else{cutShp<-cutShpLand}
  
  #list all netcdfs in folder
  files<-list.files(path=dirPath,full.names = T,include.dirs = F, pattern=".nc",recursive = T)
  oldST<-Sys.time()
  
  #get TS from netCDF

  for(i in files){

    ## pull data from i file
    GCM<-as.character(str_match(i,pattern="(?<=day_)(.*?)(?=_ssp)")[,1])
    RCP<-as.character(str_match(i,pattern="(?<=_ssp)(.*?)(?=_r1i1p1f1)")[,1])
    
    ## conditions set up to determine if next is new GCM
    cntr<-cntr+1
    nfiles<-as.numeric(length(files))
      if(cntr<nfiles){nextGCM<-as.character(str_match(files[cntr],pattern="(?<=day_)(.*?)(?=_ssp)")[,1])}else{nextGCM<-GCM}
    
    tryCatch({
            baseNc<-nc_open(as.character(i),verbose=F,write=F)
            cal <- as.character(baseNc$dim$time$calendar)
            orig<-as.character(baseNc$dim$time$units)
            tas_time <- nc.get.time.series(baseNc, v="dim",time.dim.name="time")
            tas_time <- as.PCICt(x=tas_time, origin=orig,cal=cal)
              
            lon<-ncvar_get(baseNc,"lon")
            lat<-ncvar_get(baseNc,"lat")
            lon180<-(lon + 180) %% 360 - 180
            z<-ncvar_get(baseNc,variName)
            layers<-dim(z)[3]
            
              ### pull data from layers in i file
            
                for (y in 1:layers){
                  
                  print(paste0("working on new layer of ",GCM," - time elapsed ",Sys.time()-oldST))

                        z1<-z[,,y]
                        colnames(z1)<-lat
                        rownames(z1)<-lon180
                        z2<-melt(z1)
                        colnames(z2)[3]<-"Value_mean"
                        zDate<-as.character(tas_time[y],format="%Y-%m-%d")
                        
                        ## cut to regions
                        newshp<-st_as_sf(z2,coords=c("Var1","Var2"),crs=CRS("+init=EPSG:4326"))
                        z2Cut<-as.data.frame(st_join(newshp,cutShp,left=F))
                  
                        z3<-z2Cut %>%
                            dplyr::group_by(Region) %>%
                            summarise(across(where(is.numeric), ~mean(.x,na.rm=T)))
                        
                        z3<-cbind(GCM,RCP,zDate,variName,z3)
                        
                        if(exists("z3bind")){z3bind<-rbind(z3bind,z3)}else{z3bind<-z3}
                        
                        oldST<-Sys.time()
                        remove(z3)
                     
                }
            
            
            if(exists("zbefore")){zbefore<-rbind(zbefore,z3bind)}else{zbefore<-z3bind}
              saveName<-paste0(saveDir,GCM,"-",variName,".csv")
                write.csv(zbefore,saveName)
            
            remove(z1,z2,zDate,z,baseNc,z3bind,z3,layers)

        
    })
    
    if(GCM==nextGCM){print("Next file")}else if (exists("zbefore")&(GCM != nextGCM)) {remove(zbefore)}
  }

  gc()
  return(z3bind)
}


GCMExtract2<-function(dirPath,cutShpLand,cutShpIce,saveDir,variName){
  cntr<-1
  
  if(variName == "siconc" || variName=="sithick"){cutShp<-cutShpIce}else{cutShp<-cutShpLand}
  
  #list all netcdfs in folder
  files<-list.files(path=dirPath,full.names = T,include.dirs = F,recursive = T)
  oldST<-Sys.time()
  
  #get TS from netCDF
  
  for(i in files){
    
    ## pull data from i file
    GCM<-as.character(str_match(i,pattern="(?<=day_)(.*?)(?=_ssp)")[,1])
    RCP<-as.character(str_match(i,pattern="(?<=_ssp)(.*?)(?=_r1i1p1f1)")[,1])
    
    ## conditions set up to determine if next is new GCM
    cntr<-cntr+1
    nfiles<-as.numeric(length(files))
    if(cntr<nfiles){nextGCM<-as.character(str_match(files[cntr],pattern="(?<=day_)(.*?)(?=_ssp)")[,1])}else{nextGCM<-GCM}
    
    tryCatch({
      baseNc<-nc_open(as.character(i),verbose=F,write=F)
      cal <- as.character(baseNc$dim$time$calendar)
      orig<-as.character(baseNc$dim$time$units)
      tas_time <- nc.get.time.series(baseNc, v="dim",time.dim.name="time")
      
      lon<-ncvar_get(baseNc,"longitude")
      lat<-ncvar_get(baseNc,"latitude")
      lon180<-(lon180<-(lon + 180) %% 360 - 180)
      
      coord <- data.frame(lon=as.vector(lon180), lat=as.vector(lat))
      
      layers<-baseNc$dim$time$len
      
      ### pull data from layers in i file
      for (y in 1:layers){
        print(paste0("working on new layer of ",GCM," - time elapsed ",Sys.time()-oldST))
        
        z1<-ncvar_get(baseNc,variName,raw_datavals=T, start=c(1,1,y),count=c(-1,-1,1))
        
        if(variName=="siconc"){
        z1[z1>100]<-NA}else if (variName=="sithick"){z1[z1>100000]<-NA}

        zDate<-as.character(tas_time[y],format="%Y-%m-%d")
        z2<-cbind(coord,as.vector(z1))
        colnames(z2)<-c("Var1","Var2","Value_mean")
        
        ## cut to regions
        
        newshp<-st_as_sf(z2,coords=c("Var1","Var2"),crs=CRS("+init=EPSG:4326"))
        z2Cut<-as.data.frame(st_join(newshp,cutShp,left=F))
        
        z3<-z2Cut %>%
          dplyr::group_by(Region) %>%
          summarise(across(where(is.numeric), ~mean(.x,na.rm=T)))
        
        z3<-cbind(GCM,RCP,zDate,variName,z3)
        
        if(exists("z3bind")){z3bind<-rbind(z3bind,z3)}else{z3bind<-z3}
        
        oldST<-Sys.time()
        remove(z3)

        
      }
      
      
      if(exists("zbefore")){zbefore<-rbind(zbefore,z3bind)}else{zbefore<-z3bind}
      saveName<-paste0(saveDir,GCM,"-",variName,".csv")
      write.csv(zbefore,saveName)
      
      remove(z1,z2,zDate,z,baseNc,z3bind,z3,layers)
      
      
    })
    
    if(GCM==nextGCM){print("Next file")}else if (exists("zbefore")&(GCM != nextGCM)) {remove(zbefore)}
  }
  
  gc()
  return(z3bind)
}


GCMExtract3<-function(dirPath,cutShpLand,cutShpIce,saveDir,variName){
  cntr<-1
  
  if(variName == "siconc" || variName=="sithick"){cutShp<-cutShpIce}else{cutShp<-cutShpLand}
  
  #list all netcdfs in folder
  files<-list.files(path=dirPath,full.names = T,include.dirs = F, pattern=".nc",recursive = T)
  oldST<-Sys.time()
  
  #get TS from netCDF
  
  for(i in files){
    
    ## pull data from i file
    GCM<-as.character(str_match(i,pattern="(?<=day_)(.*?)(?=_ssp)")[,1])
    RCP<-as.character(str_match(i,pattern="(?<=_ssp)(.*?)(?=_r1i1p1f1)")[,1])
    
    ## conditions set up to determine if next is new GCM
    cntr<-cntr+1
    nfiles<-as.numeric(length(files))
    if(cntr<nfiles){nextGCM<-as.character(str_match(files[cntr],pattern="(?<=day_)(.*?)(?=_ssp)")[,1])}else{nextGCM<-GCM}
    
      baseNc<-nc_open(as.character(i))
      cal <- as.character(baseNc$dim$time$calendar)
      orig<-as.character(baseNc$dim$time$units)
      tas_time <- nc.get.time.series(baseNc, v="dim",time.dim.name="time")
      
      
      
      pre1.brick<-brick(as.character(i),varname=variName)
      cutShp = st_transform(cutShp, crs(pre1.brick[[1]]))
      layers<-nlayers(pre1.brick)
      
      pre1.brick<-crop(pre1.brick,extent(cutShp))
      
    
      ### pull data from layers in i file
      
      for (y in 1:layers){
        
        print(paste0("working on new layer of ",GCM," - time elapsed ",Sys.time()-oldST))
        
        
        pre2.brick<-pre1.brick[[y]]
        clip3 <- exact_extract(pre2.brick, cutShp,'mean',progress=F,append_cols=T)
        Date<-as.character(tas_time[y])
        
        val<-as.numeric(clip3[,7])
        region<-as.character(clip3[,1])
        Observe<-cbind(region,val)
        z3<-cbind(GCM,RCP,Date,variName,Observe)
        
        if(exists("z3bind")){z3bind<-rbind(z3bind,z3)}else{z3bind<-z3}
        
        oldST<-Sys.time()
        
      }
      
      
      if(exists("zbefore")){zbefore<-rbind(zbefore,z3bind)}else{zbefore<-z3bind}
      saveName<-paste0(saveDir,GCM,"-",variName,".csv")
      write.csv(zbefore,saveName)
      
      remove(z1,z2,zDate,z,baseNc,z3bind,z3,layers)
      
    
    if(GCM==nextGCM){print("Next file")}else if (exists("zbefore")&(GCM != nextGCM)) {remove(zbefore)}
  }
  
  gc()
  return(z3bind)
}



##############################################      Visibility data
######## 1) function to assess distribution that works for monte carlo
######## 2) iterate the function for each month and region and then save file in the region folder
######## 3) sample raw data to develop timeseries for each region


VisData<-read.csv("D:/R/LandUse/ArcticTracks/VisSeries/VisDailyAllRaw.csv")

VisTS<-read.csv("D:/R/LandUse/CMIP6/Visibility TS.csv")

VisibilityTS<-VisSim(visdata=VisData,VisTS=VisTS)

VisSim<-function(visdata,VisTS){
  regions<-sort(unique(visdata$region))
  months<-sort(unique(visdata$month))
  
  visdata<-visdata[,c(3,8,13)]
  
  for(r in regions){
    regdata<-visdata %>%
      dplyr::filter(region==r)
    
    for(m in 1:12){
      dataSample<-regdata %>%
        dplyr::filter(month==m)
      
      dataSample<-na.omit(dataSample)
      
      v_ts<-VisTS %>%
        dplyr::filter(month==m & Region==r)
      
      sims<-nrow(v_ts)

      s<-sample(dataSample$adjustedVis,sims,replace=T)
      
      vis<-cbind(v_ts,s)
      
      if(exists("outVis")){outVis<-rbind(outVis,vis)}else{outVis<-vis}
    }
    
  }
  
  return(outVis)
}

write.csv(VisibilityTS,"D:/R/LandUse/CMIP6/VisibilitySimulations.csv")




##########################################  Good bad day historic analysis

DailyTrailTest<-function(Thresholds,WxDataAll){
  
  cn<-c("Date","Month","Year","Day","MonthYear","Hamlet","SN","Temp","MinT","Prcp","MeanWnd","MaxWnd","Mean.WindShift","Wnd","MeanVis","MinVis","Vis.Capped.14",
        "Vis.Diff","VisShift","Vis","Con1","Thick1","Con2","Thick2","Con3","Thick3","Land1Tmp","Land1Prcp","Land1Vis","Land1Wnd","Land1Total","Land2Total","Land3Total","Ice1Tmp","Ice1Prcp",
        "Ice1Vis","Ice1Wnd","Ice1Conc","Ice1Thick","Ice1Total","Ice2Total","Ice3Total","Water1Tmp","Water1Prcp","Water1Vis","Water1Wnd",
        "Water1Conc","Water1Total","Water2Total","Water3Total")
  
  for(i in (1:nrow(WxDataAll))){
    
    #Land model variables
    
    WxData<-WxDataAll[i,]
    n<-as.numeric(rowSums(is.na(WxData)))
    
    if(n>6){
      o<-as.list(rep("NA",24))
      Out<-cbind(WxData,o)
      colnames(Out)<-cn
      
    }else{
      
      Land1Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp<Thresholds[1,4] & WxData$Temp>Thresholds[1,5]){1}else{0})}
      Land2Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp<Thresholds[2,4] & WxData$Temp>Thresholds[2,5]){1}else{0})}
      Land3Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp<Thresholds[3,4] & WxData$Temp>Thresholds[3,5]){1}else{0})}
      
      Land1Prcp<-if(is.na(WxData$Prcp)|(is.na(WxData$Temp))){"NA"}else{(0+if(WxData$Prcp>Thresholds[4,5] & WxData$Temp>0){1}else{0}+if(WxData$Prcp>Thresholds[7,5] & WxData$Temp<0){1}else{0})}
      Land2Prcp<-if(is.na(WxData$Prcp)|(is.na(WxData$Temp))){"NA"}else{(0+if(WxData$Prcp>Thresholds[5,5] & WxData$Temp>0){1}else{0}+if(WxData$Prcp>Thresholds[8,5] & WxData$Temp<0){1}else{0})}
      Land3Prcp<-if(is.na(WxData$Prcp)|(is.na(WxData$Temp))){"NA"}else{(0+if(WxData$Prcp>Thresholds[6,5] & WxData$Temp>0){1}else{0}+if(WxData$Prcp>Thresholds[9,5] & WxData$Temp<0){1}else{0})}
      
      Land1Wnd<-if(is.na(WxData$Wnd)|(is.na(WxData$Temp))){"NA"}else{(0+if(WxData$Wnd>Thresholds[10,5] & WxData$Temp>0){1}else{0}+if(WxData$Wnd>Thresholds[13,5] & WxData$Temp<0){1}else{0})}
      Land2Wnd<-if(is.na(WxData$Wnd)|(is.na(WxData$Temp))){"NA"}else{(0+if(WxData$Wnd>Thresholds[11,5] & WxData$Temp>0){1}else{0}+if(WxData$Wnd>Thresholds[14,5] & WxData$Temp<0){1}else{0})}
      Land3Wnd<-if(is.na(WxData$Wnd)|(is.na(WxData$Temp))){"NA"}else{(0+if(WxData$Wnd>Thresholds[12,5] & WxData$Temp>0){1}else{0}+if(WxData$Wnd>Thresholds[15,5] & WxData$Temp<0){1}else{0})}
      
      Land1Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[16,4]){1}else{0})}
      Land2Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[17,4]){1}else{0})}
      Land3Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[18,4]){1}else{0})}
      
      #Land total
      Land1Total<-as.numeric(c(Land1Tmp,Land1Prcp,Land1Vis,Land1Wnd))
      Land1Total<-if(sum(is.na(Land1Total))>0){"NA"}else{sum(Land1Total,na.rm=T)}
      
      Land2Total<-as.numeric(c(Land2Tmp,Land2Prcp,Land2Vis,Land2Wnd))
      Land2Total<-if(sum(is.na(Land2Total))>0){"NA"}else{sum(Land2Total,na.rm=T)}
      
      Land3Total<-as.numeric(c(Land3Tmp,Land3Prcp,Land3Vis,Land3Wnd))
      Land3Total<-if(sum(is.na(Land3Total))>0){"NA"}else{sum(Land3Total,na.rm=T)}
      
      #Ice model variables
      
      
      Ice1Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp>Thresholds[19,5]){1}else{0})}
      Ice2Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp>Thresholds[20,5]){1}else{0})}
      Ice3Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp>Thresholds[21,5]){1}else{0})}
      
      Ice1Prcp<-if(is.na(WxData$Prcp)){"NA"}else{(0+if(WxData$Prcp>Thresholds[22,5]){1}else{0})}
      Ice2Prcp<-if(is.na(WxData$Prcp)){"NA"}else{(0+if(WxData$Prcp>Thresholds[23,5]){1}else{0})}
      Ice3Prcp<-if(is.na(WxData$Prcp)){"NA"}else{(0+if(WxData$Prcp>Thresholds[24,5]){1}else{0})}
      
      Ice1Wnd<-if(is.na(WxData$Wnd)){"NA"}else{(0+if(WxData$Wnd>Thresholds[25,5]){1}else{0})}
      Ice2Wnd<-if(is.na(WxData$Wnd)){"NA"}else{(0+if(WxData$Wnd>Thresholds[26,5]){1}else{0})}
      Ice3Wnd<-if(is.na(WxData$Wnd)){"NA"}else{(0+if(WxData$Wnd>Thresholds[27,5]){1}else{0})}
      
      Ice1Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[28,4]){1}else{0})}
      Ice2Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[29,4]){1}else{0})}
      Ice3Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[30,4]){1}else{0})}
      
      Ice1Conc<-c(if(is.na(WxData$Con1)){1}else if(WxData$Con1<Thresholds[31,4]){1}else{0},
                  if(is.na(WxData$Con2)){1}else if(WxData$Con2<Thresholds[31,4]){1}else{0},
                  if(is.na(WxData$Con3)){1}else if(WxData$Con3<Thresholds[31,4]){1}else{0})
      
      Ice2Conc<-c(if(is.na(WxData$Con1)){1}else if(WxData$Con1<Thresholds[32,4]){1}else{0},
                  if(is.na(WxData$Con2)){1}else if(WxData$Con2<Thresholds[32,4]){1}else{0},
                  if(is.na(WxData$Con3)){1}else if(WxData$Con3<Thresholds[32,4]){1}else{0})
      
      Ice3Conc<-c(if(is.na(WxData$Con1)){1}else if(WxData$Con1<Thresholds[33,4]){1}else{0},
                  if(is.na(WxData$Con2)){1}else if(WxData$Con2<Thresholds[33,4]){1}else{0},
                  if(is.na(WxData$Con3)){1}else if(WxData$Con3<Thresholds[33,4]){1}else{0})
      
      Ice1Thick<-c(if(is.na(WxData$Thick1)){1}else if(WxData$Thick1<Thresholds[34,4]){1}else{0},
                   if(is.na(WxData$Thick2)){1}else if(WxData$Thick2<Thresholds[34,4]){1}else{0},
                   if(is.na(WxData$Thick3)){1}else if(WxData$Thick3<Thresholds[34,4]){1}else{0})
      
      Ice2Thick<-c(if(is.na(WxData$Thick1)){1}else if(WxData$Thick1<Thresholds[35,4]){1}else{0},
                   if(is.na(WxData$Thick2)){1}else if(WxData$Thick2<Thresholds[35,4]){1}else{0},
                   if(is.na(WxData$Thick3)){1}else if(WxData$Thick3<Thresholds[35,4]){1}else{0})
      
      Ice3Thick<-c(if(is.na(WxData$Thick1)){1}else if(WxData$Thick1<Thresholds[36,4]){1}else{0},
                   if(is.na(WxData$Thick2)){1}else if(WxData$Thick2<Thresholds[36,4]){1}else{0},
                   if(is.na(WxData$Thick3)){1}else if(WxData$Thick3<Thresholds[36,4]){1}else{0})
      
      #Ice totals
      Ice1Conc<-if(sum(Ice1Conc,na.rm = T)>1){1}else{0}
      Ice2Conc<-if(sum(Ice2Conc,na.rm = T)>1){1}else{0}
      Ice3Conc<-if(sum(Ice3Conc,na.rm = T)>1){1}else{0}
      
      Ice1Thick<-if(sum(Ice1Thick,na.rm=T)>1){1}else{0}
      Ice2Thick<-if(sum(Ice2Thick,na.rm=T)>1){1}else{0}
      Ice3Thick<-if(sum(Ice3Thick,na.rm=T)>1){1}else{0}
      
      Ice1Total<-as.numeric(c(Ice1Tmp,Ice1Prcp,Ice1Vis,Ice1Wnd,Ice1Conc,Ice1Thick))
      Ice1Total<-if(sum(is.na(Ice1Total))>0){"NA"}else{sum(Ice1Total,na.rm=T)}
      
      Ice2Total<-as.numeric(c(Ice2Tmp,Ice2Prcp,Ice2Vis,Ice2Wnd,Ice2Conc,Ice2Thick))
      Ice2Total<-if(sum(is.na(Ice2Total))>0){"NA"}else{sum(Ice2Total,na.rm=T)}
      
      Ice3Total<-as.numeric(c(Ice3Tmp,Ice3Prcp,Ice3Vis,Ice3Wnd,Ice3Conc,Ice3Thick))
      Ice3Total<-if(sum(is.na(Ice3Total))>0){"NA"}else{sum(Ice3Total,na.rm=T)}
      
      
      ####Water model variables
      
      Water1Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp<Thresholds[37,4]){1}else{0})}
      Water2Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp<Thresholds[38,4]){1}else{0})}
      Water3Tmp<-if(is.na(WxData$Temp)){"NA"}else{(0+if(WxData$Temp<Thresholds[39,4]){1}else{0})}
      
      Water1Prcp<-if(is.na(WxData$Prcp)){"NA"}else{(0+if(WxData$Prcp>Thresholds[40,5]){1}else{0})}
      Water2Prcp<-if(is.na(WxData$Prcp)){"NA"}else{(0+if(WxData$Prcp>Thresholds[41,5]){1}else{0})}
      Water3Prcp<-if(is.na(WxData$Prcp)){"NA"}else{(0+if(WxData$Prcp>Thresholds[42,5]){1}else{0})}
      
      Water1Wnd<-if(is.na(WxData$Wnd)){"NA"}else{(0+if(WxData$Wnd>Thresholds[43,5]){1}else{0})}
      Water2Wnd<-if(is.na(WxData$Wnd)){"NA"}else{(0+if(WxData$Wnd>Thresholds[44,5]){1}else{0})}
      Water3Wnd<-if(is.na(WxData$Wnd)){"NA"}else{(0+if(WxData$Wnd>Thresholds[45,5]){1}else{0})}
      
      Water1Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[46,4]){1}else{0})}
      Water2Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[47,4]){1}else{0})}
      Water3Vis<-if(is.na(WxData$Vis)){"NA"}else{(0+if(WxData$Vis<Thresholds[48,4]){1}else{0})}
      
      
      
      Water1Conc<-c(if(is.na(WxData$Con1)){0}else if(WxData$Con1>Thresholds[49,5]){1}else{0},
                    if(is.na(WxData$Con2)){0}else if(WxData$Con2>Thresholds[49,5]){1}else{0},
                    if(is.na(WxData$Con3)){0}else if(WxData$Con3>Thresholds[49,5]){1}else{0})
      
      Water2Conc<-c(if(is.na(WxData$Con1)){0}else if(WxData$Con1>Thresholds[50,5]){1}else{0},
                    if(is.na(WxData$Con2)){0}else if(WxData$Con2>Thresholds[50,5]){1}else{0},
                    if(is.na(WxData$Con3)){0}else if(WxData$Con3>Thresholds[50,5]){1}else{0})
      
      Water3Conc<-c(if(is.na(WxData$Con1)){0}else if(WxData$Con1>Thresholds[51,5]){1}else{0},
                    if(is.na(WxData$Con2)){0}else if(WxData$Con2>Thresholds[51,5]){1}else{0},
                    if(is.na(WxData$Con3)){0}else if(WxData$Con3>Thresholds[51,5]){1}else{0})
      
      #Water totals
      Water1Conc<-if(sum(Water1Conc,na.rm = T)>1){1}else{0}
      Water2Conc<-if(sum(Water2Conc,na.rm = T)>1){1}else{0}
      Water3Conc<-if(sum(Water3Conc,na.rm = T)>1){1}else{0}
      
      Water1Total<-as.numeric(c(Water1Tmp,Water1Prcp,Water1Vis,Water1Wnd,Water1Conc))
      Water1Total<-if(sum(is.na(Water1Total))>0){"NA"}else{sum(Water1Total,na.rm=T)}
      
      Water2Total<-as.numeric(c(Water2Tmp,Water2Prcp,Water2Vis,Water2Wnd,Water2Conc))
      Water2Total<-if(sum(is.na(Water2Total))>0){"NA"}else{sum(Water2Total,na.rm=T)}
      
      Water3Total<-as.numeric(c(Water3Tmp,Water3Prcp,Water3Vis,Water3Wnd,Water3Conc))
      Water3Total<-if(sum(is.na(Water3Total))>0){"NA"}else{sum(Water3Total,na.rm=T)}
      
      
      
      Out<-cbind(WxData,Land1Tmp,Land1Prcp,Land1Vis,Land1Wnd,Land1Total,Land2Total,Land3Total,Ice1Tmp,Ice1Prcp,Ice1Vis,Ice1Wnd,Ice1Conc,Ice1Thick,Ice1Total,Ice2Total,Ice3Total,
                 Water1Tmp,Water1Prcp,Water1Vis,Water1Wnd,Water1Conc,Water1Total,Water2Total,Water3Total)
      
      colnames(Out)<-cn
      
    }
    
    if (exists("All")){All<-rbind(All, Out)}else{(All<-Out)}
  }
  return(All) 
}



