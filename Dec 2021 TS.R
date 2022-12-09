#Winter2021 Fail Pass Calculations

## The TS do not use leap years

Thresholds<-read.csv("D:/R/LandUse/ArcticTracks/ArcticTracks/data/Threshold_GCM_Feb2022.csv")
VisTS<-read.csv("D:/R/LandUse/ArcticTracks/ArcticTracks/data/VisibilitySimulations.csv")
VisTS<-VisTS[,2:5]
GCMs<-list.files("D:/R/LandUse/ArcticTracks/GCMs",full.names=T)
GCMs<-GCMs[3]


DailyTrailTest_Forcst<-function(Thresholds,GCMs,VisTS){
  
  Mergcols<-c("Date","Region","GCM","RCP","Month","Decade","tas","pr","sfcwind","siconc","sithick","month2","VisSim")
  Outcols<-c("Date","Region","GCM","RCP","Month","Decade","tas","pr","sfcwind","siconc","sithick","month2","VisSim",
        "Land1Tmp","Land1Prcp","Land1Vis","Land1Wnd","Land1Total","Land2Total","Land3Total","Ice1Tmp","Ice1Prcp",
        "Ice1Vis","Ice1Wnd","Ice1Conc","Ice1Thick","Ice1Total","Ice2Total","Ice3Total","Water1Tmp","Water1Prcp","Water1Vis","Water1Wnd",
        "Water1Conc","Water1Total","Water2Total","Water3Total")
  
  for(i in GCMs){
      GCMts<-read.csv(i,header=T)
      GCMts<-GCMts[,1:11]
  
      EnvTS<-merge(x=GCMts,y=VisTS,by.x=c("zDate","Region"),by.y=c("zDate","Region"),all.x=T)
      colnames(EnvTS)<-Mergcols
            
            for(i in (1:nrow(EnvTS))){
              
              #Land model variables ----
              
              WxData<-EnvTS[i,]
              
                
                Land1Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas<Thresholds[1,4] & WxData$tas>Thresholds[1,5]){1}else{0})}
                Land2Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas<Thresholds[2,4] & WxData$tas>Thresholds[2,5]){1}else{0})}
                Land3Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas<Thresholds[3,4] & WxData$tas>Thresholds[3,5]){1}else{0})}
                
                Land1Prcp<-if(is.na(WxData$pr)|(is.na(WxData$tas))){"NA"}else{(0+if(WxData$pr>Thresholds[4,5] & WxData$tas>273.15){1}else{0}+if(WxData$pr>Thresholds[7,5] & WxData$tas<273.15){1}else{0})}
                Land2Prcp<-if(is.na(WxData$pr)|(is.na(WxData$tas))){"NA"}else{(0+if(WxData$pr>Thresholds[5,5] & WxData$tas>273.15){1}else{0}+if(WxData$pr>Thresholds[8,5] & WxData$tas<273.15){1}else{0})}
                Land3Prcp<-if(is.na(WxData$pr)|(is.na(WxData$tas))){"NA"}else{(0+if(WxData$pr>Thresholds[6,5] & WxData$tas>273.15){1}else{0}+if(WxData$pr>Thresholds[9,5] & WxData$tas<273.15){1}else{0})}
                
                Land1Wnd<-if(is.na(WxData$sfcwind)|(is.na(WxData$tas))){"NA"}else{(0+if(WxData$sfcwind>Thresholds[10,5] & WxData$tas>273.15){1}else{0}+if(WxData$sfcwind>Thresholds[13,5] & WxData$tas<273.15){1}else{0})}
                Land2Wnd<-if(is.na(WxData$sfcwind)|(is.na(WxData$tas))){"NA"}else{(0+if(WxData$sfcwind>Thresholds[11,5] & WxData$tas>273.15){1}else{0}+if(WxData$sfcwind>Thresholds[14,5] & WxData$tas<273.15){1}else{0})}
                Land3Wnd<-if(is.na(WxData$sfcwind)|(is.na(WxData$tas))){"NA"}else{(0+if(WxData$sfcwind>Thresholds[12,5] & WxData$tas>273.15){1}else{0}+if(WxData$sfcwind>Thresholds[15,5] & WxData$tas<273.15){1}else{0})}
                
                Land1Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[16,4]){1}else{0})}
                Land2Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[17,4]){1}else{0})}
                Land3Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[18,4]){1}else{0})}
                
                #Land total
                Land1Total<-as.numeric(c(Land1Tmp,Land1Prcp,Land1Vis,Land1Wnd))
                Land1Total<-if(sum(is.na(Land1Total))>0){"NA"}else{sum(Land1Total,na.rm=T)}
                
                Land2Total<-as.numeric(c(Land2Tmp,Land2Prcp,Land2Vis,Land2Wnd))
                Land2Total<-if(sum(is.na(Land2Total))>0){"NA"}else{sum(Land2Total,na.rm=T)}
                
                Land3Total<-as.numeric(c(Land3Tmp,Land3Prcp,Land3Vis,Land3Wnd))
                Land3Total<-if(sum(is.na(Land3Total))>0){"NA"}else{sum(Land3Total,na.rm=T)}
                
                #Ice model variables ----
                
                
                Ice1Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas>Thresholds[19,5]){1}else{0})}
                Ice2Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas>Thresholds[20,5]){1}else{0})}
                Ice3Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas>Thresholds[21,5]){1}else{0})}
                
                Ice1Prcp<-if(is.na(WxData$pr)){"NA"}else{(0+if(WxData$pr>Thresholds[22,5]){1}else{0})}
                Ice2Prcp<-if(is.na(WxData$pr)){"NA"}else{(0+if(WxData$pr>Thresholds[23,5]){1}else{0})}
                Ice3Prcp<-if(is.na(WxData$pr)){"NA"}else{(0+if(WxData$pr>Thresholds[24,5]){1}else{0})}
                
                Ice1Wnd<-if(is.na(WxData$sfcwind)){"NA"}else{(0+if(WxData$sfcwind>Thresholds[25,5]){1}else{0})}
                Ice2Wnd<-if(is.na(WxData$sfcwind)){"NA"}else{(0+if(WxData$sfcwind>Thresholds[26,5]){1}else{0})}
                Ice3Wnd<-if(is.na(WxData$sfcwind)){"NA"}else{(0+if(WxData$sfcwind>Thresholds[27,5]){1}else{0})}
                
                Ice1Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[28,4]){1}else{0})}
                Ice2Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[29,4]){1}else{0})}
                Ice3Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[30,4]){1}else{0})}
                
                Ice1Conc<-if(is.na(WxData$siconc)){1}else if(WxData$siconc<Thresholds[31,4]){1}else{0}
                Ice2Conc<-if(is.na(WxData$siconc)){1}else if(WxData$siconc<Thresholds[32,4]){1}else{0}
                Ice3Conc<-if(is.na(WxData$siconc)){1}else if(WxData$siconc<Thresholds[33,4]){1}else{0}
                
                Ice1Thick<-if(is.na(WxData$sithick)){1}else if(WxData$sithick<Thresholds[34,4]){1}else{0}
                Ice2Thick<-if(is.na(WxData$sithick)){1}else if(WxData$sithick<Thresholds[35,4]){1}else{0}
                Ice3Thick<-if(is.na(WxData$sithick)){1}else if(WxData$sithick<Thresholds[36,4]){1}else{0}
                
                #Ice totals
                
                Ice1Total<-as.numeric(c(Ice1Tmp,Ice1Prcp,Ice1Vis,Ice1Wnd,Ice1Conc,Ice1Thick))
                Ice1Total<-if(sum(is.na(Ice1Total))>0){"NA"}else{sum(Ice1Total,na.rm=T)}
                
                Ice2Total<-as.numeric(c(Ice2Tmp,Ice2Prcp,Ice2Vis,Ice2Wnd,Ice2Conc,Ice2Thick))
                Ice2Total<-if(sum(is.na(Ice2Total))>0){"NA"}else{sum(Ice2Total,na.rm=T)}
                
                Ice3Total<-as.numeric(c(Ice3Tmp,Ice3Prcp,Ice3Vis,Ice3Wnd,Ice3Conc,Ice3Thick))
                Ice3Total<-if(sum(is.na(Ice3Total))>0){"NA"}else{sum(Ice3Total,na.rm=T)}
                
                
                ####Water model variables ----
                
                Water1Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas<Thresholds[37,4]){1}else{0})}
                Water2Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas<Thresholds[38,4]){1}else{0})}
                Water3Tmp<-if(is.na(WxData$tas)){"NA"}else{(0+if(WxData$tas<Thresholds[39,4]){1}else{0})}
                
                Water1Prcp<-if(is.na(WxData$pr)){"NA"}else{(0+if(WxData$pr>Thresholds[40,5]){1}else{0})}
                Water2Prcp<-if(is.na(WxData$pr)){"NA"}else{(0+if(WxData$pr>Thresholds[41,5]){1}else{0})}
                Water3Prcp<-if(is.na(WxData$pr)){"NA"}else{(0+if(WxData$pr>Thresholds[42,5]){1}else{0})}
                
                Water1Wnd<-if(is.na(WxData$sfcwind)){"NA"}else{(0+if(WxData$sfcwind>Thresholds[43,5]){1}else{0})}
                Water2Wnd<-if(is.na(WxData$sfcwind)){"NA"}else{(0+if(WxData$sfcwind>Thresholds[44,5]){1}else{0})}
                Water3Wnd<-if(is.na(WxData$sfcwind)){"NA"}else{(0+if(WxData$sfcwind>Thresholds[45,5]){1}else{0})}
                
                Water1Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[46,4]){1}else{0})}
                Water2Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[47,4]){1}else{0})}
                Water3Vis<-if(is.na(WxData$VisSim)){"NA"}else{(0+if(WxData$VisSim<Thresholds[48,4]){1}else{0})}
                
                
                Water1Conc<-if(is.na(WxData$siconc)){0}else if(WxData$siconc>Thresholds[49,5]){1}else{0}
                Water2Conc<-if(is.na(WxData$siconc)){0}else if(WxData$siconc>Thresholds[50,5]){1}else{0}
                Water3Conc<-if(is.na(WxData$siconc)){0}else if(WxData$siconc>Thresholds[51,5]){1}else{0}
                
                #Water totals
                
                Water1Total<-as.numeric(c(Water1Tmp,Water1Prcp,Water1Vis,Water1Wnd,Water1Conc))
                Water1Total<-if(sum(is.na(Water1Total))>0){"NA"}else{sum(Water1Total,na.rm=T)}
                
                Water2Total<-as.numeric(c(Water2Tmp,Water2Prcp,Water2Vis,Water2Wnd,Water2Conc))
                Water2Total<-if(sum(is.na(Water2Total))>0){"NA"}else{sum(Water2Total,na.rm=T)}
                
                Water3Total<-as.numeric(c(Water3Tmp,Water3Prcp,Water3Vis,Water3Wnd,Water3Conc))
                Water3Total<-if(sum(is.na(Water3Total))>0){"NA"}else{sum(Water3Total,na.rm=T)}
                
                #----
                ### totals
                
                Out<-cbind(WxData,Land1Tmp,Land1Prcp,Land1Vis,Land1Wnd,Land1Total,Land2Total,Land3Total,Ice1Tmp,Ice1Prcp,Ice1Vis,Ice1Wnd,Ice1Conc,Ice1Thick,Ice1Total,Ice2Total,Ice3Total,
                           Water1Tmp,Water1Prcp,Water1Vis,Water1Wnd,Water1Conc,Water1Total,Water2Total,Water3Total)
                
                colnames(Out)<-Outcols
                
          
              
              if (exists("All")){All<-rbind(All, Out)}else{(All<-Out)}
              
              GCM<-EnvTS[1,3]
              print(paste0(GCM," ", nrow(All)))
            }
            
            fname<-paste0("D:/R/LandUse/ArcticTracks/ArcticTracks/data/",GCM,"_TrailDays.csv")
            write.csv(All,fname)
            remove(All)
            
      #finish GCM
     }
  
}


All<-DailyTrailTest_Forcst(Thresholds=Thresholds,GCMs=GCMs,VisTS=VisTS)









mkTrend <- function(x, ci = .95) {
  x = x
  z = NULL
  z0 = NULL
  pval = NULL
  pval0 = NULL
  S = 0
  Tau = NULL
  essf = NULL
  ci = ci
  if (is.vector(x) == FALSE) {
    stop("Input data must be a vector")
  }
  if (any(is.finite(x) == FALSE)) {
    x[-c(which(is.finite(x) == FALSE))] -> x
    warning("The input vector contains non-finite numbers. An attempt was made to remove them")
  }
  n <- length(x)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      S = S + sign(x[j]-x[i])
    }
  }
  acf(rank(lm(x ~ I(1:n))$resid), lag.max=(n-1), plot=FALSE)$acf[-1] -> ro
  qnorm((1+ci)/2)/sqrt(n) -> sig
  rep(NA,length(ro)) -> rof
  for (i in 1:(length(ro))) {
    if(ro[i] > sig || ro[i] < -sig) {
      rof[i] <- ro[i]
    } else {
      rof[i] = 0
    }
  }
  2 / (n*(n-1)*(n-2)) -> cte
  ess=0
  for (i in 1:(n-1)) {          
    ess = ess + (n-i)*(n-i-1)*(n-i-2)*rof[i]
  }
  essf = 1 + ess*cte
  var.S = n*(n-1)*(2*n+5)*(1/18) 
  if(length(unique(x)) < n) {
    unique(x) -> aux
    for (i in 1:length(aux)) {
      length(which(x == aux[i])) -> tie
      if (tie > 1) {
        var.S = var.S - tie*(tie-1)*(2*tie+5)*(1/18)  
      }
    }
  }
  VS = var.S * essf            
  if (S == 0) {
    z = 0
    z0 = 0
  }
  if (S > 0) {
    z = (S-1)/sqrt(VS) 
    z0 = (S-1)/sqrt(var.S)
  } else {
    z = (S+1)/sqrt(VS) 
    z0 = (S+1)/sqrt(var.S)
  }      
  pval = 2*pnorm(-abs(z))
  pval0 = 2*pnorm(-abs(z0)) 
  Tau = S/(.5*n*(n-1))
  rep(NA, times=(n^2-n)/2) -> V
  k = 0
  for (i in 2:n) {
    for (j in 1:(n-1)) {
      k = k+1
      V[k] = (x[i]-x[j])/(i-j)
      
    }
  }
  median(na.omit(V)) -> slp
  return(list("Z" = z0, "p.value" = pval0, "Zc" = z, "Corrected p.value" = pval, "tau" = Tau, "N/N*s" = essf, "Sen's Slope" = slp))
}

TrendAnalysis<-function(DataTS,checkAllIndex,checkDeSeas,checkDownload,TrailType){
  
  regions<-sort(unique(DataTS$Region))
  RCPs<-sort(unique(DataTS$RCP))
  
  
  if(TrailType=="Land"){
    
    if(checkAllIndex==T){
      Indexes<-c(7,8,9,10,11,13,14,15,16,17,38,39,40)
    }else if (checkAllIndex==F){
      Indexes<-c(14,15,16,17,38,39,40)
    }
    
  }else if(TrailType=="Ice"){
    if(checkAllIndex==T){
      Indexes<-c(7,8,9,10,11,13,21:26,41,42,43)
    }else if (checkAllIndex==F){
      Indexes<-c(21:26,41,42,43)
    }
    
  }else if(TrailType=="Water"){
    if(checkAllIndex==T){
      Indexes<-c(7,8,9,10,11,13,30:34,44,45,46)
    }else if (checkAllIndex==F){
      Indexes<-c(30:34,44,45,46)
    }
  }
  
  
  for (r in RCPs)
    
    df<-DataTS[DataTS[,"RCP"]==r,]
  
  for(b in regions){
    
    region<-as.character(b)
    df1<-df[df[,"Region"]==region,]
    df1$Date<-as.Date(strptime(df1$Date, format="%m/%d/%Y"))
    
    for (i in Indexes){
      col<-i
      ts1 <- zoo(df1[,col], order.by = df1$Date)
      ts.st<-stlplus(ts1, s.window = "periodic", n.p=365)
      SeasAdjust<-as.data.frame(ts.st$data$remainder + ts.st$data$trend)
      
      ## Seasonality removed plot
      ts2 <- as.zooreg(zoo(SeasAdjust[,1], order.by = df1$Date))
      
      if(checkDeSeas==T){
        plotfile<-paste0("data/plots/SeaAdj/",region,col,".jpeg")
        jpeg(filename=plotfile)
        plot(ts2, main=region, ylab=col)
        dev.off()
      }
      
      ## MKTrend for de-seasonalized data
      ts.df<-as.data.frame(SeasAdjust[,1])
      mk<-mkTrend(as.vector(ts.df[,1]))
      mk.df<-as.data.frame(mk)
      mk.df$region<-region
      mk.df$index<-col
      mk.df$RCP<-r
      
      if(exists("Sall")){Sall<-rbind(Sall, mk.df)}else{(Sall<-mk.df)}
      
    }
    
  }
  
  return(Sall)
}




################# Historical TS code





if(ncol(historicTS<28)){
  All<-DailyTrailTest(Thresholds = UserThresholds,WxDataAll = historicTS)
  write.csv(All,"data/historicTS.csv")
  historicTS<-All
  historicTS[,8:50] <- sapply(historicTS[,8:50],as.character)
  historicTS[,8:50] <- sapply(historicTS[,8:50],as.numeric)
  
  
  
  MkHistAnalysis<-TrendAnalysis(DataTS=All,Indexes=c(8:50))
  # MkAnalysis<-TrendAnalysis(DataTS=historicTS,Indexes=40)
  
  write.csv(MkHistAnalysis,"data/analysis/MkHistAnalysis.csv")
  
}else{
  MkHistAnalysis<-read.csv("data/analysis/MkHistAnalysis.csv")
}

# if(ncol(projectedTS<10)){
#for i in (1:count(unique(projectedTS$GCM))){

#}
