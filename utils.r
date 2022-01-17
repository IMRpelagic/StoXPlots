

###############################################################################
#Prepare figure 5.2 in report
###############################################################################
Track_plot<-function(df,works,save_path = NULL){
StratumShape <- df$DefineStratumPolygon
tracklines <- df$StoxAcoustic$Log
stations <- df$StoxBiotic$Station
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[which(world$continent == "Europe"),]

p<-ggplot(world)+
  geom_sf(fill='black')+ 
  geom_point(data=tracklines,aes(x=Longitude,y=Latitude,group=NULL,colour=CruiseKey),size=0.1) +
  geom_point(data=stations,aes(Longitude,y=Latitude,group=NULL))+
  coord_sf(xlim = c(-16,15), ylim = c(50,63), expand = FALSE)+
  geom_polygon(data=StratumShape, aes(long, lat, group=group),colour='red',
               fill=NA) 

for(w in works){
  p<-p+geom_polygon(data=w,aes(x,y,group=NULL),col='red',fill=NA)
}
if(!is.null(save_path)){
ggsave(save_path)
}
return(p)
}



###############################################################################
#General function for plotting NASC for different species
###############################################################################
NASC_plot<-function(df,cat='HER',works,save_path = NULL){
  #Grab world map
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- world[which(world$continent == "Europe"),]
  
  #Grab Stratum Shape
  StratumShape <- df$DefineStratumPolygon
  #Tracklines
  tracklines <- df$StoxAcoustic$Log
  #Grab data and compute the mean nasck from NASCToStoxAcoustic
  NASC <- as.data.frame(RstoxBase::SumNASC(NASCData = RstoxBase::NASC(df$NASCToStoxAcoustic))$Data)
  NASC <- NASC[NASC$NASC>0,]
  NASC$NASC<-NASC$NASC*NASC$EffectiveLogDistance
  
  p<-ggplot(world)+
    geom_sf(fill='black')+ 
    geom_polygon(data=StratumShape, aes(long, lat, group=group),colour='red',
                 fill=NA) +
    geom_point(data=tracklines,aes(x=Longitude,y=Latitude,group=NULL),size=0.1,colour='grey')+
    geom_point(data=NASC[NASC$AcousticCategory==cat,],
               aes(x=Longitude,y=Latitude,group=NULL,size=NASC),
               colour='darkgreen',alpha=0.4)+
    coord_sf(xlim = c(-16,15), ylim = c(50,63), expand = FALSE)+ggtitle(cat)
  
  for(w in works){
    p<-p+geom_polygon(data=w,aes(x,y,group=NULL),col='red',fill=NA)
  }
  p<-p+
    scale_size_area(max_size = 15,name = TeX("NASC $m^2 nm^{-2}$"),
                    breaks=c(500,1000,2500,5000,10000),
                    labels=c(500,1000,2500,5000,10000))+ 
    theme(legend.position = c(0.1, 0.8))+xlab('Longitude')+ylab('Latitude')
  if(!is.null(save_path)){
  ggsave(save_path)
    }
  return(p)
  
}
