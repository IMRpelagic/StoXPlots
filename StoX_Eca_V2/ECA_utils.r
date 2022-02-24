
convertToICESareaHerring <- function(file){
  
  omr = file$hovedområdeKode
  lok = file$lokasjonKode
  
  ices = file$hovedområdeKode
  
  ices[ices%in%c(9)]<-'3a'
  ices[ices%in%c(8,28)]<-'4a_east'
  ices[ices%in%c(42)]<-'4a_west'
  ices[ices%in%c(41)]<-'4b'
  ices[ices%in%c(40)]<-'4c'
  ices[ices%in%c(57,58,31)]<-'5b'
  ices[ices%in%c(43)]<-'6a'
  ices[ices%in%c(47)]<-'6b'
  
  ices[(ices%in%c(48))&(lok%in%c(26,27,28,29,30,31,18,19,20,21,22,23,10,11,12,13,14,15,1,2,3,4,5,6))]<-'7c'
  ices[(ices%in%c(48))&(lok%in%c(32,33,34,24,25,16,17,7,8,9))]<-'7b'
  
  ices[(ices%in%c(49))&(lok%in%c(1,2,3,18,19,20,21,22,23,31,32,33,34,35,36,42,43,44,45,46,47,53,54,55,56,57,58,65,66,67,68,69,70,78,79,80,81,82,83,91,92,93,94,95,96))]<-'7k'
  ices[(ices%in%c(49))&(lok%in%c(4,5,6,11,12,13,24,25,26,37,38,39,48,49,50,59,60,61,71,72,73,84,85,86,97,98,99))]<-'7i'
  ices[(ices%in%c(49))&(lok%in%c(7,8,9,10,14,15,16,17,27,28,29,30,40,41))]<-'7h'
  ices[(ices%in%c(49))&(lok%in%c(51,52,62,63,64,74,75,76,77,87,88,89,90))]<-'7g'
  
  ices[ices%in%c(1,2,3,10,11,13,14,15,16,17,18,24)]<-'1'
  ices[ices%in%c(0,4,5,6,7,12,30,34,36,37,39)]<-'2a'
  ices[ices%in%c(20,21,22,23,25,26,27)]<-'2b'
  
  mapp<- unique(as.data.frame(list(omr=omr,lok=lok,ices=ices)))
  
  mapp$tmp <-sprintf("%02d-%02d",mapp$omr,mapp$lok)
  tmp <- unique(mapp[c('ices','tmp')])
  
  test <- c()
  for(ice in unique(ices)){
    test[toString(ice)] =list(tmp[tmp$ices==ice,]$tmp)
  }
  return(test)
}


convertToICESarea <- function(file){
  
  omr = file$hovedområdeKode
  lok = file$lokasjonKode
  
  ices = file$hovedområdeKode
  
  
  ices[ices%in%c(8,28,42)]<-'4a'
  ices[ices%in%c(40)]<-'4c'
  ices[ices%in%c(9)]<-'3a'
  ices[ices%in%c(0,4,5,6,7,12,30,34,36,37,39)]<-'2a'
  ices[ices%in%c(41)]<-'4b'
  ices[ices%in%c(43)]<-'6a'
  ices[ices%in%c(47)]<-'6b'
  
  ices[ices%in%c(57,58,31)]<-'5b'
  
  ices[(ices%in%c(48))&(lok%in%c(26,27,28,29,30,31,18,19,20,21,22,23,10,11,12,13,14,15,1,2,3,4,5,6))]<-'7c'
  ices[(ices%in%c(48))&(lok%in%c(32,33,34,24,25,16,17,7,8,9))]<-'7b'
  
  ices[(ices%in%c(49))&(lok%in%c(1,2,3,18,19,20,21,22,23,31,32,33,34,35,36,42,43,44,45,46,47,53,54,55,56,57,58,65,66,67,68,69,70,78,79,80,81,82,83,91,92,93,94,95,96))]<-'7k'
  ices[(ices%in%c(49))&(lok%in%c(4,5,6,11,12,13,24,25,26,37,38,39,48,49,50,59,60,61,71,72,73,84,85,86,97,98,99))]<-'7i'
  ices[(ices%in%c(49))&(lok%in%c(7,8,9,10,14,15,16,17,27,28,29,30,40,41))]<-'7h'
  ices[(ices%in%c(49))&(lok%in%c(51,52,62,63,64,74,75,76,77,87,88,89,90))]<-'7g'
  
  ices[ices%in%c(1,2,3,10,11,13,14,15,16,17,18,24)]<-'1'
  ices[ices%in%c(20,21,22,23,25,26,27)]<-'2b'
  
  
  mapp<- unique(as.data.frame(list(omr=omr,lok=lok,ices=ices)))
  
  mapp$tmp <-sprintf("%02d-%02d",mapp$omr,mapp$lok)
  tmp <- unique(mapp[c('ices','tmp')])
  
  test <- c()
  for(ice in unique(ices)){
    test[toString(ice)] =list(tmp[tmp$ices==ice,]$tmp)
  }
  
  
  return(test)
  
  
}



CatchMap<-function(file,strata,strata2,biotic,path = NULL){
  
  
  map <- get_map(location=c(left= -45, bottom=45, right=70 ,top=83))
  world <- ggmap(map, extent="normal")
  file$landingsdato<-as.Date(file$landingsdato)
  strata$hovedområdeKode <- strata$omr
  strata_tmp <-aggregate(strata[, 3:4], list(strata$omr), mean)
  strata_tmp$hovedområdeKode<-strata_tmp$Group.1
  
  tmp <- merge(file,strata_tmp)
  
  sum_rund <- aggregate(tmp[,names(tmp)=='rundvekt'],list(tmp$hovedområdeKode),sum)
  sum_rund$hovedområdeKode<-sum_rund$Group.1
  
  
  tmp <- merge(tmp,sum_rund)
  
  p<-world +
    geom_path(data = strata2$lonlatAll,aes(x=longitude,y=latitude,colour=stratum), show.legend = FALSE)+
    geom_point(data = tmp,aes(lon,lat,size=x/1000),colour='blue')+
    geom_point(data = biotic$FilterBiotic_BioticData_fishstation.txt,aes(longitudestart,latitudestart),colour='red')+
    geom_text(data=tmp,aes(lon,lat,label=hovedområdeKode),vjust=1,hjust=1)+
    scale_size_continuous(breaks = c(0,1,10,100,1000,10000,100000,1000000),name = 'Catch in ton')
  
  p<-p+xlim(min(biotic$FilterBiotic_BioticData_fishstation.txt$longitudestart)-5,max(biotic$FilterBiotic_BioticData_fishstation.txt$longitudestart)+5)+
    ylim(min(biotic$FilterBiotic_BioticData_fishstation.txt$latitudestart)-5,max(biotic$FilterBiotic_BioticData_fishstation.txt$latitudestart)+5)
  
  show(p)
  if(!is.null(path))ggsave(paste0(path,'/output/plot.jpg'))
  
}



ageDistribution<-function(BioticCovData,path = NULL){
  
  age_tmp <- BioticCovData[c('age','Temporal','Spatial','stationstartdate','serialnumber')]
  x<-as.yearqtr(age_tmp$stationstartdate,format='%d/%m/%Y')
  age_tmp$quarter <-format(x, "%q")
  
  p<-ggplot(data=age_tmp,aes(x=age,group=interaction(Temporal,Spatial),
                          color=interaction(Temporal,Spatial),
                          fill=interaction(Temporal,Spatial)))+
    geom_histogram(alpha=0.6, binwidth = 1) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    # theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("age") +
    ylab("Age distribution") +
    facet_wrap(~interaction(Temporal,Spatial), scales = "free")+ggtitle(toString(unique(BioticCovData$commonname)))
  show(p)
  if(!is.null(path))
  ggsave(paste0(path,'/output/agedistribution.jpg'))
}



CatchDevelopment<-function(biotic,file,path = NULL){
  
  sample_dates <- biotic$FilterBiotic_BioticData_fishstation.txt$stationstartdate
  
  tmp2<-as.data.frame(sample_dates)
  tmp2$y<-0
  
  year = unique(file$fangstår)
  
  p<-ggplot(file,aes(x=as.Date(sisteFangstdato),y=rundvekt))+
    stat_summary(data = file, fun.y = sum,geom = "bar")+
    geom_vline(aes(xintercept=as.Date(c(paste0(year,"-01-01")))),colour='red')+
    geom_vline(aes(xintercept=as.Date(c(paste0(year,"-04-01")))),colour='red')+
    geom_vline(aes(xintercept=as.Date(c(paste0(year,"-07-01")))),colour='red')+
    geom_vline(aes(xintercept=as.Date(c(paste0(year,"-10-01")))),colour='red')+
    geom_vline(aes(xintercept=as.Date(c(paste0(year,"-12-31")))),colour='red')
  
  p<-p+geom_point(data=tmp2,aes(x=as.Date(sample_dates),y=y),colour='green')
  
  show(p)
  if(!is.null(path))ggsave(paste0(path,'/output/plot_catch.jpg'))
}