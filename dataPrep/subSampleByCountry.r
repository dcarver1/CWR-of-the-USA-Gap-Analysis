###
# sub sample feautres based on country if there are over 2000
# 20200414
# dan.carver@carverd.com
# based on work by
# Maria Victoria Diaz
# CIAT, 2018
###

subSampleByCountry<-function(species){
  set.seed(1234)
  # the 2000 occurrence limit only applies to H points. This spilts the data.
  d1 <- cleanPoints@data %>%
    dplyr::filter(type == "H")
  dG <- cleanPoints@data %>%
    dplyr::filter(type == "G")
  # determine number of unique countries
  countries<- unique(na.omit(d1$iso3_check))
  count_occ<-nrow(d1)

  # numPoints is a user define variable in runLineal, it is set to 2000
  if(count_occ >= numPoints+1){
    p<-c()
    n<-c()
    x<-data.frame()
    y<-c()
    muestra<-list()


    for(i in 1:length(countries)){
      n[i]<-nrow(d1[which(d1$iso3_check==countries[i]),])
      p[i]<-n[i]/count_occ
    }
    # this is the subsample method from install_github("DFJL/SamplingUtil")
    nsizeProp<-nstrata(n=numPoints,wh=p,method="proportional")
    smple<-list()
    for(i in 1:length(countries)){
      smple[[i]]<-sample(rownames(d1[which(d1$iso3_check==countries[i]),]), size=nsizeProp[i], replace=F)
      muestra[[i]]<-d1[smple[[i]],]

    }

    muestra<- do.call(rbind, muestra)


  }else{

    muestra<-d1
  }
  cleanData <- rbind(data.frame(muestra), dG)
  write.csv(cleanData, file = paste0(sp_dir,"/cleanedModelingData.csv"),row.names = FALSE)
  # reassign cleanPoints to represent the subsampled data
  cleanPoints <<- SpatialPointsDataFrame(coords = cleanData[,c(3,2)], data = cleanData)
  raster::crs(cleanPoints) <- raster::crs(ecoReg)
}
