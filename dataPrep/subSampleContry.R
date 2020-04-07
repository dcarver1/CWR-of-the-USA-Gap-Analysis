###
# sub sample feautres based on country if there are over 2000 
# 20190828
# carver.dan1@gmail.com 
###

#Maria Victoria Diaz
#CIAT, 2018

# This function makes a stratified sampling of the species that have more than 2000 ocurrences
# @param (string) species: Species ID.
# @return (dataFrame): This function return a DataFrame with a sample of the total records of the species. 

#species="5358748"


sampling<-function(species){
  d1 <- cleanPoints@data %>%
    dplyr::filter(type == "H")
  dG <- cleanPoints@data %>% 
    dplyr::filter(type == "G")
  countries<- unique(na.omit(d1$iso3_check))
  count_occ<-nrow(d1)

  p<-c()
  n<-c()
  x<-data.frame()
  y<-c()
  muestra<-list()
  
  
  for(i in 1:length(countries)){
    
    n[i]<-nrow(d1[which(d1$iso3_check==countries[i]),])
    p[i]<-n[i]/count_occ
    
  }
  if(count_occ>= numPoints+1){
    
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
  
  cleanData <<-rbind(data.frame(muestra), dG)
  write.csv(cleanData, file = paste0(sp_dir,"/cleanedModelingData.csv"),row.names = FALSE)
  cleanPoints <<- SpatialPointsDataFrame(coords = cleanData[,c(3,2)], data = cleanData)
  raster::crs(cleanPoints) <- raster::crs(ecoReg)
}




