###
# Calculate the proportion of points that fall within a protected areas. Insitu SRS 
# 20191002
# carver.dan1@gmail.com
###


#species <- species1
srs_insitu <- function(species) {
  #load config
  config(dirs=T,exsitu=T,Country=F)
  
  #directory for species

  sp_dir <- paste0(gap_dir,"/",species)

  ## read in protected areas raster 
  protArea <- raster::raster(paste0(base_dir,"/parameters/protected_areas/raster/wdpa_reclass.tif"))
  
  
  ##read in data
  data <- read.csv(paste(occ_dir,"/raw/",species,".csv",sep=""),header=T)
  data <- data[complete.cases(data),]
  
  #total number of points 
  totalNum <- nrow(data)
  
  # create a spatail points object 
  coordinates(data) = ~ longitude + latitude
  # set coodinate systems equal  
  crs(data) = crs(protArea)
  # run a extract to values and select all data. Test for NA, then sum true values for total number of points in protected
  #areas 
  protectPoints <- sum(!is.na(raster::extract(x = protArea,y = data)))
  
  #define SRS 
  if(protectPoints >= 0 ){
    srsInsitu <- 100 *(protectPoints/totalNum)
  }else{
    srsInsitu <- 0
  }
  
  
  #create data.frame with output
  out_df <- data.frame(ID=species, 
                       NTOTAL=totalNum,
                       ProTotal = protectPoints,
                       SRS=srsInsitu)
  write.csv(out_df,paste0(sp_dir,"/",run_version,"/gap_analysis/insitu/srs_result.csv"),row.names=F)
  
  
  #return object
  return(out_df)
}

