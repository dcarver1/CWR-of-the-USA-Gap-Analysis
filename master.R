# This function runs the entire process for a selected species
# @param (chr) species: species ID
# @return (dir): status of run


#here for troubleshooting.
#species <- species1

master_run <- function(species) {
  species <<- species
  print(paste0("the process for ", species, " has begun."))
  # build a datframe that captures the total run time for a process.
  time_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(time_df) <- c("functionUsed", "runTime")
  # start time for totaling run time at the end 
  startTime <- Sys.time()

  t1a <- Sys.time()
  cat("...creating directories\n")
  spp_dirs <- create_sp_dirs(species)
  time_df <- rbind(time_df, data.frame(functionUsed="create_sp_dirs",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # sp_dir <<- primary directory for this run 

  t1a <- Sys.time()
  cat("...developing raw data\n")
  spp_raw <- developRaw(species)
  time_df <- rbind(time_df, data.frame(functionUsed="developing raw data",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # rawData <<- listing of all data for the species 

  t1a <- Sys.time()
  cat("...generating counts csv\n")
  genCounts2 <- genCounts(species)
  time_df <- rbind(time_df, data.frame(functionUsed="genCounts",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # dataThin <<- raw data with complete lat long and only necessary 
  
  t1a <- Sys.time()
  cat("...creating spdataframe\n")
  spp_point <- spPoints(species)
  time_df <- rbind(time_df, data.frame(functionUsed="spPoints",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # spPoint <<- all usable occurence data for NA 
  
  t1a <- Sys.time()
  cat("...adding North American Points to Counts.csv\n")
  naPoints <- addNorthAmericanCounts(species)
  time_df <- rbind(time_df, data.frame(functionUsed="addNorthAmericanCounts",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # spPoint <<- all usable occurence data for NA 
  


  if(class(spPoint)== "character"){
    print("there is not locational data for this species this is the end of the modeling process")
    lowOccurence <- append(lowOccurence, species)
  }else{
    t1a <- Sys.time()
    cat("...extracting country values to points and remove duplicate lat long\n")
    landPoints <- countryCheck(species)
    time_df <- rbind(time_df, data.frame(functionUsed="countryCheck",  runTime=difftime(Sys.time(), t1a, units='secs')))
    #xyData <<- coords for elements in spPoint object
    #cleanPoints <<- spatial points layer were all points are on land.

    t1a <- Sys.time()
    cat("...spatial sample to under 2000 tota\n")
    subSample <- sampling(species)
    time_df <- rbind(time_df, data.frame(functionUsed="sampling",  runTime=difftime(Sys.time(), t1a, units='secs')))
    # cleanPoints <<- if spatail thining is applied the cleanPoint var is redeclared.
    
    t1a <- Sys.time()
    cat("...generate native area shp\n")
    natArea <- nat_area_shp(species)
    time_df <- rbind(time_df, data.frame(functionUsed="nat_area_shp",  runTime=difftime(Sys.time(), t1a, units='secs')))
    # ecoVal <<- df of all eco_id present in the model. used in ERSex 
    # ecoAreas <<- ecoregions that intersect with cleanPoints
    # nativeArea <<- ecoregions clipped to countries with species present
    
    t1a <- Sys.time()
    cat("...generate ga50Raster\n")
    createBuffer <- create_buffers(species)
    time_df <- rbind(time_df, data.frame(functionUsed="create_buffers",  runTime=difftime(Sys.time(), t1a, units='secs')))
    # No global variables declared
    
    t1a <- Sys.time()
    cat("...generate background and extract raster data to background and presence data\n")
    genModelData <- generateModelingData(species)    
    time_df <- rbind(time_df, data.frame(functionUsed="generateModelingData",  runTime=difftime(Sys.time(), t1a, units='secs')))
    # bioValues <<- Presence and background points with predictor data attached
    
    t1a <- Sys.time()
    cat("...perform variable selection and correlation\n")
    variableSelect <- varaibleSelection(species)
    time_df <- rbind(time_df, data.frame(functionUsed="varaibleSelection",  runTime=difftime(Sys.time(), t1a, units='secs')))
    # bioValues <<- redefined if any NA are present in predictor datasets.
    # variblesToModel <<- a listing of variable names used to modeling
    
    t1a <- Sys.time()
    cat("...perform maxent model\n")
    run_Maxnet <- try(runMaxnet(species))
    time_df <- rbind(time_df, data.frame(functionUsed="runMaxnet",  runTime=difftime(Sys.time(), t1a, units='secs')))
    # sdm_results <<- output of the Maxnet modeling process

    if(!file.exists(paste0(sp_dir, "/sdm.rds"))){
     print("the model did not successfully run")
     notModeled <- append(notModeled,species)
    }else{    
      
      t1a <- Sys.time()
      #this code produces the threshold raster 
      test1 <- evaluate_sdm_function(species)
      time_df <- rbind(time_df, data.frame(functionUsed="evaluate_sdm_function",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...create a mess map based on top predictor \n")
      messMap(species)
      time_df <- rbind(time_df, data.frame(functionUsed="messMap",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...evaluating the  model\n")
      evaluateFunction <- evaluate_function(species)
      time_df <- rbind(time_df, data.frame(functionUsed="evaluate_function",  runTime=difftime(Sys.time(), t1a, units='secs')))

      
      t1a <- Sys.time()
      cat("...conducting SRSex assessment\n")
      srsExsitu <- srs_exsitu(species)
      time_df <- rbind(time_df, data.frame(functionUsed="srs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...conducting GRSex assessment\n")
      grsExsitu <- grs_exsitu(species)
      time_df <- rbind(time_df, data.frame(functionUsed="grs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...conducting ERSex assessment\n")
      ersExsitu <- ers_exsitu(species)
      time_df <- rbind(time_df, data.frame(functionUsed="ers_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...conducting  fcsex assessment\n")
      fcsExsitu <- fcs_exsitu(species)
      time_df <- rbind(time_df, data.frame(functionUsed="fcs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
    
      cat("...Ex situ assessment is complete\n")
    
      t1a <- Sys.time()
      cat("...conducting srs In assessment\n")
      grsInsitu <- srs_insitu(species)
      time_df <- rbind(time_df, data.frame(functionUsed="srs_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...conducting grs In assessment\n")
      grsInsitu <- insitu_grs(species)
      time_df <- rbind(time_df, data.frame(functionUsed="insitu_grs",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...conducting ers In assessment\n")
      ersInsitu <- ers_insitu(species)
      time_df <- rbind(time_df, data.frame(functionUsed="ers_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...conducting fcs In assessment\n")
      fcssInsitu <- fcs_insitu(species)
      time_df <- rbind(time_df, data.frame(functionUsed="fcs_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))

      cat("...in situ assessment is complete\n")
      
      t1a <- Sys.time()
      cat("...conducting fcs combined assessment\n")
      fcsCombine <- fcs_combine(species)
      time_df <- rbind(time_df, data.frame(functionUsed="fcs_combine",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      cat("...conducting EOO and AOO assessment\n")
      redlist <- eooAoo(species)
      time_df <- rbind(time_df, data.frame(functionUsed="eooAoo",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      t1a <- Sys.time()
      rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/singleSpeciesSummary.rmd"),
                        output_file =  paste("report_", species, '_', run_version,'_' , Sys.Date(), ".html", sep=''),
                        output_dir = paste0(sp_dir))
      time_df <- rbind(time_df, data.frame(functionUsed="sinlge species Summary",  runTime=difftime(Sys.time(), t1a, units='secs')))
      time_df$Minutes <- time_df$runTime/60 
      write.csv(x = time_df, file = paste0(sp_dir, "/time_df.csv"))
       print(paste0("the process for ", species, " has ended."))
      
      }
  }
}
