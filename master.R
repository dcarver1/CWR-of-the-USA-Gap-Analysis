# This function runs the entire process for a selected species
# @param (chr) species: species ID
# @return (dir): status of run


#here for troubleshooting.
#species <- species1

master_run <- function(species) {
  species <<- species
  
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
  spp_raw <- genCounts(species)
  time_df <- rbind(time_df, data.frame(functionUsed="genCounts",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # no global variables
  
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
    append(lowOccurence, species)
  }else{
    cat("...extracting country values to points\n")
    landPoints <- testLand(species)
    #xyData <<- coords for elements in spPoint object
    #cleanPoints <<- spatial points layer were all points are on land.

    cat("...spatial sample to under 2000 total\n")
    subSample <- sampling(species)
    # cleanPoints <<- if spatail thining is applied the cleanPoint var is redeclared.

    cat("...generate native area shp\n")
    natArea <- nat_area_shp(species)
    # ecoAreas <<- ecoregions that intersect with cleanPoints
    # nativeArea <<- ecoregions clipped to countries with species present

    cat("...generate ga50Raster\n")
    createBuffer <- create_buffers(species)
    # No global variables declared

    cat("...generate background and extract raster data to background and presence data\n")
    genModelData <- generateModelingData(species)
    # bioValues <<- Presence and background points with predictor data attached

    cat("...perform variable selection and correlation\n")
    variableSelect <- varaibleSelection(species)
    # bioValues <<- redefined if any NA are present in predictor datasets.
    # variblesToModel <<- a listing of variable names used to modeling

    cat("...perform maxent model\n")
    run_Maxnet <- runMaxnet(species)
    # sdm_results <<- output of the Maxnet modeling process

    if(!file.exists(paste0(sp_dir, "/sdm.rds"))){
     print("the model did not successfully run")
     append(notModeled,species)
    }else{

      cat("...evaluating the  model\n")
      evaluateFunction <- evaluate_function(species)


      cat("...conducting SRSex assessment\n")
      srsExsitu <- srs_exsitu(species)


      cat("...conducting GRSex assessment\n")
      grsExsitu <- grs_exsitu(species)


      cat("...conducting ERSex assessment\n")
      ersExsitu <- ers_exsitu(species)

      cat("...conducting ERSex assessment\n")
      fcsExsitu <- fcs_exsitu(species)

      cat("...Ex situ assessment is complete\n")

      cat("...conducting grsIn assessment\n")
      grsInsitu <- insitu_grs(species)

      cat("...conducting ersIn assessment\n")
      ersInsitu <- ers_insitu(species)

      cat("...conducting ersIn assessment\n")
      fcssInsitu <- fcs_insitu(species)

      cat("...in situ assessment is complete\n")

      cat("...conducting fcs combined assessment\n")
      fcsCombine <- fcs_combine(species)

      rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/singleSpeciesSummary.rmd"),
                        output_file =  paste("report_", species, '_', run_version,'_' , Sys.Date(), ".html", sep=''),
                        output_dir = paste0(sp_dir))
    }
  }
}
