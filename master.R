###
# Primary function to call all functions that are a part of the modeling method. 
# dan.carver@carver.com 
# 20200414
### 

master_run <- function(species){
  species <<- species
  print(paste0("the process for ", species, " has begun."))
  
  # build a datframe that captures the total run time for a process.
  time_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(time_df) <- c("functionUsed", "runTime")
  
  # start time for totaling run time at the end
  startTime <- Sys.time()
  
  t1a <- Sys.time()
  cat("...creating directories\n")
  create_sp_dirs(species)
  time_df <- rbind(time_df, data.frame(functionUsed="create_sp_dirs", 
                                       runTime=difftime(Sys.time(), t1a, units='secs')))
  # sp_dir <<- primary directory for this run
  
  # if(!file.exists(paste0(sp_dir, '/sdm.rds'))){
  #   print('been modeled, moving on')
    # run <- "Run20200203"
    # try(
    #   rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/singleSpeciesSummary.rmd"),
    #                     output_file =  paste(species, '_', run,'_' , Sys.Date(), ".html", sep=''),
    #                     output_dir = paste0(gap_dir,"/", genus,"/summaryDocs"))
    # )
  #
  # }else{
  t1a <- Sys.time()
  cat("...developing raw data\n")
  developRaw(species)
  time_df <- rbind(time_df, data.frame(functionUsed="developing raw data",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # rawData <<- listing of all data for the species
  #
    t1a <- Sys.time()
    cat("...generating counts csv\n")
    developCounts(species)
    time_df <- rbind(time_df, data.frame(functionUsed="testLatLong",  runTime=difftime(Sys.time(), t1a, units='secs')))
    ## dataThin <<- raw data with complete lat long and only necessary

    t1a <- Sys.time()
    cat("...conducting SRSex assessment\n")
    srs_exsitu(species)
    time_df <- rbind(time_df, data.frame(functionUsed="srs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))

    t1a <- Sys.time()
    cat("...creating spatial points data frame\n")
    spPoints(species)
    time_df <- rbind(time_df, data.frame(functionUsed="spPoints",  runTime=difftime(Sys.time(), t1a, units='secs')))
    # # spPoint <<- all usable occurence data for NA
    
    if(class(spPoint) == "character"){
      run <- "Run20200203"
      rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/singleSpeciesSummary.rmd"),
                        output_file =  paste(species, '_', run,'_' , Sys.Date(), ".html", sep=''),
                        output_dir = paste0(gap_dir,"/", genus,"/summaryDocs"))
    }else{
      t1a <- Sys.time()
      cat("...adding North American Points to Counts.csv\n")
      try(addNorthAmericanCounts(species))
      time_df <- rbind(time_df, data.frame(functionUsed="addNorthAmericanCounts",  runTime=difftime(Sys.time(), t1a, units='secs')))
      #
      #
      t1a <- Sys.time()
      cat("...srsIn before the modeling process\n")
      srs_insitu_preModel(species)
      time_df <- rbind(time_df, data.frame(functionUsed="srs_insitu_preModel",  runTime=difftime(Sys.time(), t1a, units='secs')))
      
      if(class(spPoint) == 'character' ){
        print("there is not locational data for this species this is the end of the modeling process")
      }else{
        cat("...extracting country values to points and remove duplicate lat long\n")
        countryCheck(species)
        time_df <- rbind(time_df, data.frame(functionUsed="countryCheck",  runTime=difftime(Sys.time(), t1a, units='secs')))
        # xyData <<- coords for elements in spPoint object
        # cleanPoints <<- spatial points layer were all points are on land.
        #   #
        if(class(cleanPoints) == "character"){
          print('no usable records for the modeling method')
        }else{
          t1a <- Sys.time()
          cat("...spatial sample to under 2000 tota\n")
          subSampleByCountry(species)
          time_df <- rbind(time_df, data.frame(functionUsed="sampling",  runTime=difftime(Sys.time(), t1a, units='secs')))
          # cleanPoints <<- if spatail thining is applied the cleanPoint var is redeclared.
          # code needs to be ran to this location inorder to produce htmls
          
            t1a <- Sys.time()
            cat("...generate native area shp\n")
          nat_area_shp(species)
            time_df <- rbind(time_df, data.frame(functionUsed="nat_area_shp",  runTime=difftime(Sys.time(), t1a, units='secs')))
            ##nativeArea <<- ecoregions clipped to countries with species present

                    # t1a <- Sys.time()
                    # cat("...conducting EOO and AOO assessment\n")
                    # eooAoo(species)
                    # time_df <- rbind(time_df, data.frame(functionUsed="eooAoo",  runTime=difftime(Sys.time(), t1a, units='secs')))

                    if(class(nativeArea)== "character"){
                    print("not enough points for modelings")
                      }else{
                      if(nrow(cleanPoints)<= 3){
                      print("not enough points for modelings")
                        }else{

                        t1a <- Sys.time()
                        cat("...generate ga50Raster\n")
                       create_buffers(species)
                        time_df <- rbind(time_df, data.frame(functionUsed="create_buffers",  runTime=difftime(Sys.time(), t1a, units='secs')))

                        t1a <- Sys.time()
                        cat("...generate background and extract raster data to background and presence data\n")
                        generateModelingData(species)
                        time_df <- rbind(time_df, data.frame(functionUsed="generateModelingData",  runTime=difftime(Sys.time(), t1a, units='secs')))
                        # bioValues <<- Presence and background points with predictor data attached

                        t1a <- Sys.time()
                        cat("...perform variable selection and correlation\n")
                        varaibleSelection(species)
                        time_df <- rbind(time_df, data.frame(functionUsed="varaibleSelection",  runTime=difftime(Sys.time(), t1a, units='secs')))
                        # bioValues <<- redefined if any NA are present in predictor datasets.
                        # variblesToModel <<- a listing of variable names used to modeling

                        t1a <- Sys.time()
                        cat("...perform maxent model\n")
                        runMaxnet(species)
                        time_df <- rbind(time_df, data.frame(functionUsed="runMaxnet",  runTime=difftime(Sys.time(), t1a, units='secs')))
                        # sdm_results <<- output of the Maxnet modeling process

          if(!file.exists(paste0(sp_dir, "/sdm.rds"))){
          print("the model did not successfully run")
            }else{

                          t1a <- Sys.time()
                          evaluate_sdm_function(species)
                          time_df <- rbind(time_df, data.frame(functionUsed="evaluate_sdm_function",  runTime=difftime(Sys.time(), t1a, units='secs')))
                          ## thrshold <<- threshold raster

                          if(length(unique(values(thrshold)))==2){
                          print("the model did not successfully run")
                            }else{

                            t1a <- Sys.time()
                            cat("...create a mess map based on top predictor \n")
                            messMap(species)
                            time_df <- rbind(time_df, data.frame(functionUsed="messMap",  runTime=difftime(Sys.time(), t1a, units='secs')))

                            t1a <- Sys.time()
                            cat("...create a kernal density map of the sample points  \n")
                            kernalDensity(species)
                            time_df <- rbind(time_df, data.frame(functionUsed="kernalDensity",  runTime=difftime(Sys.time(), t1a, units='secs')))
          #
                            ### start of the gap analysis metrics.
                            t1a <- Sys.time()
                            cat("...conducting GRSex assessment\n")
                            grs_exsitu(species)
                            time_df <- rbind(time_df, data.frame(functionUsed="grs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))

              t1a <- Sys.time()
              cat("...conducting ERSex assessment\n")
              try(ers_exsitu(species))
              time_df <- rbind(time_df, data.frame(functionUsed="ers_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))

              t1a <- Sys.time()
              cat("...conducting  fcsex assessment\n")
              fcs_exsitu(species)
              time_df <- rbind(time_df, data.frame(functionUsed="fcs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))

                            cat("...Ex situ assessment is complete\n")

          t1a <- Sys.time()
          cat("...conducting srs In assessment\n")
          try(srs_insitu(species))
          time_df <- rbind(time_df, data.frame(functionUsed="srs_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))

          t1a <- Sys.time()
          cat("...conducting grs In assessment\n")
          try(insitu_grs(species))
          time_df <- rbind(time_df, data.frame(functionUsed="insitu_grs",  runTime=difftime(Sys.time(), t1a, units='secs')))

          t1a <- Sys.time()
          cat("...conducting ers In assessment\n")
          try(ers_insitu(species))
          time_df <- rbind(time_df, data.frame(functionUsed="ers_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))

          t1a <- Sys.time()
          cat("...conducting fcs In assessment\n")
          try(fcs_insitu(species))
          time_df <- rbind(time_df, data.frame(functionUsed="fcs_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
                  #
                                cat("...in situ assessment is complete\n")

          t1a <- Sys.time()
          cat("...conducting fcs combined assessment\n")
          fcs_combine(species)
          time_df <- rbind(time_df, data.frame(functionUsed="fcs_combine",  runTime=difftime(Sys.time(), t1a, units='secs')))

          t1a <- Sys.time()
          run <- "Run20200915"
          rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/singleSpeciesSummary.rmd"),
                            output_file =  paste(species, '_', run,'_' , Sys.Date(), ".html", sep=''),
                            output_dir = paste0(gap_dir,"/", genus,"/summaryDocs"))
          
          
          time_df <- rbind(time_df, data.frame(functionUsed="sinlge species Summary",  runTime=difftime(Sys.time(), t1a, units='secs')))
          time_df$Minutes <- time_df$runTime/60
          write.csv(x = time_df, file = paste0(sp_dir, "/time_df.csv"))
          print(paste0("the process for ", species, " has ended."))

          
          # remove all global variables that are specific to the species
          try(globalVars <- c(xyData, cleanPoints, sp_dir,rawData,dataThin, nativeArea, spPoint,pa_spp,
                              ecoValsAllPointsLen,pa_spp_area, gBufferRas1, gBufferRas_area,
                              sp_counts,bioValues,sdm,evaluate_table,sdm_results, thrshold,
                              evaluate_table_f, crossValDir,rastersToModel, data_train, bioValues,
                              variblesToModel,species))
          
          try(rm(globalVars))
          beepr::beep(1)
          
    }
    
    
 
      }
     }
    }
  }
 }
}
   }
# #}




