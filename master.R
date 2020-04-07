# This function runs the entire process for a selected species
# @param (chr) species: species ID
# @return (dir): status of run


#here for troubleshooting.
#species <- species1

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
  spp_dirs <- create_sp_dirs(species)
  time_df <- rbind(time_df, data.frame(functionUsed="create_sp_dirs",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # sp_dir <<- primary directory for this run
  
  # if(file.exists(paste0(sp_dir, '/sdm.rds'))){
  #   print('been modeled, moving on')
  # }else{
  t1a <- Sys.time()
  cat("...developing raw data\n")
  spp_raw <- developRaw(species)
  time_df <- rbind(time_df, data.frame(functionUsed="developing raw data",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # rawData <<- listing of all data for the species

  t1a <- Sys.time()
  cat("...generating counts csv\n")
  genCounts1 <- try(counts1(species))
  time_df <- rbind(time_df, data.frame(functionUsed="testLatLong",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # dataThin <<- raw data with complete lat long and only necessary

#     t1a <- Sys.time()
#     cat("...conducting SRSex assessment\n")
#     srsExsitu <- srs_exsitu(species)
#     time_df <- rbind(time_df, data.frame(functionUsed="srs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
# # 
    t1a <- Sys.time()
    cat("...creating spdataframe\n")
    spp_point <- try(spPoints(species))
    time_df <- rbind(time_df, data.frame(functionUsed="spPoints",  runTime=difftime(Sys.time(), t1a, units='secs')))
    #spPoint <<- all usable occurence data for NA
# 
#     # if(class(spPoint) == 'character' ){
#     #   print("there is not locational data for this species this is the end of the modeling process")
#     #   #lowOccurence <<- append(lowOccurence, species)
#     #   c1 <- read.csv(paste0(sp_dir,"/counts.csv"))
#     #   dfCounts <- rbind(dfCounts, c1)
#     # }else{
#       t1a <- Sys.time()
#     cat("...adding North American Points to Counts.csv\n")
      naPoints <- try(addNorthAmericanCounts(species))
      time_df <- rbind(time_df, data.frame(functionUsed="addNorthAmericanCounts",  runTime=difftime(Sys.time(), t1a, units='secs')))
      # spPoint <<- all usable occurence data for NA
      dfCounts <- dfCounts[complete.cases(dfCounts),]
      testNA <-dfCounts[dfCounts$species== species,]
# 
# 
#        if((testNA$NA_occurrences == 0) ){
#         print("no proints are in NA, ending the modeling process")
#        # lowOccurence <- append(lowOccurence, species)
#       }else{
#         t1a <- Sys.time()
        cat("...extracting country values to points and remove duplicate lat long\n")
        landPoints <- try(countryCheck(species))
        time_df <- rbind(time_df, data.frame(functionUsed="countryCheck",  runTime=difftime(Sys.time(), t1a, units='secs')))
       # xyData <<- coords for elements in spPoint object
      #  cleanPoints <<- spatial points layer were all points are on land.

#         ### 20200303 - adding initial srsIn metric here. If the species is modeling this result
#         # will be overwritten
#         t1a <- Sys.time()
#         cat("...srsIn before the modeling process\n")
#         srsin_Pre <-try(srs_insitu_preModel(species))
#         time_df <- rbind(time_df, data.frame(functionUsed="srs_insitu_preModel",  runTime=difftime(Sys.time(), t1a, units='secs')))
# 
# 
#         if(class(cleanPoints) == "character"){
#           print(cleanPoints)
#         }else{
#           if(file.exists(paste0(sp_dir, '/sdm.rds'))){
#             print('been modeled, moving on')
#           #}else{
        t1a <- Sys.time()
        cat("...spatial sample to under 2000 tota\n")
        subSample <- try(sampling(species))
        time_df <- rbind(time_df, data.frame(functionUsed="sampling",  runTime=difftime(Sys.time(), t1a, units='secs')))
        ##cleanPoints <<- if spatail thining is applied the cleanPoint var is redeclared.
        ## code needs to be ran to this location inorder to produce htmls
        # 
  #           ### 20200129 - moving the redlist process to before the modeling effort as we can still
  #           ### run red list even if the models failed. Could possible do this with the spPoint object
  #           ### rather then the cleanPoints but I'd rather keep it here.
            t1a <- Sys.time()
            cat("...conducting EOO and AOO assessment\n")
            redlist <- try(eooAoo(species))
            time_df <- rbind(time_df, data.frame(functionUsed="eooAoo",  runTime=difftime(Sys.time(), t1a, units='secs')))
  #

  #           t1a <- Sys.time()
  #           cat("...generate native area shp\n")
  #           natArea <- try(nat_area_shp(species))
  #           time_df <- rbind(time_df, data.frame(functionUsed="nat_area_shp",  runTime=difftime(Sys.time(), t1a, units='secs')))
  #           ##ecoVal <<- df of all eco_id present in the model. used in ERSex
  #           ##ecoAreas <<- ecoregions that intersect with cleanPoints
  #           ##nativeArea <<- ecoregions clipped to countries with species present
  # 
  #           if(class(natArea)== "character"){
  #             print("not enough points for modelings")
  #             # lowOccurence <<- append(lowOccurence, species)
  #           }else{
  # 
  #             # run up to here in order to create single species summary html for already completed run.
  #             if(nrow(cleanPoints)<= 3){
  #               print("not enough points for modelings")
  #               #lowOccurence <<- append(lowOccurence, species)
  #             }else{
  # 
  #               t1a <- Sys.time()
  #               cat("...generate ga50Raster\n")
  #               createBuffer <- try(create_buffers(species))
  #               time_df <- rbind(time_df, data.frame(functionUsed="create_buffers",  runTime=difftime(Sys.time(), t1a, units='secs')))
  #               ###
  #               #update counts base on thinning process
  #               t1a <- Sys.time()
  #               cat("...update the counts csv to reflect modeled data \n")
  #               updateCount <- try(updateCounts(species))
  #               time_df <- rbind(time_df, data.frame(functionUsed="updateCounts",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # 
  #               t1a <- Sys.time()
  #               cat("...generate background and extract raster data to background and presence data\n")
  #               genModelData <- try(generateModelingData(species))
  #               time_df <- rbind(time_df, data.frame(functionUsed="generateModelingData",  runTime=difftime(Sys.time(), t1a, units='secs')))
  #               # bioValues <<- Presence and background points with predictor data attached
  # 
  #               t1a <- Sys.time()
  #               cat("...perform variable selection and correlation\n")
  #               variableSelect <- try(varaibleSelection(species))
  #               time_df <- rbind(time_df, data.frame(functionUsed="varaibleSelection",  runTime=difftime(Sys.time(), t1a, units='secs')))
  #               # bioValues <<- redefined if any NA are present in predictor datasets.
  #               # variblesToModel <<- a listing of variable names used to modeling
  # 
  #               t1a <- Sys.time()
  #               cat("...perform maxent model\n")
  #               run_Maxnet <- try(runMaxnet(species))
  #               time_df <- rbind(time_df, data.frame(functionUsed="runMaxnet",  runTime=difftime(Sys.time(), t1a, units='secs')))
  #               # sdm_results <<- output of the Maxnet modeling process
  # 
  #               if(!file.exists(paste0(sp_dir, "/sdm.rds"))){
  #                 print("the model did not successfully run")
  #                 #notModeled <<- append(notModeled,species)
  #               }else{
  # 
  #                 t1a <- Sys.time()
  #                 #this code produces the threshold raster
  #                 test1 <- evaluate_sdm_function(species)
  #                 time_df <- rbind(time_df, data.frame(functionUsed="evaluate_sdm_function",  runTime=difftime(Sys.time(), t1a, units='secs')))
  #                 ## thrshold <<- threshold raster
  # 
  #                 #so i'm having models with no threshold presence values... which is odd, But I'm putting a bandaid on now to push it as
  #                 # a failed model
  #                 if(length(unique(values(thrshold)))==2){
  #                   print("the model did not successfully run")
  #                   #notModeled <<- append(notModeled,species)
  #                 }else{
  # 
  #                   t1a <- Sys.time()
  #                   cat("...create a mess map based on top predictor \n")
  #                   messMap(species)
  #                   time_df <- rbind(time_df, data.frame(functionUsed="messMap",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # 
  #                   t1a <- Sys.time()
  #                   cat("...create a kernal density map of the sample points  \n")
  #                   try(kernalDensity(species))
  #                   time_df <- rbind(time_df, data.frame(functionUsed="kernalDensity",  runTime=difftime(Sys.time(), t1a, units='secs')))
  # 
  #                   t1a <- Sys.time()
  #                   cat("...evaluating the  model\n")
  #                   evaluateFunction <- evaluate_function(species)
  #                   time_df <- rbind(time_df, data.frame(functionUsed="evaluate_function",  runTime=difftime(Sys.time(), t1a, units='secs')))
# 
#             ### start of the gap analysis metrics. 
#                     t1a <- Sys.time()
#                     cat("...conducting GRSex assessment\n")
#                     grsExsitu <- try(grs_exsitu(species))
#                     time_df <- rbind(time_df, data.frame(functionUsed="grs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
# 
#                     t1a <- Sys.time()
#                     cat("...conducting ERSex assessment\n")
#                     ersExsitu <- try(ers_exsitu(species))
#                     time_df <- rbind(time_df, data.frame(functionUsed="ers_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
# 
#                     t1a <- Sys.time()
#                     cat("...conducting  fcsex assessment\n")
#                     fcsExsitu <- try(fcs_exsitu(species))
#                     time_df <- rbind(time_df, data.frame(functionUsed="fcs_exsitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
#                     
#                     cat("...Ex situ assessment is complete\n")
#                     #
#                     t1a <- Sys.time()
#                     cat("...conducting srs In assessment\n")
#                     srsInsitu <- try(srs_insitu(species))
#                     time_df <- rbind(time_df, data.frame(functionUsed="srs_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
# 
#                     t1a <- Sys.time()
#                     cat("...conducting grs In assessment\n")
#                     grsInsitu <- try(insitu_grs(species))
#                     time_df <- rbind(time_df, data.frame(functionUsed="insitu_grs",  runTime=difftime(Sys.time(), t1a, units='secs')))
# 
# 
#                     ### remove try from the three functions below
# 
# 
#                     t1a <- Sys.time()
#                     cat("...conducting ers In assessment\n")
#                     ersInsitu <- try(ers_insitu(species))
#                     time_df <- rbind(time_df, data.frame(functionUsed="ers_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
# 
#                     t1a <- Sys.time()
#                     cat("...conducting fcs In assessment\n")
#                     fcssInsitu <- try(fcs_insitu(species))
#                     time_df <- rbind(time_df, data.frame(functionUsed="fcs_insitu",  runTime=difftime(Sys.time(), t1a, units='secs')))
# 
#                     cat("...in situ assessment is complete\n")
#                     
#                     t1a <- Sys.time()
#                     cat("...conducting fcs combined assessment\n")
#                     fcsCombine <- try(fcs_combine(species))
#                     time_df <- rbind(time_df, data.frame(functionUsed="fcs_combine",  runTime=difftime(Sys.time(), t1a, units='secs')))
#                     
#                     #} #temp value for species that did not model to by pass ERS
#                     t1a <- Sys.time()
#                     # try(rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/singleSpeciesSummary.rmd"),
#                     #                       output_file =  paste("report_", species, '_', run_version,'_' , Sys.Date(), ".html", sep=''),
#                     #                       output_dir = paste0(gap_dir,"/", genus,"/summaryDocs")))
#                     # 
#                     time_df <- rbind(time_df, data.frame(functionUsed="sinlge species Summary",  runTime=difftime(Sys.time(), t1a, units='secs')))
#                     time_df$Minutes <- time_df$runTime/60
#                     write.csv(x = time_df, file = paste0(sp_dir, "/time_df.csv"))
#                     print(paste0("the process for ", species, " has ended."))
#                     
#                     #fullModelProcess <<- append(fullModelProcess, species)
#                     
                    rm(cleanPoints, spPoint, sp_dir, rawData, DataThin, species)
#                     
#                     # remove all global variables that are specific to the species
#                     try(globalVars <- c(xyData, cleanPoints, sp_dir,rawData,dataThin,
#                                         nativeArea, spPoint,pa_spp,ecoValsAllPointsLen,pa_spp_area,
#                                         gBufferRas1, gBufferRas_area,sp_counts,bioValues,sdm,
#                                         evaluate_table,sdm_results, thrshold,evaluate_table_f, crossValDir,
#                                         rastersToModel, data_train, bioValues,
#                                         variblesToModel,species
#                     ))
#                     try(rm(globalVars))
#                     beepr::beep(1)
#               }
#     }
#  }
 }
#}
#        }
#      }
#   }# this is connected to if statement about RDS file
# }



