###
# run  maxnet, include validations and projections to rasters 
# based on work by CIAT group 
# 20190904
# carver.dan1@gmail.com
###

#species <- species1

# run maxnet multiple times 

runMaxnet <- function(species){
    tryCatch(
      # This is what I want to do...
      {
        # pull out presence points 
        nPresence <- bioValues %>%
          filter(presence == 1) %>% nrow()
        
        if(nPresence <= 20){
          "not enough presence points to run a model"
          # might want to impliment this again at some point but this maxnet process seems a bit more sensitive to lower numbers
          # this is something to trouble shoot later. 
          # }else{
          #   if(nPresence <=10){
          #     kfold <- 3
          #     feat <- "l"
        }else{
          kfold <- 10 
          feat <- "lap"
          
          
          # select needed raster bands 
          rastersToModel <<- bioVars$as.RasterStack() %>%
            raster::subset(names(variblesToModel))%>%
            raster::mask(nativeArea)
          
          ###
          # for testing 
          #kfold <- 3
          bioValuesModel <- bioValues[complete.cases(bioValues),]
          
          # run model within here 
          cvfolds <- modelr::crossv_kfold(bioValuesModel, k= kfold)
          
          
          
          
          sdm_results <<- cvfolds %>% dplyr::mutate(.
                                                    #train 5 sdm models using Maxnet and train data
                                                    ,model_train = purrr::map2(.x = train, .y = .id, function(.x, .y){
                                                      
                                                      cat("Training MAXNET model for fold", .y, ", all presence points added to background \n")
                                                      
                                                      data_train <- as.data.frame(.x) 
                                                      
                                                      p <- data_train$presence
                                                      # cat(print(p),"\n")
                                                      data <- data_train %>% dplyr::select(names(variblesToModel))
                                                      
                                                      fit.maxent <- maxnet::maxnet(p       = p,
                                                                                   data    = data,
                                                                                   #regmult = beta,
                                                                                   f       = maxnet.formula(p, data, classes = feat))
                                                      
                                                      return(fit.maxent)
                                                      
                                                    })
                                                    
                                                    #evaluate trained model
                                                    , predictions_train = purrr::pmap(list(.x = model_train, .y = .id, .z = train), function(.x, .y, .z){
                                                      cat("Predicting train data for fold", .y, "\n")
                                                      train <- as.data.frame(.z)
                                                      predictions <- raster::predict(object = .x, newdata = train[, -1], type = "logistic")
                                                      dt <-  data.frame(obs = factor(train[, 1]), pred = predictions)
                                                      
                                                      return(dt)
                                                    })
                                                    #calculate auc for trained model
                                                    ,AUC_train = purrr::map2(.x = predictions_train, .y = .id, function(.x, .y){
                                                      cat("Calculating AUC_train for model", .y,"\n")
                                                      croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                                                      
                                                      return(as.numeric(croc$auc))
                                                    } )
                                                    #calculate max preformance measures (sensitivity, specificity and Treshold) using train data
                                                    ,evaluation_train = purrr::map2(.x = predictions_train, .y = .id, function(.x, .y){
                                                      
                                                      cat("Calculating optimal threshold for model", .y, "\n")
                                                      croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                                                      croc_summ <- data.frame (sensi = croc$sensitivities, speci = croc$specificities, threshold =  croc$thresholds) %>% 
                                                        round(., 3) %>% 
                                                        dplyr::mutate(., max.TSS = sensi + speci - 1) %>% 
                                                        dplyr::mutate(., minROCdist = sqrt((1- sensi)^2 + (speci -1)^2))
                                                      
                                                      max.tss <- croc_summ %>% dplyr::filter(., max.TSS == max(max.TSS)) %>% 
                                                        dplyr::mutate(., method = rep("max(TSS)", nrow(.)))
                                                      
                                                      minRoc <- croc_summ %>% 
                                                        dplyr::filter(., minROCdist == min(minROCdist))%>% 
                                                        dplyr::mutate(., method = rep("minROCdist", nrow(.)))
                                                      
                                                      croc_summ <- rbind(max.tss, minRoc) %>% 
                                                        dplyr::filter(., speci == max(speci))  %>% 
                                                        dplyr::sample_n(., 1)
                                                      
                                                      return(croc_summ)
                                                    }) 
                                                    #Make predictions using testing data
                                                    , predictions_test = purrr::pmap(list(.x = test, .y = model_train, .z = .id), function(.x, .y, .z){
                                                      
                                                      cat("Using test data to predict model", .z," \n")
                                                      test <- as.data.frame(.x)
                                                      predictions <- raster::predict(object = .y,newdata = test[, -1],type = "logistic")
                                                      dt <-  data.frame(obs = factor(test[, 1]), pred = predictions)
                                                      
                                                      return(dt )
                                                    })
                                                    #Calculate AUC for testing 
                                                    , AUC = map2(.x = predictions_test, .y = .id, function(.x, .y){
                                                      cat("Calculating AUC for model", .y,"\n")
                                                      croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                                                      
                                                      return(as.numeric(croc$auc))
                                                    } )
                                                    #calculate max preformance measures (sensitivity, specificity and Treshold) using max(TSS) criterion
                                                    , evaluation_test = pmap(list(.x = evaluation_train, .y = .id, .z = predictions_test), function(.x, .y, .z){
                                                      
                                                      thr <- .x$threshold
                                                      
                                                      a <- .z %>% dplyr::filter(., pred >= thr & obs == 1) %>% nrow()
                                                      b <- .z %>% dplyr::filter(., pred >= thr & obs == 0) %>% nrow()
                                                      c <- .z %>% dplyr::filter(., pred < thr & obs == 1) %>% nrow()
                                                      d <- .z %>% dplyr::filter(., pred < thr & obs == 0) %>% nrow()
                                                      
                                                      #senitivity and specificity
                                                      se <- a/(a+c)
                                                      es <- d/(b+d)
                                                      #Matthews correlation coefficient
                                                      den <- sqrt(a+b)*sqrt(a+c)*sqrt(d+b)*sqrt(d+c)
                                                      den <- ifelse(den  != 0 ,den, 1 )
                                                      mcc <- (a*d - b*c)/den
                                                      #Likelyhood Ratio +
                                                      lr_ps <- se/(1 - es)
                                                      #Likelihood ratio -
                                                      lr_ne <- (1 - se)/es
                                                      
                                                      #calculate kappa index
                                                      pr_a <- (a+d)/(a+b+c+d)
                                                      pr_e <- (((a+b)/(a+b+c+d))* ((a+c)/(a+b+c+d))) + ( ((c+d)/(a+b+c+d) )* ((b+d)/(a+b+c+d) )) 
                                                      kappa <- (pr_a - pr_e)/(1 - pr_e) 
                                                      
                                                      
                                                      evaluation <- data.frame(threshold= thr, sensi = se, speci = es, matthews.cor = mcc, LR_pos = lr_ps, LR_neg = lr_ne, kappa_index = kappa)
                                                      
                                                      return(evaluation)
                                                      # cat("Calculating optimal threshold for model", .y, "\n")
                                                      # croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                                                      # croc_summ <- data.frame (sensi = croc$sensitivities, speci = croc$specificities, threshold =  croc$thresholds) %>% 
                                                      #   round(., 3) %>% 
                                                      #   dplyr::mutate(., max.TSS = sensi + speci - 1) %>% 
                                                      #   dplyr::mutate(., minROCdist = sqrt((1- sensi)^2 + (speci -1)^2))
                                                      # 
                                                      # max.tss <- croc_summ %>% dplyr::filter(., max.TSS == max(max.TSS)) %>% 
                                                      #   dplyr::mutate(., method = rep("max(TSS)", nrow(.)))
                                                      # 
                                                      # minRoc <- croc_summ %>% 
                                                      #   dplyr::filter(., minROCdist == min(minROCdist))%>% 
                                                      #   dplyr::mutate(., method = rep("minROCdist", nrow(.)))
                                                      # 
                                                      # croc_summ <- rbind(max.tss, minRoc) %>% 
                                                      #   dplyr::filter(., speci == max(speci))  %>% 
                                                      #   dplyr::sample_n(., 1)
                                                      # 
                                                      # return(croc_summ)
                                                    })
                                                    #Calculate nAUC using both train and test data
                                                    , nAUC = pmap(list(.x = train, .y = test, .z = .id), function(.x, .y, .z){
                                                      cat("calculating AUC from NULL model", .z,"\n")
                                                      # train_dt <- as.data.frame(.x) %>% dplyr::select(., occName, starts_with("lon"), starts_with("lat")  )
                                                      # test_dt  <- as.data.frame(.y) %>% dplyr::select(., occName, starts_with("lon"), starts_with("lat")  )
                                                      
                                                      train_dt <- as.data.frame(.x) %>% dplyr::select(., presence, longitude, latitude )
                                                      test_dt  <- as.data.frame(.y) %>% dplyr::select(., presence, longitude, latitude)
                                                      
                                                      
                                                      train_p <- train_dt[which(train_dt$presence == 1), 2:3]
                                                      train_a <- train_dt[which(train_dt$presence == 0), 2:3]
                                                      
                                                      gd <- dismo::geoDist(p = train_p, a = train_a, lonlat=TRUE)
                                                      pred <- dismo::predict(gd, test_dt %>% dplyr::select(longitude, latitude))
                                                      
                                                      nAUC <- pROC::roc(response = test_dt$presence, predictor = pred)
                                                      return(as.numeric(nAUC$auc))
                                                    })
                                                    #Calculate cAUC using the formula cAUC = AUC + 0.5 - max( 0.5, nAUC)
                                                    , cAUC = purrr::pmap(list(.x = AUC, .y = nAUC, .z = .id), function(.x, .y, .z){
                                                      cat("Calculating AUC correction using NULL model", .z, " \n")
                                                      cAUC = .x + 0.5 - max( 0.5, .y)
                                                      return(cAUC)
                                                    })
                                                    #Project rasters using maxnet model for mean, median and sd
                                                    , do.projections =  purrr::pmap(list(.x = model_train, .y = .id, .z = evaluation_train) ,function(.x, .y, .z){
                                                      
                                                      cat(">>> Proyecting MAXNET model", .y,"to a raster object \n")
                                                      r <- raster::predict(rastersToModel, .x, type = "logistic", progress='text')
                                                      writeRaster(r, paste0(sp_dir,"/modeling/replicates/",species,"_prj_rep-", .y,".tif"), format="GTiff", overwrite = TRUE)
                                                      #thresholding raster 
                                                      # if(!validation){
                                                      #   r[which(r[] < .z$threshold)] <- NA 
                                                      # }
                                                      writeRaster(r, paste(sp_dir,"/modeling/replicates/",species,"_prj_th_rep-", .y,".tif",sep=""), format="GTiff", overwrite = TRUE)
                                                      return(r)
                                                    })
                                                    
                                                    
                                                    
          )#end mutate
          
          
          #calculate  mean, median and sd raster from replicates 
          prj_stk <- sdm_results %>% dplyr::select(., do.projections) %>% unlist() %>% raster::stack() %>% raster::mask(nativeArea)
          cat("Calculating mean, median and sd for replicates \n")
          mean(prj_stk) %>% writeRaster(., paste0(sp_dir,"/modeling/", species, "_prj_mean.tif" ), overwrite = TRUE)
          cat("Mean raster calculated \n")
          raster::calc(prj_stk, fun = function(x) {median(x)}) %>% writeRaster(., paste0(sp_dir,"/modeling/", species, "_prj_median.tif" ), overwrite = TRUE)
          cat("Median raster calculated \n")
          raster::calc(prj_stk, fun = function(x) {sd(x)}) %>% writeRaster(., paste0(sp_dir,"/modeling/", species, "_prj_std.tif" ), overwrite = TRUE)
          cat("Sd raster calculated \n")
          
          
          #save all results in an .rds file
          cat("Process Done... Saving results as .rds file in the path", paste0(sp_dir, "/sdm.rds"), " \n")
          saveRDS(sdm_results, paste0(sp_dir, "/sdm.rds"));gc()
          
          cat(" ","\n")
          cat("Maxent model finished and saved","\n")
          cat(" ","\n")
          return(sdm_results)
      }
        },
      # ... but if an error occurs, tell me what happened: 
      error=function(error_message) {
        message("This species encountered an error it will be added to a list to evalualte later")
        message("And below is the error message from R:")
        message(error_message)
        return(NA)
      }
    )
  
  
}
#end sdm_maxnet function 

