###
# runs statistics on the predictive capability of the median model
# Based on work by the CIAT group
# dan.carver@carverd.com
# 20200414
###

evaluate_function <- function(species){

  ###ASD15
  esdCpt <- raster(paste0(sp_dir, "/modeling/",species,"_prj_std.tif"))
  dumm <- raster(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))

  esdCpt[which(dumm[] < 0.001)] <- NA

  #create 0,1 raster with areas below 0.15 STD (below=1, above=0)
  esdCpt_ref <- esdCpt
  esdCpt_ref[which(!is.na(esdCpt[]))] <- 1

  #create 0,1 raster with areas above 0.15 STD (below=0, above=1)
  esdCpt_a15 <- esdCpt
  esdCpt_a15[which(esdCpt[] >= 0.15)] <- 1
  esdCpt_a15[which(esdCpt[] < 0.15)] <- 0

  #make a raster of area
  dist_area <- area(esdCpt)

  #calculate size of distribution within native area, and within thresholded distribution
  #total, and above 0.15 STD.
  szCpt <- dist_area * esdCpt_ref
  szCptUncertain <- dist_area * esdCpt_a15
  rateCpt <- sum(szCptUncertain[],na.rm=T) / sum(szCpt[],na.rm=T) * 100

  #############################
  rm(dumm,esdCpt);gc()
  #############################

  evaluate_table_f <- data.frame(matrix(nrow = 1,ncol = 16))
  evaluate_table_f[1,] <- colMeans(evaluate_table[,-10],na.rm = T)
  colnames(evaluate_table_f) <- names(colMeans(evaluate_table[,-10],na.rm = T))
  #SP
  evaluate_table_f[,"species"]<- as.character(species)
  evaluate_table_f[,"STAUC"]<- as.numeric(sd(evaluate_table$AUCtrain,na.rm = T))
  colnames(evaluate_table_f)[1] <- "ATAUC"

  ####ASD15
  evaluate_table_f$ASD15 <- NA
  if(is.na(rateCpt)){
    evaluate_table_f$ASD15 <- 100
  } else {
    evaluate_table_f$ASD15 <- rateCpt
  }

  if (evaluate_table_f$ATAUC >=0.7 &
      evaluate_table_f$STAUC <0.15 &
      evaluate_table_f$ASD15 <=10 &
      evaluate_table_f$cAUC >=0.4
  ) {
    evaluate_table_f$VALID  <-TRUE
  } else {
    evaluate_table_f$VALID  <-FALSE
  }

  #write output table
  write.csv(evaluate_table_f,paste0(crossValDir,"/","eval_metrics.csv"),quote = F,row.names = F)
}
