###
# generates the median threshold raster. Calls evaluateFunction to access the
# statistics on the median threshold model.
# Based on work by the CIAT group
# dan.carver@carverd.com
# 20200414
###

evaluate_sdm_function <- function(species){

    if(file.exists(paste0(sp_dir,"/","sdm.rds"))){

      cat("Loading sdm results!", "\n")

      sdm <<- readRDS(paste0(sp_dir,"/","sdm.rds"))
      # Extracting metrics for 5 replicates
      cat("Gathering replicate metrics  for: ", species, "\n")
      evaluate_table <<- metrics_function(species)
      #evaluate_table<-read.csv(paste0(sp_dir,"/","eval_metrics_rep.csv"),header=T)

      # Apply threshold from evaluation
      cat("Thresholding using Max metrics  for: ", species, "\n")
      # thrsld <- as.numeric(mean(evaluate_table[,"Threshold"],na.rm=T))
      thrsld <- as.numeric(mean(evaluate_table[,"threshold_train"],na.rm=T))
      if (!file.exists(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))) {
        # spThrsld <- spMedian
        spThrsld <- raster(paste0(sp_dir,"/modeling/",species,"_prj_median.tif"))
        spThrsld[which(spThrsld[] >= thrsld)] <- 1
        spThrsld[which(spThrsld[] < thrsld)] <- 0
        raster::writeRaster(x = spThrsld, filename = paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"),overwrite = TRUE)
          } else {
        spThrsld <- raster(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
      }
      thrshold<<- spThrsld
      if(nrow(cleanPoints)>=3){
        # Gathering final evaluation table
        evaluate_table_f <<- evaluate_function(species)
        #return(cat("Process finished successfully for specie:", species, "\n"))

      } else { #if(base::nrow(cleanPoints)<10 & base::nrow(cleanPoints)>0  ) {
        cat("Species:", species, "only has", nrow(cleanPoints), "coordinates, it is not appropriate for modeling\n")

        evaluate_table_f <- data.frame(ATAUC=NA,AUCtest=NA,nAUC=NA,cAUC=NA,sensi_train=NA,speci_train=NA,threshold_train=NA,
                                       max.TSS_train=NA,minROCdist_train=NA,threshold_test=NA,sensi_test=NA,speci_test=NA,matthews.cor_test=NA,
                                       LR_pos_test=NA,LR_neg_test=NA,kappa_index_test=NA,species=NA,STAUC=NA,ASD15=NA,VALID=NA)

        evaluate_table_f[,"VALID"] <- FALSE
        evaluate_table_f[,"species"] <- species
        write.csv(evaluate_table_f, paste0(sp_dir,"/","eval_metrics.csv"),row.names=F,quote=F)

      }
    }else {   cat(paste(species," not modelled yet"),"\n")

      if(nrow(cleanPoints)<3){

        cat("Species:", species, "only has", nrow(cleanPoints), "coordinates, it is not appropriate for modeling\n")

        evaluate_table_f <- data.frame(ATAUC=NA,AUCtest=NA,nAUC=NA,cAUC=NA,sensi_train=NA,speci_train=NA,threshold_train=NA,
                                       max.TSS_train=NA,minROCdist_train=NA,threshold_test=NA,sensi_test=NA,speci_test=NA,matthews.cor_test=NA,
                                       LR_pos_test=NA,LR_neg_test=NA,kappa_index_test=NA,species=NA,STAUC=NA,ASD15=NA,VALID=NA)

        evaluate_table_f[,"VALID"] <- FALSE
        evaluate_table_f[,"species"] <- species
        write.csv(evaluate_table_f, paste0(sp_dir,"/","eval_metrics.csv"),row.names=F,quote=F)
      } else {
        cat(paste(species," not modelled yet"),"\n")
      }
    }

  return(species)
}
