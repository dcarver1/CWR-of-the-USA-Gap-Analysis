###
# compiles individual model run data
# built on content from CIAT group
# dan.carver@carver.com
# 20200414
###

metrics_function<-function(species){

  evaluate_table <- data.frame(
    AUCtrain = do.call(rbind,sdm$AUC_train),
    AUCtest = do.call(rbind,sdm$AUC),
    nAUC = do.call(rbind,sdm$nAUC),
    cAUC = do.call(rbind,sdm$cAUC)
  )
  atrain <- do.call(rbind,sdm$evaluation_train)
  colnames(atrain) <- paste0(colnames(atrain),"_","train")

  atest <- do.call(rbind,sdm$evaluation_test)
  colnames(atest) <- paste0(colnames(atest),"_","test")

  evaluate_table <- cbind(evaluate_table,atrain)
  evaluate_table <- cbind(evaluate_table,atest)

  crossValDir <<- paste0(sp_dir, "/modeling/maxent")
  write.csv(evaluate_table,paste0(crossValDir,"/","eval_metrics_rep.csv"),quote = F,row.names = F)

  return(evaluate_table)
}
