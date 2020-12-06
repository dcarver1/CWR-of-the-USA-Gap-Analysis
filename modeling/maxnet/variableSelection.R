###
# preform a variable selection on the data generates input dataset for modeling
# methods
# 20190904
# dan.carver@carverd.com
###

varaibleSelection <- function(species){
  # subset predictor data and presence column
  varSelect <- bioValues %>% dplyr::select(-c(longitude,latitude ))
  # remove all na from dataframe
  test2 <-complete.cases(varSelect)
  varSelect <- varSelect[test2,]
  # drop all column from bioValues set as well so the same data is used for maxnet modeling.
  bioValues <<- bioValues[test2,]
  write.csv(x = bioValues, file = paste0(sp_dir, "/modeling/maxent/bioValuesForPresencePoints.csv"))


  # # #vsurf
  ### Considered altering the number of trees, 100 is somewhat low for the
  # number of predictors used. It was a time concern more then anything.
  # vsurfThres <- VSURF_thres(x=bioValues[,1:26] , y=as.factor(bioValues$presence) ,
  #                           ntree = 100 )
  ### change for 30 arc second run 
  vsurfThres <- VSURF_thres(x=bioValues[,1:20] , y=as.factor(bioValues$presence) ,
                            ntree = 100 )
  ###
  #correlation matrix
  ###

  # define predictor list based on Run
  inputPredictors <- vsurfThres$varselect.thres

  # ordered predictors from our variable selection
  predictors <- varSelect[,c(inputPredictors)]
  # Calculate correlation coefficient matrix
  correlation <-cor(predictors, method="pearson")
  #change self correlation value

  # #define the list of top 15 predictors
  varNames <- colnames(correlation)
  # empty list containing the variables tested
  varsTested <- c()
  #loop through the top 5 predictors to remove correlated varables.
  for( i in 1:5){
    print(varNames[i])
    if(varNames[i] %in% varNames){
      # add variable to the test list
      varsTested <- c(varsTested, varNames[i])
      # Test for correlations with predictors
      vars <- correlation[(i+1):nrow(correlation),i] > 0.7 | correlation[(i+1):nrow(correlation),i] < -0.7
      # Select correlated values names
      corVar <- names(which(vars == TRUE))
      #test is any correlated variables exist
      if(length(corVar) >0 ){
        # loop through the list of correlated variables
        varNames <- varNames[!varNames  %in% corVar]
        print(paste0("the variable ", corVar, " was removed"))
      }
    }else{
      print("this variable has been removed already")
    }
  }

  # include all variables that were tested.
  for(p in varsTested){
    if(p %in% varNames){
    }else{
      varNames <- c(varNames, p)
    }
  }# It's a little bit confusing why variable are being dropped after they area tested. Correlation
  # should be the same in both directs. This is just a test to make sure it works.


  #create a dataframe of the top predictors and
  rankPredictors <- data.frame(matrix(nrow = length(colnames(correlation)),ncol = 3))
  rankPredictors$varNames <- colnames(correlation)
  rankPredictors$importance <- vsurfThres$imp.varselect.thres
  rankPredictors$includeInFinal <- colnames(correlation) %in% varNames
  rankPredictors <- rankPredictors[,4:6]
  write.csv(x = rankPredictors, file = paste0(sp_dir, "/modeling/maxent/predictorImportance.csv"))

  variblesToModel <<- varSelect[,varNames]
}
