###
# add the number of points that occur in modeling area to counts CSV and rewrite 
# 
#
###


addNorthAmericanCounts <- function(species){
  if(!file.exists(paste0(sp_dir,"/counts.csv"))){
    print("there are no spatial points available for this model")
  }else{
    if(class(spPoint) == "character"){
      df <- read.csv(paste0(sp_dir,"/counts.csv")) %>%
        mutate(NorthAmericanPoint = 0)
    }else{
      df <- read.csv(paste0(sp_dir,"/counts.csv")) %>%
        dplyr::mutate(NorthAmericanPoint = nrow(spPoint))
    }

  }
  write.csv(x = df, file = paste0(sp_dir,"/counts.csv"), row.names = FALSE)
  dfCounts <<- rbind(dfCounts, df)
}