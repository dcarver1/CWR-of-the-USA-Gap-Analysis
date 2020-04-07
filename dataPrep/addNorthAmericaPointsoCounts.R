###
# add the number of points that occur in modeling area to counts CSV and rewrite 
# 
#
###


addNorthAmericanCounts <- function(species){
      df1 <- spPoint@data %>%
        dplyr::group_by(type) %>%
        dplyr::summarise(count = n())
      if(nrow(df1) == 2){
        gs <- df1 %>% filter(type=="G")
        gs <- gs$count[1]
        hs <- df1 %>% filter(type=="H")
        hs <- hs$count[1]
      }else{
        if(df1$type == "G"){
          gs <- df1 %>% filter(type=="G")
          gs <- gs$count[1]
          hs <- 0
        }else{
          gs <- 0
          hs <- df1 %>% filter(type=="H")
          hs <- hs$count[1]
        }
      }
      
      df <- read.csv(paste0(sp_dir,"/counts.csv")) 
      
      df$NA_occurrences <- nrow(spPoint)
      df$NA_GUseful <- gs
      df$NA_HUseful <- hs
  write.csv(x = df, file = paste0(sp_dir,"/counts.csv"), row.names = FALSE)
  dfCounts <<- rbind(dfCounts, df)
  print("done")
}

