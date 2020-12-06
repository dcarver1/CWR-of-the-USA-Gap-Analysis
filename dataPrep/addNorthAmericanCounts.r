###
# add the number of points that occur in modeling area to counts CSV and rewrite
# dan.carver@carverd.com
# 20200414
###

addNorthAmericanCounts <- function(species){
      # read in data filtered to north america
      df1 <- spPoint@data %>%
        dplyr::group_by(type) %>%
        dplyr::summarise(count = n())
      # if both g and h occurrences are present
      if(nrow(df1) == 2){
        gs <- df1 %>% filter(type=="G")
        gs <- gs$count[1]
        hs <- df1 %>% filter(type=="H")
        hs <- hs$count[1]
      }else{
        # if only one or the other occurrences types area present
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
      # read in existing counts data
      df <- read.csv(paste0(sp_dir,"/counts.csv"))
      # assign values
      df$NA_occurrences <- nrow(spPoint)
      df$NA_GUseful <- gs
      df$NA_HUseful <- hs
  # write out content.
  write.csv(x = df, file = paste0(sp_dir,"/counts.csv"), row.names = FALSE)
}
