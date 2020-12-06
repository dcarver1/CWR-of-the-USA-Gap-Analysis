###
# use the redListR library to calculate the EOO and AOO assessments for the species
# dan.carver@carverd.com
# 20200414
###

eooAoo <- function(species){
  # clause for low occurrence species
  if(class(spPoint)== "character" | nrow(cleanPoints@data) <= 2){
    df <- data.frame(matrix(data = NA, nrow = 1, ncol = 7))
    colnames(df) <- c("taxon", "EOO Area km2","EOO Status", "AOO",
                      "AOO adjusted Minimum", "AOO Status", "Combined Status")

    df$taxon <- species
    df$`EOO Area km2` <- NA
    df$`EOO Status`<- "Critically Endangered (CR)"
    df$AOO <- NA
    df$`AOO adjusted Minimum` <- NA
    df$`AOO Status` <- "Critically Endangered (CR)"
    df$`Combined Status` <- "Critically Endangered (CR)"
    write.csv(x = df, file = paste0(sp_dir, '/gap_analysis/redList/listingValues4kmClean.csv'))
  }else{
    if(class(cleanPoints) != "SpatialPointsDataFrame"){
      cleanPoints <- spPoint
    }
# 
#     if(file.exists(paste0(sp_dir, "/gap_analysis/redList/listingValues.csv"))){
#       print('completed, moving on')
#      }else{
      wgs84 <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      worldEqualArea <- crs("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")
      cleanPoints2 <- SpatialPoints(coords = cleanPoints@coords, proj4string = wgs84)
      spAllPro <- spTransform(cleanPoints2, worldEqualArea)
      EOO.polygon <- makeEOO(spAllPro)
      # then calcualte the area of the bounding box
      EOO.area <- getAreaEOO(EOO.polygon)
      # determine status based on area
      if (EOO.area >= 45000){
        blo <- "Least Concern (LC)"
        eVal <- 1 }
      if (EOO.area < 45000){
        blo <- "Possible Near Threatened (NT)"
      eVal <- 2 }
      if (EOO.area < 20000){
        blo <- "Vulnerable (VU)"
        eVal <- 3} # 20000
      if (EOO.area < 5000){
        blo <- "Endangered (EN)"
        eVal <- 4} # 5000
      if (EOO.area < 100)
        {blo <- "Critically Endangered (CR)"
        eVal <- 5} # 100
      if (EOO.area == "NA"){blo <- "Critically Endangered (CR)"
      eVal <- 6}

      #EOO.area
      # this value is then use in the develop of other criteria in the sebcriterion B1

      ### Subcriterion B2 (calculating AOO)
      # create a 4 x 4 grid of to overlay on distribution.

      AOO.grid <- makeAOOGrid(spAllPro, grid.size = 4000,
                              min.percent.rule = F)
      n.AOO <- length(AOO.grid)
      AOOarea <- n.AOO * 4
      # determine status based on area
      if (AOOarea >= 4500){
        AOO_cat <- "Least Concern (LC)"
      aVal <- 1 } # <
      if (AOOarea < 4500){
        AOO_cat <- "Possible Near Threatened (NT)"
        aVal <- 2 }
      if (AOOarea < 2000){AOO_cat <- "Vulnerable (VU)"
      aVal <- 3 } # < 2000
      if (AOOarea < 500){AOO_cat <- "Endangered (EN)"
      aVal <- 4 }# < 500
      if (AOOarea < 10){AOO_cat  <- "Critically Endangered (CR)"
      aVal <- 5 }# < 10
      if (AOOarea == "NA"){AOO_cat <- "Critically Endangered (CR)"
      aVal <- 6 }


      #n.AOO
      # so the length is just the number of grid cells that overlay this environment
      # because the position of the grid cells can potential change the number of cells
      # a randomized process is used to determine a minimun number of grids.

      # 20200414, the n.AOO.improvement is set to one as a time saving measure.
      # we do not use the grid uncertainty measure in the catergorization of
      # the species, so timeliness was most important here.
      gU.results <- gridUncertainty(spAllPro, 4000,
                                    n.AOO.improvement = 1,
                                    min.percent.rule = F)

      # dataframe to save items
      df <- data.frame(matrix(data = NA, nrow = 1, ncol = 7))
      colnames(df) <- c("taxon", "EOO Area km2","EOO Status", "AOO",
                        "AOO adjusted Minimum", "AOO Status", "Combined Status")
      df$taxon <- species
      df$`EOO Area km2` <- EOO.area
      df$`EOO Status`<- blo
      df$AOO <- n.AOO * 4
      df$`AOO adjusted Minimum` <- gU.results$min.AOO.grid$AOO.number * 4
      df$`AOO Status` <- AOO_cat

      # the names for combined status
      status <- c("Least Concern (LC)","Possible Near Threatened (NT)",
                  "Vulnerable (VU)", "Endangered (EN)"
                  ,"Critically Endangered (CR)","Critically Endangered (CR)")
      # Select the lowest status and use that to define the overall status
      if(eVal >= aVal){
        stat <- status[eVal]
      }else{
        stat <- status[aVal]
      }

      df$`Combined Status` <- stat
      write.csv(x = df, file = paste0(sp_dir, '/gap_analysis/redList/listingValues4kmClean.csv'))
    #}
  }
}
