---
  title: Summary of maxent modeling results and conservation gap analysis results per taxon
author: Colin Khoury, Daniel Carver
output:
  html_document:
  code_folding: hide
highlight: tango
theme: yeti
toc: no
toc_depth: 4
toc_float:
  collapsed: yes
smooth_scroll: yes
---
  
  
  ```{r echo=FALSE, message=FALSE, warning=FALSE}
#install.packages("jpeg")
#install.packages("tiff")
library(knitr)
library(markdown)
library(rmarkdown)
library(tmap)
library(raster)
library(DT)
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
# function for flitering list based on character values
include <- function (theList, toMatch){
  matches <- unique (grep(paste(toMatch,collapse="|"),
                          theList, value=TRUE))
  return(matches)
}
### var for testing Comment out before final save
# taxa <- "Daucus_gracilis"
# run<- "daucus20190426"
#  base_dr <- "base_dir + /gap_analysis"
useCountry <- FALSE
```

## Summary of the models for `r taxa`.

```{r echo=FALSE, message=FALSE, warning=FALSE}
# taxa is the

baseDir <- paste0(base_dr, '/', taxa,'/',run)
csv <- list.files(baseDir, pattern = ".csv", recursive = TRUE , full.names = TRUE)
if (file.exists(paste0(base_dr, '/', taxa,'/counts.csv'))){
  counts <- as.data.frame(read.csv(paste0(base_dr, '/', taxa,'/counts.csv'))) %>%
    dplyr::select(c("totalRecords", "totalUseful", "totalGRecords", "totalGUseful","totalHRecords", "totalHUseful"))
}else{
  print("There are no samples present in Tunisia")
}

if (file.exists(paste0(base_dr, '/', taxa,'/countsTunOnly.csv'))){
  countsTun <- as.data.frame(read.csv(paste0(base_dr, '/', taxa,'/countsTunOnly.csv'))) %>%
    dplyr::select(c("totalRecords", "totalUseful", "totalGRecords", "totalGUseful","totalHRecords", "totalHUseful"))
}else{
  print("There are no samples present in Tunisia")
}


```
<br>
  <br>
  <br>
  
  ### Evaluation metrics
  
  The modeling process was run with 10 replicates to attempt to account for random variation occuring in this type of process. The table below shows the statistical results for the 10 runs.

```{r echo=FALSE, message=FALSE, warning=FALSE}
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  # evalMetric <- include(csvs,"eval_metrics_rep.csv")
  DT::datatable(read.csv(include(csv,"eval_metrics_rep.csv")))
}
```
<br>
  <br>
  <br>
  
  #### Median value
  The median result across the 10 replicates is shown below. Per previous articles we have done, to be considered an accurate and stable model, we are looking for an ATAUC >= 0.7; STAUC < 0.15; cAUC >= 0.4, and ASD15 <= 10

```{r echo=FALSE, message=FALSE, warning=FALSE}
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  # evalMetric <- include(csvs,"eval_metrics_rep.csv")
  DT::datatable(read.csv(include(csv,"eval_metrics.csv")))
}
```
<br>
  <br>
  <br>
  
  ```{r echo=FALSE, message=FALSE, warning=FALSE}

if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  tif <- list.files(path = baseDir, pattern = '.tif', recursive = TRUE, full.names = TRUE)
  if(useCountry == TRUE){
    test1 <- raster(include(tif, "spdist_median")) %>%
      mask(mask = country)
    if(length(unique(getValues(test1)))>1){
      median <- raster(include(tif, "spdist_median")) %>%
        mask(mask = country)
      thrshold <- raster(include(tif, "spdist_thrsld"))%>%
        mask(mask = country)
      sd <- raster(include(tif, "spdist_sd"))%>%
        mask(mask = country)
    }}else{
      median <- raster(include(tif, "spdist_median"))
      thrshold <- raster(include(tif, "spdist_thrsld"))
      sd <- raster(include(tif, "spdist_sd"))
    }
}
tmap_mode("view")


median2 <- raster(include(tif, "spdist_median"))
thrshold2 <- raster(include(tif, "spdist_thrsld"))
sd2 <- raster(include(tif, "spdist_sd"))


#}
```

### Maps of model outputs.


#### Median

Map of the median result from maxent models. Values in the key refer to probability of occurrence.
```{r echo=FALSE, message=FALSE, warning=FALSE}
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  occList <- read.csv(list.files(path = occ_dir,pattern = paste0(taxa,".csv"),full.names = TRUE), header = TRUE)
  spAll = sp::SpatialPointsDataFrame(occList[,c("longitude", "latitude")], data = occList)
  
  
  #qtm(shp = median)
  tm_shape(median2) + tm_raster()+
    tm_shape(spAll) + tm_dots()+
    tm_scale_bar()
}
```

<br>
  
  ```{r echo=FALSE, message=FALSE, warning=FALSE}
# if(length(include(csv,"eval_metrics_rep.csv")) == 0){
#   print("Models were not are to be ran on this species due to insufficent number of samples")
# }else{
#     occList <- read.csv(list.files(path = occ_dir,pattern = paste0(taxa,".csv"),full.names = TRUE), header = TRUE)
#     crs(spAll)<-crs(country)
#     spTun <- crop(spAll, country)
#
# #qtm(shp = median)
# tm_shape(median) + tm_raster()+
#   tm_shape(spTun) + tm_dots()+
#     tm_scale_bar()
# }
```
<br>
  <br>
  <br>
  
  #### Standard Deviation
  Map of the standard deviation result from maxent models
```{r echo=FALSE, message=FALSE, warning=FALSE}
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  qtm(shp = sd2)
}
```

<br>
  
  ```{r echo=FALSE, message=FALSE, warning=FALSE}
# if(length(include(csv,"eval_metrics_rep.csv")) == 0){
#   print("Models were not are to be ran on this species due to insufficent number of samples")
# }else{
# qtm(shp = sd)
# }
```
<br>
  <br>
  <br>
  
  
  #### Threshold
  Map of the threshold (binary presence-absence) result from maxent models. This is the final map we use for the subsequent conservation gap analysis.

```{r echo=FALSE, message=FALSE, warning=FALSE}
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  palette1 <- c(  "#FFFFFF","#45B320")
  tm_shape(thrshold2) + tm_raster(palette=palette1)+
    tm_scale_bar()
}
```

Map of the threshold (binary presence-absence) result from maxent models.

```{r echo=FALSE, message=FALSE, warning=FALSE}
# if(length(include(csv,"eval_metrics_rep.csv")) == 0){
#   print("Models were not are to be ran on this species due to insufficent number of samples")
# }else{
# palette1 <- c(  "#FFFFFF","#45B320")
# tm_shape(thrshold) + tm_raster(palette=palette1)+
#     tm_scale_bar()
# }
```
<br>
  <br>
  <br>
  
  
  ### Conservation gap analysis
  
  
  #### Ex situ Conservation
  
  The table below shows the ex situ conservation summary. SRS is a gross comparison of germplasm (G) and reference (H) records. GRS analyzes how comprehensively the G records cover the maxent model spatially. ERS analyzes how well the G records cover the maxent model with regard to ecosystems covered.
All of the conservation metrics are on a scale from 0-100, with 0 = poor conservation and 100 = perfectly sufficient conservation. The final ex situ conservation score is called FCS and is a mean of the 3 ex situ conservation scores.
```{r echo=FALSE, message=FALSE, warning=FALSE}
# exSummary <- include(csv, "exsitu/summary.csv")
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  tableEx <- dplyr::bind_cols(counts, read.csv(include(csv, "exsitu/summary.csv")))
  tableEx <- tableEx[c(7,1,2,3,4,5,6,8,9,10, 11)]
  DT::datatable(tableEx)
}
```
<br>
  <br>
  <br>
  
  
  ```{r echo=FALSE, message=FALSE, warning=FALSE}
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  # pull in all g points.
  #No idea why this is not working, going back to old indexing
  # %>%   filter(type == "G")
  occList <- occList[which(occList$type == "G"),]
  
  sp = sp::SpatialPoints(occList[,c("longitude", "latitude")])
  gapRaster <- raster(include(tif, "ca50_g_narea_pa"))
  gapRaster2 <- raster(include(tif, "grs_pa_PAs_narea_areakm2"))
  gapRaster2a <- gapRaster2[is.na(gapRaster2)] <- 0
  collgap1 <- thrshold + gapRaster
  GRSin <- thrshold + gapRaster2
}
```

#### GRSex
Map of the potential distribution, with previous germplasm collection points surrounded by a 50 km buffer overlaid. Only germplasm points are displayed on the map.

Areas of Native Range = 0

Potential Distribution = 1

Area Where samples have been collected = 2

```{r echo=FALSE, message=FALSE, warning=FALSE}
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  gPoint <- spAll[spAll@data$type == "G",]
  palette1 <- c(  "#FFFFFF","#45B320","#7570b3")
  if(length(gPoint$X)>0){
    tm_shape(collgap1) + tm_raster(palette=palette1)+
      tm_shape(gPoint) + tm_dots()+
      tm_scale_bar()
  }else{
    palette2 <- c(  "#FFFFFF","#45B320")
    tm_shape(collgap1) + tm_raster(palette=palette2)+
      tm_scale_bar()
  }
}
#
# tm_shape(Europe) +
# tm_fill("gdp_cap_est", title = "GDP", style = "fixed",
#         breaks = c(0, 10000, 20000, 30000, 40000, Inf),
#         textNA = "Dunno",
#         colorNA = "green",   # <-------- color for NA values
#         palette = c("red", "orange", "yellow", "turquoise", "blue", "white")) +
# tm_borders() +
# tm_layout("Wealth (or so)",
#           legend.title.size = 1,
#           legend.text.size = 0.6,
#           legend.position = c("left","bottom"),
#           legend.bg.color = "white",
#           legend.digits = 5,
#           legend.bg.alpha = 1)
#

```

<br>
  <br>
  <br>
  
  
  
  #### In Situ Conservation
  
  The table below shows the in situ conservation summary.  GRS analyzes how comprehensively protected areas (WDPA database 2019) cover the maxent model spatially. ERS analyzes how well protected areas cover the maxent model with regard to ecosystems covered.
All of the conservation metrics are on a scale from 0-100, with 0 = poor conservation and 100 = perfectly sufficient conservation. The final in situ conservation score is called FCS and is a mean of the 2 in situ conservation scores.
```{r echo=FALSE, message=FALSE, warning=FALSE}
# inSummary <- include(csv, "insitu/summary.csv")
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  tableIn <- dplyr::bind_cols(counts, read.csv(include(csv, "insitu/summary.csv")))
  tableIn <- tableIn[c(7,1,2,3,4,5,6,8,9,10)]
  DT::datatable(tableIn)
}
```
<br>
  <br>
  <br>
  
  #### GRSin
  
  Map of the potential distribution, with distribution occurring within existing protected areas (WDPA 2019) highlighted.

Areas of Native Range = 0

Potential Distribution = 1

Protected Lands within the Potential Distribution = 2

```{r echo=FALSE, message=FALSE, warning=FALSE}
if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not are to be ran on this species due to insufficent number of samples")
}else{
  
  tm_shape(GRSin) + tm_raster(palette = palette1)+
    tm_scale_bar()
}

```
<br>
  <br>
  <br>
  
  #### Combined Summary
  This table shows the combined ex situ and in situ conservation metrics. FCSCmean is the final conservation score we have been using. We also categorize these scores (0-25 = high priority (HP) for further conservation work; 25-50 medium (MP); 50-75 low (LP); and 75-100 sufficiently conserved (SC)
                                                                                                                                                                        ```{r echo=FALSE, message=FALSE, warning=FALSE}
                                                                                                                                                                        # inSummary <- include(csv, "insitu/summary.csv")
                                                                                                                                                                        if(length(include(csv,"eval_metrics_rep.csv")) == 0){
                                                                                                                                                                          print("Models were not are to be ran on this species due to insufficent number of samples")
                                                                                                                                                                        }else{
                                                                                                                                                                          tableCo<- dplyr::bind_cols(counts, read.csv(include(csv, "combined/fcs_combined.csv")))
                                                                                                                                                                          tableCo <- tableCo[c(7,1,2,3,4,5,6, 8,9,10,11,12,13,14,15)]
                                                                                                                                                                          DT::datatable(tableCo)
                                                                                                                                                                        }
                                                                                                                                                                        ```
                                                                                                                                                                        <br>
                                                                                                                                                                          <br>
                                                                                                                                                                          <br>
                                                                                                                                                                          