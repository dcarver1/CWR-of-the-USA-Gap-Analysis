

march <- data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-03-18.csv", header = TRUE)
dim(march)
april <- data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-04-07.csv")
april <- april[,2:ncol(april)]

jan <- data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-01-11.csv",
                         header = TRUE)


lM <- march %>%
  dplyr::filter(taxon == "Pseudoroegneria spicata")
View(lM)
lA <- april %>%
  dplyr::filter(taxon == "Leymus californicus")
View(lA)

lJ <- jan %>%
  dplyr::filter(taxon == "Pseudoroegneria spicata")
View(lJ)
