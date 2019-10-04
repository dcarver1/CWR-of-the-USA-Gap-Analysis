###
# build a test dataset for trouble shooting to increase processing times
# 20190829
# carver.dan1@gmail.com
###

t3 <- read.csv(file = "D:/cwrOfNA/modelingData2019-08-30.csv")

test1 <- dplyr::sample_n(tbl = t3, size = 8000 )

write.csv(x = test1,file = "D:/cwrOfNA/parameters/testData.csv", row.names = FALSE)
