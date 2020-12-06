###
#20200325
# script to generate bar chart summarises 
# dan.carver@carverd@com
###
library(plotly)
# read in metrics data 
d1 <- read.csv("D:/cwrNA/runSummaries/allMetricData2020-04-12forFigures.csv")
dim(d1)
d1 <- d1[d1$Included.in.summary.metrics == "Y",]
dim(d1)

#set wd 
setwd("D:/cwrNA/parameters/barCharts")

# this is not prefect at the moment but it is working.. run the process for 



### generated the data structure I needed to test the plot method 
#d2 <- read.csv(file = "D:/cwrNA/parameters/barCharts/testchart.csv")

total <- length(d1$FCSc.mean.priority.category[!is.na(d1$FCSc.mean.priority.category)])
tA <- d1 %>% 
  group_by(FCSc.mean.priority.category) %>%
  dplyr::summarise(count =n())%>%
  dplyr::mutate(per=paste0(round(100*count/total,2)))
tA <- tA[c(1,3,2),]
tA
tI <- d1 %>% 
  group_by(FCSin.priority.category) %>%
  dplyr::summarise(count =n())%>%
  drop_na()%>% ### 20200411 added clause to drop na values. 
  dplyr::mutate(per=paste0(round(100*count/total,2)))
tI <- tI[c(1,3,2,4),]
tE <- d1 %>% 
  group_by(FCSex.priority.category) %>%
  dplyr::summarise(count =n())%>%
  drop_na()%>% ### 20200411 added clause to drop na values. 
  dplyr::mutate(per=paste0(round(100*count/total,2)))
tE <- tE[c(1,3,2,4),]

df2 <- data.frame(matrix(nrow = 3, ncol = 5))
colnames(df2) <- c("category" ,	"HP",	"MP",	"LP",	"SC")
df2$category <- as.factor(c("FCSc-mean","FCSex","FCSin" ))
df2[1,2:4] <- as.numeric(tA$per)
df2[1,5] <- as.numeric(0)
df2[2,2:5] <- as.numeric(tE$per)
df2[3,2:5] <- as.numeric(tI$per)

c1 <- factor(c("FCSin","FCSex","FCSc-mean"), levels = c("FCSin","FCSex","FCSc-mean"))


pg <- plot_ly(x = rev(df2$HP), y = c1,orientation = 'h',
              type = 'bar',  name = "HP",
              marker = list(color = 'rgba(204, 44, 0, 0.6)'
              ))%>% 
  add_trace(x = rev(df2$MP), name = 'MP',
            marker = list(color = 'rgba(255, 145, 0, 0.6)'
            ))%>% 
  add_trace(x = rev(df2$LP), name = 'LP',
            marker = list(color = 'rgba(255, 234, 0, 0.6)'
            ))%>% 
  add_trace(x = rev(df2$SC), name = 'SC',
            marker = list(color = 'rgba(3, 204, 0, 0.6)'
            ))%>% 
  layout(barmode = 'stack',
         xaxis = list(title = "Proportion of taxa (%)"),
         yaxis = list(title =""))
pg
htmlwidgets::saveWidget(pg,file = "totalInsituExsitu.html")

# priority group 
uc <- sort(unique(d1$Category))
priorityGroup <- data.frame(matrix(ncol = 4, nrow = length(uc)))
colnames(priorityGroup) <- c("category","HP", "MP","LP")
for(i in 1:length(uc)){
  p <- d1 %>%
    dplyr::filter(Category == uc[i])%>%
    dplyr::group_by(`FCSc.mean.priority.category`)%>%
    dplyr::summarise(count =n())%>%
    drop_na()%>% ### 20200411 added clause to drop na values. 
    dplyr::mutate(countT= sum(count)) %>%
    dplyr::mutate(per=paste0(round(100*count/countT,2)))
  
  p1 <- p[,c(1,4)] %>% tidyr::spread(FCSc.mean.priority.category,per) %>%
    dplyr::select(HP,MP,LP)
  priorityGroup$category[i] <- as.character(paste0(uc[i], "  "))
  priorityGroup$HP[i] <- as.numeric(p1$HP)
  priorityGroup$MP[i] <- as.numeric(p1$MP)
  priorityGroup$LP[i] <- as.numeric(p1$LP)
}
write.csv(x = priorityGroup, file = "priorityGroup.csv")

c1 <- factor(c("1C","1B","1A"), levels = c("1C","1B","1A"))

pg <- plot_ly(x = rev(priorityGroup$HP), y = c1,
              type = 'bar',  name = "HP",
              marker = list(color = 'rgba(204, 44, 0, 0.6)'
              ))%>% 
  add_trace(x = rev(priorityGroup$MP), name = 'MP',
            marker = list(color = 'rgba(255, 145, 0, 0.6)'
            ))%>% 
  add_trace(x = rev(priorityGroup$LP), name = 'LP',
            marker = list(color = 'rgba(255, 234, 0, 0.6)'
            ))%>%
  layout(barmode = 'stack',
         xaxis = list(title = "Proportion of taxa (%)"),
         yaxis = list(title =""))
pg 
htmlwidgets::saveWidget(pg,file = "priorityGroup.html")

# associated crop type 
uc <- sort(unique(d1$Associated.crop.type.specific))
priorityGroup <- data.frame(matrix(ncol = 4, nrow = length(uc)))
colnames(priorityGroup) <- c("category","HP", "MP","LP")
for(i in 1:length(uc)){
  p <- d1 %>%
    dplyr::filter(Associated.crop.type.specific == uc[i])%>%
    dplyr::group_by(`FCSc.mean.priority.category`)%>%
    dplyr::summarise(count =n()) %>%
    drop_na()%>% ### 20200411 added clause to drop na values. 
    dplyr::mutate(countT= sum(count)) %>%
    dplyr::mutate(per=paste0(round(100*count/countT,2)))
  p1 <- p[,c(1,4)] 
  if(nrow(p1) ==3){
    p1 <- p1 %>% tidyr::spread(FCSc.mean.priority.category,per) %>%
      dplyr::select(HP,MP,LP)
    priorityGroup$category[i] <- as.character(paste0(uc[i], "  "))
    priorityGroup$HP[i] <- as.numeric(p1$HP)
    priorityGroup$MP[i] <- as.numeric(p1$MP)
    priorityGroup$LP[i] <- as.numeric(p1$LP)
  }else{
    vals <- unique(p1$FCSc.mean.priority.category)
    priorityGroup$category[i] <- as.character(paste0(uc[i], "  "))
    p1 <- p1 %>% tidyr::spread(FCSc.mean.priority.category,per) %>%
      dplyr::select(vals)
    if("HP" %in% vals){
      priorityGroup$HP[i] <- as.numeric(p1$HP)
    }else{
      priorityGroup$HP[i] <- 0 
    }
    if("MP" %in% vals){
      priorityGroup$MP[i] <- as.numeric(p1$MP)
    }else{
      priorityGroup$MP[i] <- 0 
    }
    if("LP" %in% vals){
      priorityGroup$LP[i] <- as.numeric(p1$LP)
    }else{
      priorityGroup$LP[i] <- 0 
    }
  }
}
write.csv(x = priorityGroup, file = "associateCropType.csv")


uc1 <- rev(as.character(uc))


c1 <- factor(uc1, levels = uc1)

pg <- plot_ly(x = rev(priorityGroup$HP), y = c1,
              type = 'bar',  name = "HP",
              marker = list(color = 'rgba(204, 44, 0, 0.6)'
              ))%>% 
  add_trace(x = rev(priorityGroup$MP), name = 'MP',
            marker = list(color = 'rgba(255, 145, 0, 0.6)'
            ))%>% 
  add_trace(x = rev(priorityGroup$LP), name = 'LP',
            marker = list(color = 'rgba(255, 234, 0, 0.6)'
            ))%>%
  layout(barmode = 'stack',
         xaxis = list(title = "Proportion of taxa (%)"),
         yaxis = list(title =""))
pg
htmlwidgets::saveWidget(pg,file = "associateCropType.html")


# assocaited crop 
uc <- sort(unique(d1$Associated.crop.common.name))
priorityGroup <- data.frame(matrix(ncol = 4, nrow = length(uc)))
colnames(priorityGroup) <- c("category","HP", "MP","LP")
for(i in 1:length(uc)){
  p <- d1 %>%
    dplyr::filter(Associated.crop.common.name == uc[i])%>%
    dplyr::group_by(`FCSc.mean.priority.category`)%>%
    dplyr::summarise(count =n()) %>%
    drop_na()%>% ### 20200411 added clause to drop na values. 
    dplyr::mutate(countT= sum(count)) %>%
    dplyr::mutate(per=paste0(round(100*count/countT,2)))
  p1 <- p[,c(1,4)] 
  if(nrow(p1) ==3){
    p1 <- p1 %>% tidyr::spread(FCSc.mean.priority.category,per) %>%
      dplyr::select(HP,MP,LP)
    priorityGroup$category[i] <- as.character(paste0(uc[i], "  "))
    priorityGroup$HP[i] <- as.numeric(p1$HP)
    priorityGroup$MP[i] <- as.numeric(p1$MP)
    priorityGroup$LP[i] <- as.numeric(p1$LP)
  }else{
    vals <- unique(p1$FCSc.mean.priority.category)
    priorityGroup$category[i] <- as.character(paste0(uc[i], "  "))
    p1 <- p1 %>% tidyr::spread(FCSc.mean.priority.category,per) %>%
      dplyr::select(vals)
    if("HP" %in% vals){
      priorityGroup$HP[i] <- as.numeric(p1$HP)
    }else{
      priorityGroup$HP[i] <- 0 
    }
    if("MP" %in% vals){
      priorityGroup$MP[i] <- as.numeric(p1$MP)
    }else{
      priorityGroup$MP[i] <- 0 
    }
    if("LP" %in% vals){
      priorityGroup$LP[i] <- as.numeric(p1$LP)
    }else{
      priorityGroup$LP[i] <- 0 
    }
  }
}
write.csv(x = priorityGroup, file = "Associated.crop.common.name.csv")


uc1 <- rev(as.character(uc))


c1 <- factor(uc1, levels = uc1)

pg <- plot_ly(x = rev(priorityGroup$HP), y = c1,
              type = 'bar',  name = "HP",
              marker = list(color = 'rgba(204, 44, 0, 0.6)'
              ))%>% 
  add_trace(x = rev(priorityGroup$MP), name = 'MP',
            marker = list(color = 'rgba(255, 145, 0, 0.6)'
            ))%>% 
  add_trace(x = rev(priorityGroup$LP), name = 'LP',
            marker = list(color = 'rgba(255, 234, 0, 0.6)'
            ))%>%
  layout(barmode = 'stack',
         xaxis = list(title = "Proportion of taxa (%)"),
         yaxis = list(title =""))

pg
htmlwidgets::saveWidget(pg,file = "Associated.crop.common.name.html")

# redlist groups 

uc <- rev(c("Least Concern (LC)",
        "Possible Near Threatened (NT)",
        "Vulnerable (VU)",
        "Endangered (EN)",
        "Critically Endangered (CR)"))
priorityGroup <- data.frame(matrix(ncol = 4, nrow = length(uc)))
colnames(priorityGroup) <- c("category","HP", "MP","LP")

for(i in 1:length(uc)){
  p <- d1 %>%
    dplyr::filter(Combined.threat.assessment.status == uc[i])%>%
    dplyr::group_by(`FCSc.mean.priority.category`)%>%
    dplyr::summarise(count =n()) %>%
    dplyr::mutate(countT= sum(count)) %>%
    dplyr::mutate(per=paste0(round(100*count/countT,2)))
  p1 <- p[,c(1,4)] 
  if(nrow(p1) ==3){
    p1 <- p1 %>% tidyr::spread(FCSc.mean.priority.category,per) %>%
      dplyr::select(HP,MP,LP)
    priorityGroup$category[i] <- as.character(paste0(uc[i], "  "))
    priorityGroup$HP[i] <- as.numeric(p1$HP)
    priorityGroup$MP[i] <- as.numeric(p1$MP)
    priorityGroup$LP[i] <- as.numeric(p1$LP)
  }else{
    vals <- unique(p1$FCSc.mean.priority.category)
    priorityGroup$category[i] <- as.character(paste0(uc[i], "  "))
    p1 <- p1 %>% tidyr::spread(FCSc.mean.priority.category,per) %>%
      dplyr::select(vals)
    if("HP" %in% vals){
      priorityGroup$HP[i] <- as.numeric(p1$HP)
    }else{
      priorityGroup$HP[i] <- 0 
    }
    if("MP" %in% vals){
      priorityGroup$MP[i] <- as.numeric(p1$MP)
    }else{
      priorityGroup$MP[i] <- 0 
    }
    if("LP" %in% vals){
      priorityGroup$LP[i] <- as.numeric(p1$LP)
    }else{
      priorityGroup$LP[i] <- 0 
    }
  }
}
write.csv(x = priorityGroup, file = "redlistGroups.csv")



uc1 <- c("Least Concern (LC) ",
             "Possible Near Threatened (NT) ",
             "Vulnerable (VU) ",
             "Endangered (EN) ",
             "Critically Endangered (CR) ")


c1 <- factor(uc1, levels = uc1)

pg <- plot_ly(x = rev(priorityGroup$HP), y = c1,
              type = 'bar',  name = "HP",
              marker = list(color = 'rgba(204, 44, 0, 0.6)'
              ))%>% 
  add_trace(x = rev(priorityGroup$MP), name = 'MP',
            marker = list(color = 'rgba(255, 145, 0, 0.6)'
            ))%>% 
  add_trace(x = rev(priorityGroup$LP), name = 'LP',
            marker = list(color = 'rgba(255, 234, 0, 0.6)'
            ))%>%
  layout(barmode = 'stack',
         xaxis = list(title = "Proportion of taxa (%)"),
         yaxis = list(title =""))

pg 
htmlwidgets::saveWidget(pg,file = "redlistGroups.html")




# ### older function did not work due to the ordering of factors issue 
# #function for ploting
# plot2<- function(df){
#   fig <- plot_ly(data = df, x = ~HP, y = ~category,
#                  type = 'bar', orientation = 'h', name = "HP", 
#                  marker = list(color = 'rgba(204, 44, 0, 0.6)'
#                  ))%>% 
#     add_trace(x = ~MP, name = 'MP',
#               marker = list(color = 'rgba(255, 145, 0, 0.6)'
#               ))%>% 
#     add_trace(x = ~LP, name = 'LP',
#               marker = list(color = 'rgba(255, 234, 0, 0.6)'
#               ))%>% 
#     layout(barmode = 'stack',
#            xaxis = list(title = "Proportion of taxa (%)"),
#            yaxis = list(title =""))
#   return(fig)
# }
