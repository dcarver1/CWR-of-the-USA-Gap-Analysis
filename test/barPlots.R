###
#20200325
# script to generate bar chart summarises 
# dan.carver@carverd@com
###
library(plotly)
# read in metrics data 
d1 <- read.csv(file = "D:/cwrNA/parameters/barCharts/allMetricData2020-03-20.csv")

#function for ploting
plot2<- function(df){
  fig <- plot_ly(data = df, x = ~HP, y = ~category,
                 type = 'bar', orientation = 'h', name = "HP", 
                 marker = list(color = 'rgba(204, 44, 0, 0.6)'
                 ))%>% 
    add_trace(x = ~MP, name = 'MP',
              marker = list(color = 'rgba(255, 145, 0, 0.6)'
              ))%>% 
    add_trace(x = ~LP, name = 'LP',
              marker = list(color = 'rgba(255, 234, 0, 0.6)'
              ))%>% 
    layout(barmode = 'stack',
           xaxis = list(title = "Proportion of taxa (%)"),
           yaxis = list(title =""))
  return(fig)
}

#set wd 
setwd("D:/cwrNA/parameters/barCharts")

# this is not prefect at the moment but it is working.. run the process for 



### generated the data structure I needed to test the plot method 
d2 <- read.csv(file = "D:/cwrNA/parameters/barCharts/testchart.csv")

c1 <- factor(c("FCSin","FCSex","FCSc-mean"), levels = c("FCSin","FCSex","FCSc-mean"))


pg <- plot_ly(x = rev(d2$HP), y = c1,
              type = 'bar',  name = "HP",
              marker = list(color = 'rgba(204, 44, 0, 0.6)'
              ))%>% 
  add_trace(x = rev(d2$MP), name = 'MP',
            marker = list(color = 'rgba(255, 145, 0, 0.6)'
            ))%>% 
  add_trace(x = rev(d2$LP), name = 'LP',
            marker = list(color = 'rgba(255, 234, 0, 0.6)'
            ))%>% 
  add_trace(x = rev(d2$SC), name = 'SC',
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
    dplyr::summarise(count =n()) %>%
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
uc <- sort(unique(d1$Associated.crop))
priorityGroup <- data.frame(matrix(ncol = 4, nrow = length(uc)))
colnames(priorityGroup) <- c("category","HP", "MP","LP")
for(i in 1:length(uc)){
  p <- d1 %>%
    dplyr::filter(Associated.crop == uc[i])%>%
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
write.csv(x = priorityGroup, file = "associateCrop.csv")


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
priorityGroup
htmlwidgets::saveWidget(pg,file = "associateCrop.html")

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
