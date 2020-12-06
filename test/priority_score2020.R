# H. Achicanoy
# CIAT, 2019

library(tidyverse)
library(grid)
library(readxl)

root <- 'D:/cwrNA/parameters/priorityFigure'
df   <- read.csv("D:/cwrNA/runSummaries/allMetricData2020-04-12forFigures.csv")
cats <- c('Category','Associated.crop.type.specific','Associated.crop.common.name')
dim(df)
df <- df[df$Included.in.summary.metrics == "Y",]
dim(df)
# Category graph
tbl <- df %>%
  dplyr::select('Taxon','Category','FCSc.mean')
tbl %>%
  ggplot2::ggplot(aes(x = factor(Category,levels=c("(1)C","(1)B","(1)A")),
                      y = `FCSc.mean`, group = factor(Category,levels=c("(1)C","(1)B","(1)A"))))+
  ggplot2::theme_bw() +
  ggplot2::geom_blank() +
  ggplot2::coord_flip() +
  ggplot2::ylim(0, 100) +
  ggplot2::annotate("rect", ymin=0, ymax=25, xmin=0.5, xmax=3.5, alpha=.25, fill="red") +
  ggplot2::annotate("rect", ymin=25, ymax=50, xmin=0.5, xmax=3.5, alpha=.5, fill="orange") +
  ggplot2::annotate("rect", ymin=50, ymax=75, xmin=0.5, xmax=3.5, alpha=.4, fill="yellow") +
  ggplot2::annotate("rect", ymin=75, ymax=100, xmin=0.5, xmax=3.5, alpha=.3, fill="forestgreen") +
  #ggplot2::geom_point() +
  ggplot2::geom_jitter(width = 0.30)+
  ggplot2::geom_point(data = tbl %>%
                        dplyr::group_by(Category) %>%
                        dplyr::summarise(`FCSc.mean` = mean(`FCSc.mean`, na.rm=TRUE)), size = 5, colour = 'red') +
  ggplot2::ylab(label="Final conservation score (FCSc-mean)") + ggplot2::xlab("") +
  ggplot2::scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
                 axis.text.x  = element_text(size=15),
                 axis.text.y  = element_text(face="plain",size=15),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15)) +
  ggplot2::annotate("text",
                    y        = c(12.5, 37.5, 62.5, 87.5),
                    x        = 3.4,
                    label    = c("HP","MP","LP","SC"),
                    colour   = "black",
                    size     = 4,
                    fontface = 2) +
  ggsave(paste0(root,"/category.png"), device = "png", units = "in", width = 10, height = 6, dpi = 320)

# Associated.crop.type.specific
tbl <- df %>%
  dplyr::select('Taxon','Associated.crop.type.specific','FCSc.mean')
colnames(tbl)[2] <- 'Category'
lbls <- (tbl$Category %>% unique)[tbl$Category %>% unique %>% order(decreasing = T)]
tbl %>%
  ggplot2::ggplot(aes(x = factor(Category,levels=lbls), y = `FCSc.mean`, group = factor(Category,levels=lbls))) +
  ggplot2::theme_bw() +
  ggplot2::geom_blank() +
  ggplot2::coord_flip() +
  ggplot2::ylim(0, 100) +
  ggplot2::annotate("rect", ymin=0, ymax=25, xmin=0.5, xmax=14, alpha=.25, fill="red") +
  ggplot2::annotate("rect", ymin=25, ymax=50, xmin=0.5, xmax=14, alpha=.5, fill="orange") +
  ggplot2::annotate("rect", ymin=50, ymax=75, xmin=0.5, xmax=14, alpha=.4, fill="yellow") +
  ggplot2::annotate("rect", ymin=75, ymax=100, xmin=0.5, xmax=14, alpha=.3, fill="forestgreen") +
  #ggplot2::geom_point() +
  ggplot2::geom_jitter(width = 0.25)+
  ggplot2::geom_point(data = tbl %>%
                        dplyr::group_by(Category) %>%
                        dplyr::summarise(`FCSc.mean` = mean(`FCSc.mean`, na.rm=TRUE)), size = 5, colour = 'red') +
  ggplot2::ylab(label="Final conservation score (FCSc-mean)") + ggplot2::xlab("") +
  ggplot2::scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
                 axis.text.x  = element_text(size=15),
                 axis.text.y  = element_text(face="plain",size=15),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15)) +
  ggplot2::annotate("text",
                    y        = c(12.5, 37.5, 62.5, 87.5),
                    x        = 13.6,
                    label    = c("HP","MP","LP","SC"),
                    colour   = "black",
                    size     = 4,
                    fontface = 2) +
  ggsave(paste0(root,"/associated_crop_type_specific.png"), device = "png", units = "in", width = 10, height = 6, dpi = 320)

# Associated.crop.common.name
tbl <- df %>%
  dplyr::select('Taxon','Associated.crop.common.name','FCSc.mean')
colnames(tbl)[2] <- 'Category'
lbls <- (tbl$Category %>% unique)[tbl$Category %>% unique %>% order(decreasing = T)]
tbl %>%
  ggplot2::ggplot(aes(x = factor(Category,levels=lbls), y = `FCSc.mean`, group = factor(Category,levels=lbls))) +
  ggplot2::theme_bw() +
  ggplot2::geom_blank() +
  ggplot2::coord_flip() +
  ggplot2::ylim(0, 100) +
  ggplot2::annotate("rect", ymin=0, ymax=25, xmin=0, xmax=53, alpha=.25, fill="red") +
  ggplot2::annotate("rect", ymin=25, ymax=50, xmin=0, xmax=53, alpha=.5, fill="orange") +
  ggplot2::annotate("rect", ymin=50, ymax=75, xmin=0, xmax=53, alpha=.4, fill="yellow") +
  ggplot2::annotate("rect", ymin=75, ymax=100, xmin=0, xmax=53, alpha=.3, fill="forestgreen") +
  #ggplot2::geom_point() +
  ggplot2::geom_jitter(width = 0.25)+
  ggplot2::geom_point(data = tbl %>%
                        dplyr::group_by(Category) %>%
                        dplyr::summarise(`FCSc.mean` = mean(`FCSc.mean`, na.rm=TRUE)), size = 5, colour = 'red') +
  ggplot2::ylab(label="Final conservation score (FCSc-mean)") + ggplot2::xlab("") +
  ggplot2::scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
                 axis.text.x  = element_text(size=15),
                 axis.text.y  = element_text(face="plain",size=15),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15)) +
  ggplot2::annotate("text",
                    y        = c(12.5, 37.5, 62.5, 87.5),
                    x        = 52,
                    label    = c("HP","MP","LP","SC"),
                    colour   = "black",
                    size     = 4,
                    fontface = 2) +
  ggsave(paste0(root,"/associated_crop.png"), device = "png", units = "in", width = 10, height = 12, dpi = 320)



