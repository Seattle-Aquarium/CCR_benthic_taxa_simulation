## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Graphing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
rm(list = ls())

library(tidyverse)
library(reshape2)

## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## data prep: filtering function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## function to filter down to the site level
filter.site <- function(df, select.site, taxa.list){
  
  df <- filter(df, site %in% select.site)
  df <- subset(df, select = taxa.list)
  df <- melt(df, id.vars="SU")
  return(df)
}

sites <- filter.site(dat, c("1"), c("red_algae", "sugar_kelp", "green_algae", "soft_sediment", 
                                    "small_rocks", "boulder_bedrock", "remainder", "SU"))


## function to filter down to the transect level
filter.transect <- function(df, select.site, select.transect, taxa.list){
  
  df <- filter(df, site %in% select.site)
  df <- filter(df, key %in% select.transect)
  df <- subset(df, select = taxa.list)
  df <- melt(df, id.vars="SU")  
  
  return(df)
}

transects <- filter.transect(dat, c("1"), c("1_1"), c("red_algae", "sugar_kelp", "green_algae", "soft_sediment", 
                                                      "small_rocks", "boulder_bedrock", "remainder", "SU"))
## END data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## graphical parameter functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## set themes
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(size=15),
                 axis.text.x = element_text(size=15),
                 axis.title.y = element_text(size=15),
                 axis.text.y = element_text(size=15))


legend.theme <- theme(#legend.position=c(1,1),
  #legend.justification=c(.98,.98), 
  legend.text=element_text(size=15), 
  legend.title = element_text(size=15),
  #legend.title.align = 0.5, 
  #legend.key.height = unit(.2, "cm"),
  legend.key=element_rect(fill = FALSE, color=FALSE), 
  legend.background=element_rect(fill=FALSE, color=FALSE))


## set custom color pallet 
pal <- c("#ab2221", "#be5e00", "#b3a100", "#4d6934", "#3386a2", "#803280", "#000000")
my.cols <- scale_color_manual(values=pal)


## function to close out graphing windows and set custom pane
my.window <- function(width, height){
  graphics.off()
  windows(width, height, record=TRUE)
}
## END graphing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## create plotting functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## function to plot 1 category across all sites 
plot.all.sites <- function(data, cat, alpha, x.lab, y.lab){
  p1 <- ggplot(data, aes(SU, cat)) +
    geom_point(alpha=alpha) + my.theme + xlab(x.lab) + ylab(y.lab)
  print(p1)
  return(p1)
}

p1 <- plot.all.sites(dat, sugar_kelp, 0.75, "Simulated images", "Sugar kelp percent-cover per image")




# Everything on the same plot
p1 <- ggplot(transects, aes(SU, value, col=variable)) + 
  my.theme + geom_point(size=1.75) + my.cols + legend.theme +
  xlab("Simulated images") + ylab("Percent-cover points")

print(p1)






plot.transect <- function(pt.size, x.lab, y.lab){
  p1 <- ggplot(transects, aes(SU, value, col=variable)) + 
    my.theme + geom_point(size = pt.size) + my.cols + legend.theme +
    xlab(x.lab) + ylab(y.lab)
  
  print(p1)
  return(p1)
  
  setwd(figs)
  save.pdf 
  save.png
}

print(p1)


p1 <- plot.transect(1.75, "Simulated images", "Percent-cover points")









