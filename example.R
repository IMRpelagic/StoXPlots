
###############################################################################
#Startup to clean upp any mess
###############################################################################
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


###############################################################################
#load some packages
###############################################################################
library(ggplot2)
library(rgeos)
library("rnaturalearth")
library("rnaturalearthdata")
library(latex2exp)
library(RstoxFramework)
source('utils.r')

###############################################################################
#Run the stox project
###############################################################################
path_to_stock<-'HERAS2021'
data_stox_NS <- runModel(path_to_stock,modelName = 'baseline')


###############################################################################
#Get stratum boundaries
###############################################################################
df <- read.table("MSHAS_2020_HER.wkt",header = F, sep = "\t")
readWKT(as.character(text=df[1]))
wow <- apply(df, 1, function(x) readWKT(as.character(x))) # Applies readWKT to every row of your df, i.e. to each WKT object
works = list()
for (i in 1:length(wow)) { 
  works[[i]] <- as.data.frame(coordinates(wow[[i]]@polygons[[1]]@Polygons[[1]]))
}

NASC_plot(df = data_stox_NS,cat = 'HER',works = works)
Track_plot(df=data_stox_NS,works = works)
