############################################################## priogrid_cellshp + ICEWS + cm_data ############################################################

#Load necessary packages
library(dplyr)
library(sf)
library(data.table)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(readr)


rm(list=ls())


#Assign path with  data_icews_cm.csv 
path_data_icews_cm<- "~/ICEWS-Project/Data/data_icews_cm.csv"

#Assign path with folder containing Shapefiles for the Prio Grid cells
#PRIO shape files can be found: https://grid.prio.org/#/extensions, Source: Tollefsen et al. (2012)
path_priogrid<-"~/ICEWS-Project/Data/priogrid_cellshp"

#Assign path were to save new created data: data_icews_pgm.csv
path_data_icews_pgm<- "~/ICEWS-Project/Data"


#----------------------------------------------------------------------------------------------------------------------------------------------
###################################################
#Make data set with Prio Grids
###################################################

#Load ICEWS data_icews_cm data set
data_icews_cm<- read.csv(path_data_icews_cm)

#Read PRIO data
prio <- st_read(dsn = path_priogrid, layer = "priogrid_cell", stringsAsFactors = F, quiet=T) %>% mutate(gid = as.character(gid))

#Assign "Grid ID" to ICEWS Africa data set
#Remark:CRS in WGS 84 in both data sets
data_icews_cm <- st_as_sf(data_icews_cm, coords = c("Longitude", "Latitude"), crs = st_crs(prio))
data_icews_pgm<- st_join(data_icews_cm, prio)

#Transform geometry into longitude and latitude
data_icews_pgm<-data_icews_pgm %>% extract(geometry, c('Longitude', 'Latitude'), '\\((.*), (.*)\\)', convert = TRUE) 

#Drop geometry variable
data_icews_pgm<- as.data.frame(data_icews_pgm)
data_icews_pgm<- data_icews_pgm[,!(names(data_icews_pgm) %in% c("geometry"))]

#Remove non-necessary data sets
#rm(data_icews_cm)
#rm(prio)


#----------------------------------------------------------------------------------------------------------------------------------------------
###################################################
#Export and Save Data Set
###################################################


#Save
write_rds(data_icews_pgm, file=paste(path_data_icews_pgm, "/data_icews_pgm", sep=""))











