##############################################################THE PRIO GRID-MONTH############################################################

#Load necessary packages
library(dplyr)
library(sf)
library(data.table)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(readr)



###################################################
############ Prepare Dataset ######################
###################################################

#Load ICEWS Africa dataset
load("~/ICEWS-Project/Data/data_icews_cm.RData")



#Format Date variables correctly
data_icews_cm$Event.Date <- as.Date(data_icews_cm$Event.Date, format="%Y-%m-%d")
data_icews_cm$Year_month<-as.Date(data_icews_cm$Year_month, format="%Y-%m")
data_icews_cm$year<-as.numeric(data_icews_cm$year) 
data_icews_cm$month<-as.numeric(data_icews_cm$month) 



##################################################
############ Load and Prepare Prio Grid ##########
##################################################

#Set working directory with PRIO Grid Shapefiles
folder_cellshp<- "~/ICEWS-Project/Data/priogrid_cellshp"

#Read Prio Data
prio <- st_read(dsn = folder_cellshp, layer = "priogrid_cell", stringsAsFactors = F, quiet=T) %>% mutate(gid = as.character(gid))
#Remarks: 
#Closed Polygon, therefore repeated coordinate in geometry at start and end
#In total: 259200 cells, as both land and sea are covered--> can be limited to terrestrial grid cells (in total: 64818)
#In total: 6 variables

#Assig "Grid ID" to ICEWS Africa dataset
data_icews_pgm <- st_as_sf(data_icews_cm, coords = c("Longitude", "Latitude"), crs = st_crs(prio))
data_icews_pgm <- st_join(data_icews_pgm, prio)
#Remark:
#CRS in WGS 84 in both data sets

#Tranform geometry into longitude and latitude
data_icews_pgm<-data_icews_pgm %>% extract(geometry, c('Longitude', 'Latitude'), '\\((.*), (.*)\\)', convert = TRUE) 

#Drop geometry variable
data_icews_pgm<- as.data.frame(data_icews_pgm)
data_icews_pgm<- data_icews_pgm[,!(names(data_icews_pgm) %in% c("geometry"))]


#Summary: 1500669 observations

######################################################
############ Prio Grid Validation ####################
#####################################################

#Check for missings of Prio ID
sum(is.na(data_icews_pgm$gid))
#Findings:
#0 missings

#Check for duplicates in ID
duplicates_prio<- data_icews_pgm[duplicated(data_icews_pgm$ID),]
#Findings: 180567 duplicates 

#Group by ID to see number of duplicates by ID
duplicates_prio<-duplicates_prio %>% group_by(ID) %>% summarise(n())
summary(duplicates_prio)
#Findings: 3 duplicates by ID, one coordinate is assigned to 4 different grid cells

#Check for one Grid ID if the coordinates are correct
duplicates_prio[duplicates_prio$ID=="110",] #the same ID is assigned 3 different grid cells
table(prio[prio$gid=="167464 ",]) #check for each grid cell ID and see that the coordinate lies on the corner of each grid

############################################
######### Handling Duplicates #############
###########################################

#Drop Duplicates
data_icews_pgm<-data_icews_pgm[!duplicated(data_icews_pgm$ID),] 


#Save
write_rds(data_icews_pgm, file="~/ICEWS-Project/Data/data_icews_pgm")
