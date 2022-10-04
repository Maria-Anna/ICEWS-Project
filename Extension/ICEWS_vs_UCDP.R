############################################# TRUE ged best sb UCDP vs. real time forecasts #################

#Load necessary packages
library(tidyr)


#load data set
ucpd_ged <- read.csv("GED_event.csv")


#------------------------------------------------------------------------------------------------------------#
#STEP 1
#Filter data set for:
#year: 2020 till 2021

ucpd_ged$year<-as.Date(as.character(ucpd_ged$year),"%Y")
ucdp_ged_sub<- ucpd_ged %>% filter(year>=2020)


