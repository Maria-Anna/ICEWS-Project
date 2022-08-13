############################################################## PGM Data + ICEWS ############################################################

#Load necessary packages
library(dplyr)
library(sf)
library(data.table)
library(tidyr)
library(tidyselect)
library(tidyverse)

#Set working directory
 
#----------------------------------------------------------------------------------------------------------------------------------------------
###################################################
#Prepare Data Set
###################################################

#Load ICEWS Events Africa data set
#Remark: required is "make_events_africa.R"
events_africa<- read.delim("~/events_africa.tsv")
#Set date as date format
events_africa$Event.Date <- as.Date(events_africa$Event.Date, format="%Y-%m-%d")

#Append 2020 data set (month 5 to 8)
data_2020<- read.csv("~/data_2020.csv")
data_2020$Event.Date <- as.Date(data_2020$Event.Date, format="%Y-%m-%d")
events_africa<-rbind(events_africa, data_2020)

#Change data set name
data<-events_africa

#Add variables
data$Year<-format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 
data$Year_month<-format(as.Date(data$Event.Date, format="%Y-%m-%d"),"%Y-%m") 
data$Month<-as.numeric(format(as.Date(data$Event.Date, format="%Y-%m-%d"), "%m"))

#Drop duplicates
data<-data[!duplicated(data),]

#Remove data set
rm(events_africa)

#Uniquely identify observations
data$ID<- cumsum(!duplicated(data))

#----------------------------------------------------------------------------------------------------------------------------------------------
###################################################
#Prepare Prio Grid (50x50km) Shapefiles
###################################################

#Set working directory with PRIO Grid Shape Files
#PRIO shape files can be found: https://grid.prio.org/#/extensions
#Source: Tollefsen et al. (2012)
folder_cellshp<- "~/priogrid_cellshp"

#Read PRIO data
prio <- st_read(dsn = folder_cellshp, layer = "priogrid_cell", stringsAsFactors = F, quiet=T) %>% mutate(gid = as.character(gid))
#Remarks: 
#Closed Polygon, therefore repeated coordinate in geometry at start and end
#In total: 259200 cells, as both land and sea are covered--> can be limited to terrestrial grid cells (in total: 64818)
#In total: 6 variables

#Assign "Grid ID" to ICEWS Africa data set
data <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(prio))
data_prio <- st_join(data, prio)
#Remark:
#CRS in WGS 84 in both data sets

#Transform geometry into longitude and latitude
data_prio<-data_prio %>% extract(geometry, c('Longitude', 'Latitude'), '\\((.*), (.*)\\)', convert = TRUE) 

#Drop geometry variable
data_prio<- as.data.frame(data_prio)
data_prio<- data_prio[,!(names(data_prio) %in% c("geometry"))]

#Remove non-necessary data sets
rm(data)
rm(prio)

#----------------------------------------------------------------------------------------------------------------------------------------------
###################################################
# Prio Grid Validation
###################################################

#Check for missings of Prio ID
sum(is.na(data_prio$gid))
#Findings:
#0 missings

#Check for duplicates in ID
duplicates_prio<- data_prio[duplicated(data_prio$ID),]
#Findings: 330231 duplicates
#Remark: duplicates are not removed, coordinate is on edge of four PRIO grids

#Check for duplicates by grouped ID
duplicates_prio<-duplicates_prio %>% group_by(ID) %>% summarise(n())
summary(duplicates_prio)
#Findings: 3 duplicates by ID, one coordinate is assigned to 4 different PRIO grid cells


#----------------------------------------------------------------------------------------------------------------------------------------------
###################################################
#Load PGM Data and CM Data by Fritz et al. (2021)
###################################################

#Upload Country data from Fritz et al. (2021)
cm_data = fread("cm_data.csv")

#Upload PGM data by Fritz et al. (2021)
load("pgm_data.RData")


######################################################
#########Load and Prepare Country Code################
######################################################

#Upload Country data from Fritz et al. (2021)
cm_data = fread("cm_data.csv")

#Keep per Country the Country ID
country_code <-cm_data[,c(6:7)]
country_code<-country_code[!duplicated(country_code),]
rm(cm_data)

#Country Code Remarks:
#Observations that are double: Ethiopia, South Africa, Sudan, Tanzania
#Observation NOT in Africa: Israel
#Observations with different country name: "Congo, DRC" and "Democratic Republic of Congo" and "The Gambia" and "Gambia"

#Year-month where countries change country code:
#Ethiopia: 1993-05-01, from 191 to 57
#South Africa: 1990-03-01 from 192 to 163
#Sudan: 2011-07 from 59 to 245
#Tanzania: 1996-02 from 236 to 242

#----------------------------------------------------------------------------------------------------------------------------------------------
#########################################################
#Combine ICEWS and CM and PGM Data of Fritz et al. (2021)
#########################################################

#Generate following variables:
#key_cm: monthid_countryid
#key_pm: monthid_pgid
#key_py: year_pgid
#month_id: monthid
#country_id: countryid
#pg_id: pgid
#month: from 1 to 12
#long: change name
#lat: change name
#key_cy: year_countryid
#date: in date format year-month-day

##############
#Country ID
##############

#Generate country code list for Africa
country_code$country_id<-as.character(country_code$country_id)
country_code<-subset(country_code,country_name!="Israel") #remove Israel from Country Code list
country_code<-subset(country_code,country_id!="191")#for Ethiopia keep Country ID 57
country_code<-subset(country_code,country_id!="192")#for South Africa keep Country ID 163
country_code<-subset(country_code,country_name!="Sudan" & country_name!="Tanzania")

#Change country names to uniform country names
data_prio$Country<-as.character(data_prio$Country) #Set country name as character value
data_prio$Source.Country<-as.character(data_prio$Source.Country) #Set country name as character value
data_prio$Target.Country<-as.character(data_prio$Target.Country) #Set country name as character value

#for Country
data_prio["Country"][data_prio["Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
data_prio["Country"][data_prio["Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia

#for Source.Country
data_prio["Source.Country"][data_prio["Source.Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
data_prio["Source.Country"][data_prio["Source.Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia


#for Target.Country
data_prio["Target.Country"][data_prio["Target.Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
data_prio["Target.Country"][data_prio["Target.Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia


#Generate Country ID Variable

#for countries with same ID across years:
data_prio<-merge(data_prio,country_code, by.x = "Country", by.y="country_name", all=T, sort=F)

#for countries with changing ID across years:
#for Tanzania: till 1996-02 Country ID 236, from 1996-02 Country ID 242
data_prio$country_id[data_prio$Year_month<"1996-02" & data_prio$Country=="Tanzania"]<- "236"
data_prio$country_id[data_prio$Year_month>="1996-02" & data_prio$Country=="Tanzania"]<- "242"
#for Sudan: till 1996-02 Country ID 236, from 1996-02 Country ID 242
data_prio$country_id[data_prio$Year_month<"2011-07" & data_prio$Country=="Sudan"]<- "59"
data_prio$country_id[data_prio$Year_month>"2011-07" & data_prio$Country=="Sudan"]<- "245"

##############
#Month ID
##############

#Generate Month Code list for Year-month
month_code<-cm_data[,c(1,10)] #keep for each year-month the month_id
month_code<-month_code[!duplicated(month_code),] #drop duplicates
month_code$date<-format(as.Date(month_code$date, format="%Y-%m-%d"),"%Y-%m")#change date format for merging

#Generate month ID
data_prio<-merge(data_prio,month_code, by.x = "Year_month", by.y="date", sort=F)

##############
#Keys
##############


#key_cm: monthid_countryid
data_prio$key_cm<-paste(data_prio$month_id,data_prio$country_id,sep="_")
data_prio$key_cm[data_prio$Year_month=="2011-07" & data_prio$Country=="Sudan"]<-NA  #gen missings for Sudan

#key_pm: monthid_pgid
data_prio$key_pm<-paste(data_prio$month_id,data_prio$gid,sep="_")

#key_py: year_pgid
data_prio$key_py<-paste(data_prio$Year,data_prio$gid,sep="_")

#key_cy: year_countryid
data_prio$key_cy<-paste(data_prio$Year,data_prio$country_id,sep="_")
data_prio$key_cy[data_prio$Year_month=="2011-07" & data_prio$Country=="Sudan"]<-NA  #gen missings for Sudan


###############
#CAMEO ROOT
###############
#Install package from github
#library("remotes")
#remotes::install_github("andybega/icews")
#library(icews)
#library(DBI)

#load CAMEO data set and keep relevant variables
data("cameo_codes")
cameo_codes<-cameo_codes[,c("name","lvl0")]

#merge with data by event text
data_prio<-merge(data_prio,cameo_codes, by.x="Event.Text", by.y="name")

###############
#Variable Names
###############

#year
names(data_prio)[names(data_prio) == "Year"] <- "year"

#month
names(data_prio)[names(data_prio) == "Month"] <- "month"

#date
data_prio$date<-format(as.Date(data_prio$Event.Date, format="%Y-%m-%d"),"%Y-%m-01") 

#pg_id
names(data_prio)[names(data_prio) == "gid"] <- "pg_id"

#cameo root code
names(data_prio)[names(data_prio) == "lvl0"] <- "CAMEO_root"



#----------------------------------------------------------------------------------------------------------------------------------------------
###################################################
#Export and Save Data Set
###################################################

data_icews_pgm<- data_prio

#Save
#write.csv(data_icews_pgm, file = "~/data_icews_pgm.csv", row.names = FALSE)
#save(data_icews_pgm, file = "~/data_icews_pgm.RData")
#write_rds(data_icews_pgm, file="~/data_icews_pgm")

#Remark: possible to drop duplicates and save data set
#Take Away: it should be explained how and why duplicates are dropped
data_icews_pgm<-data_icews_pgm[!duplicated(data_icews_pgm$ID),] 









