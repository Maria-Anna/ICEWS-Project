##############################################################THE PRIO GRID-MONTH############################################################

#Load necessary packages
library(dplyr)
library(sf)
library(data.table)
library(tidyr)
library(tidyselect)
library(tidyverse)

#Set working directory

#for Clara
setwd("~/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa")
#for Maria-Anna
setwd("~/Consulting Bewaffnete Konflikte/Data")

###################################################
############ Prepare Dataset ######################
###################################################

#Load ICEWS Africa dataset
events_africa<- read.delim("events_africa.tsv",header = TRUE,sep= "\t")
events_africa$Event.Date <- as.Date(events_africa$Event.Date, format="%Y-%m-%d")
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

#Summary data set: 2359668 observations

##################################################
############ Load and Prepare Prio Grid ##########
##################################################

#Set working directory with PRIO Grid Shapefiles
folder_cellshp<- "~/Desktop/Consulting Bewaffnete Konflikte/Prio_Grid/priogrid_cellshp"

#Read Prio Data
prio <- st_read(dsn = folder_cellshp, layer = "priogrid_cell", stringsAsFactors = F, quiet=T) %>% mutate(gid = as.character(gid))
#Remarks: 
#Closed Polygon, therefore repeated coordinate in geometry at start and end
#In total: 259200 cells, as both land and sea are covered--> can be limited to terrestrial grid cells (in total: 64818)
#In total: 6 variables

#Assig "Grid ID" to ICEWS Africa dataset
data <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(prio))
data_prio <- st_join(data, prio)
#Remark:
#CRS in WGS 84 in both data sets

#Tranform geometry into longitude and latitude
data_prio<-data_prio %>% extract(geometry, c('Longitude', 'Latitude'), '\\((.*), (.*)\\)', convert = TRUE) 

#Drop geometry variable
data_prio<- as.data.frame(data_prio)
data_prio<- data_prio[,!(names(data_prio) %in% c("geometry"))]

#Remove non-necessary dataframes
rm(data)
rm(prio)

#Summary: 2689899 observations and 330231 duplicates

######################################################
############ Prio Grid Validation ####################
#####################################################

#Check for missings of Prio ID
sum(is.na(data_prio$gid))
#Findings:
#0 missings

#Check for duplicates in ID
duplicates_prio<- data_prio[duplicated(data_prio$ID),]
#Findings: 330231 duplicates

#Group by ID to see number of duplicates by ID
duplicates_prio<-duplicates_prio %>% group_by(ID) %>% summarise(n())
summary(duplicates_prio)
#Findings: 3 duplicates by ID, one coordinate is assigned to 4 different grid cells

#Check for one Grid ID if the coordinates are correct
duplicates_prio[duplicates_prio$ID=="158",] #the same ID is assigned 3 different grid cells
table(prio[prio$gid=="166745",]) #check for each grid cell ID and see that the coordinate lies on the corner of each grid

#Upload Prio Grid data from Fritz et al. (2021)
load("pgm_data.RData")

#Compare for two countries, Alegria and Somalia, if Prio Grid ID correct
pgm_data_subset<-pgm_data %>%filter(country_name=="Algeria"| country_name=="Somalia")
data_prio_subset<-data_prio %>% filter(Country=="Algeria"| Country=="Somalia")
summary(pgm_data_subset$pg_id)
summary(data_prio_subset$gid)
pgm_data<-pgm_data %>% filter(country_name=="Tanzania")

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

################################################################
############ Generate Variables#################################
################################################################
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

#Generate Country Code list for Africa
country_code$country_id<-as.character(country_code$country_id)
country_code<-subset(country_code,country_name!="Israel") #remove Israel from Country Code list
country_code<-subset(country_code,country_id!="191")#for Ethiopia keep Country ID 57
country_code<-subset(country_code,country_id!="192")#for South Africa keep Country ID 163
country_code<-subset(country_code,country_name!="Sudan" & country_name!="Tanzania")

#Change Country names to uniform country names
data_prio$Country<-as.character(data_prio$Country) #Set country name as character value

data_prio["Country"][data_prio["Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
data_prio["Country"][data_prio["Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia

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

#Upload Country data from Fritz et al. (2021)
cm_data = fread("cm_data.csv")

#Generate Month Code list for Year-month
month_code<-cm_data[,c(1,10)] #keep for each year-month the month_id
month_code<-month_code[!duplicated(month_code),] #drop duplicates
month_code$date<-format(as.Date(month_code$date, format="%Y-%m-%d"),"%Y-%m")#change date format for merging

#Generate month ID
data_prio<-merge(data_prio,month_code, by.x = "Year_month", by.y="date", sort=F)

##############
#Keys
##############

data_prio %>% filter(Country=="Sudan" & Year_month=="2011-07")
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
library("remotes")
#remotes::install_github("andybega/icews")
library(icews)
library(DBI)

#load CAMEO dataset and keep relevant variables
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

################################################################
############Export Full Prio Dataset###########################
###############################################################

#Load, save and remove
save(data,file="data.Rdata")
save(prio,file="prio.Rdata")
save(data_prio, file = "data_prio.Rdata")
rm(data)
rm(prio)
rm(data_prio)
load("prio.Rdata")
load("data.Rdata")
load("data_prio.Rdata")


