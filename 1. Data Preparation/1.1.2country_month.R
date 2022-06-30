########################################################################THE COUNTRY-MONTH##########################################################################

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
setwd("~/ICEWS-Project")
rm(list=ls())
###################################################
############ Prepare Dataset ######################
###################################################

#Load ICEWS Africa dataset
events_africa<- read.csv("~/ICEWS-Project/Data/Preparation Data/events_africa.csv")
events_africa$Event.Date <- as.Date(events_africa$Event.Date, format="%Y-%m-%d")


#Add variables
events_africa$Year<-format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 
events_africa$Year_month<-format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y-%m") 
events_africa$Month<-as.numeric(format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"), "%m"))


#Uniquely identify observations
events_africa$ID<- cumsum(!duplicated(events_africa))


#Summary data set: 1320102 observations and 24 variables

######################################################
#########Load and Prepare Country Code################
######################################################

#Upload Country data from Fritz et al. (2021)
cm_data = fread("~/ICEWS-Project/Data/cm_data.csv")

#Keep per Country the Country ID
country_code <-cm_data[,c(6:7)]
country_code<-country_code[!duplicated(country_code),]
#rm(cm_data)

#Country Code Remarks:
#In total 55 Countries (incl.Israel)
#Observations that are double: Ethiopia, South Africa, Sudan, Tanzania
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
country_code<-subset(country_code,country_id!="191")#for Ethiopia keep Country ID 57
country_code<-subset(country_code,country_id!="192")#for South Africa keep Country ID 163
country_code<-subset(country_code,country_name!="Sudan" & country_name!="Tanzania")

#Change Country names to uniform country names
events_africa$Country<-as.character(events_africa$Country) #Set country name as character value

#for Country:
events_africa["Country"][events_africa["Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
events_africa["Country"][events_africa["Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia

#for Source Country
events_africa["Source.Country"][events_africa["Source.Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
events_africa["Source.Country"][events_africa["Source.Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia

#for Target Country:
events_africa["Target.Country"][events_africa["Target.Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
events_africa["Target.Country"][events_africa["Target.Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia

#Generate Country ID Variable

#for countries with same ID across years:
events_africa<-merge(events_africa,country_code, by.x = "Country", by.y="country_name", all=T, sort=F)

#for countries with changing ID across years:

#for Tanzania: till 1996-02 Country ID 236, from 1996-02 Country ID 242
events_africa$country_id[events_africa$Year_month<"1996-02" & events_africa$Country=="Tanzania"]<- "236"
events_africa$country_id[events_africa$Year_month>="1996-02" & events_africa$Country=="Tanzania"]<- "242"

#for Sudan: till 1996-02 Country ID 236, from 1996-02 Country ID 242
events_africa$country_id[events_africa$Year_month<"2011-07" & events_africa$Country=="Sudan"]<- "59"
events_africa$country_id[events_africa$Year_month>"2011-07" & events_africa$Country=="Sudan"]<- "245"

##############
#Month ID
##############

#Upload Country data from Fritz et al. (2021)
cm_data = fread("~/ICEWS-Project/Data/cm_data.csv")

#Generate Month Code list for Year-month
month_code<-cm_data[,c(1,10)] #keep for each year-month the month_id
month_code<-month_code[!duplicated(month_code),] #drop duplicates
month_code$date<-format(as.Date(month_code$date, format="%Y-%m-%d"),"%Y-%m")#change date format for merging

#Generate month ID
events_africa<-merge(events_africa,month_code, by.x = "Year_month", by.y="date", sort=F)

##############
#Keys
##############

#key_cm: monthid_countryid
events_africa$key_cm<-paste(events_africa$month_id,events_africa$country_id,sep="_")
events_africa$key_cm[events_africa$Year_month=="2011-07" & events_africa$Country=="Sudan"]<-NA  #gen missings for Sudan

#key_cy: year_countryid
events_africa$key_cy<-paste(events_africa$Year,events_africa$country_id,sep="_")
events_africa$key_cy[events_africa$Year_month=="2011-07" & events_africa$Country=="Sudan"]<-NA  #gen missings for Sudan

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
events_africa<-merge(events_africa,cameo_codes, by.x="Event.Text", by.y="name")

###############
#Variable Names
###############

#year
names(events_africa)[names(events_africa) == "Year"] <- "year"

#month
names(events_africa)[names(events_africa) == "Month"] <- "month"

#date
events_africa$date<-format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y-%m-01") 

#cameo root code
names(events_africa)[names(events_africa) == "lvl0"] <- "CAMEO_root"

data_icews_cm<-events_africa

################################################################
############Export Full Prio Dataset###########################
###############################################################

#Load, save and remove
#save(data_icews_cm, file = "data_icews_cm.Rdata")
#write.csv(data_icews_cm, file = "data_icews_cm.csv", row.names = FALSE)
#rm(data)
#rm(events_africa)
#load("data.Rdata")
#load("data_cm.Rdata")


