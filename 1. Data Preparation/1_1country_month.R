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
setwd("~/ICEWS Project/")
rm(list=ls())
###################################################
############ Prepare Dataset ######################
###################################################

#Load ICEWS Africa dataset
events_africa<- read.delim("data_icews_cm.csv",header = TRUE,sep= "\t")
events_africa$Event.Date <- as.Date(events_africa$Event.Date, format="%Y-%m-%d")
data<-events_africa

#Add variables
data$Year<-format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 
data$Year_month<-format(as.Date(data$Event.Date, format="%Y-%m-%d"),"%Y-%m") 
data$Month<-as.numeric(format(as.Date(data$Event.Date, format="%Y-%m-%d"), "%m"))

#Remove events data set
rm(events_africa)

#Uniquely identify observations
data$ID<- cumsum(!duplicated(data))

#Save new data set
data_cm<-data
rm(data)

#Summary data set: 1320102 observations and 24 variables

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
data_cm$Country<-as.character(data_cm$Country) #Set country name as character value

#for Country:
data_cm["Country"][data_cm["Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
data_cm["Country"][data_cm["Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia

#for Source Country:
data_cm["Source.Country"][data_cm["Source.Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
data_cm["Source.Country"][data_cm["Source.Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia

#for Target Country:
data_cm["Target.Country"][data_cm["Target.Country"]=="Democratic Republic of Congo"] <- "Congo, DRC" #Change to Congo, DRC
data_cm["Target.Country"][data_cm["Target.Country"]=="Gambia"] <- "The Gambia" #Change to The Gambia

#Generate Country ID Variable

#for countries with same ID across years:
data_cm<-merge(data_cm,country_code, by.x = "Country", by.y="country_name", all=T, sort=F)

#for countries with changing ID across years:

#for Tanzania: till 1996-02 Country ID 236, from 1996-02 Country ID 242
data_cm$country_id[data_cm$Year_month<"1996-02" & data_cm$Country=="Tanzania"]<- "236"
data_cm$country_id[data_cm$Year_month>="1996-02" & data_cm$Country=="Tanzania"]<- "242"

#for Sudan: till 1996-02 Country ID 236, from 1996-02 Country ID 242
data_cm$country_id[data_cm$Year_month<"2011-07" & data_cm$Country=="Sudan"]<- "59"
data_cm$country_id[data_cm$Year_month>"2011-07" & data_cm$Country=="Sudan"]<- "245"

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
data_cm<-merge(data_cm,month_code, by.x = "Year_month", by.y="date", sort=F)

##############
#Keys
##############

#key_cm: monthid_countryid
data_cm$key_cm<-paste(data_cm$month_id,data_cm$country_id,sep="_")
data_cm$key_cm[data_cm$Year_month=="2011-07" & data_cm$Country=="Sudan"]<-NA  #gen missings for Sudan

#key_cy: year_countryid
data_cm$key_cy<-paste(data_cm$Year,data_cm$country_id,sep="_")
data_cm$key_cy[data_cm$Year_month=="2011-07" & data_cm$Country=="Sudan"]<-NA  #gen missings for Sudan

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
data_cm<-merge(data_cm,cameo_codes, by.x="Event.Text", by.y="name")

###############
#Variable Names
###############

#year
names(data_cm)[names(data_cm) == "Year"] <- "year"

#month
names(data_cm)[names(data_cm) == "Month"] <- "month"

#date
data_cm$date<-format(as.Date(data_cm$Event.Date, format="%Y-%m-%d"),"%Y-%m-01") 

#cameo root code
names(data_cm)[names(data_cm) == "lvl0"] <- "CAMEO_root"


################################################################
############Export Full Prio Dataset###########################
###############################################################

#Load, save and remove
save(data,file="data.Rdata")
save(data_cm, file = "data_cm.Rdata")
rm(data)
rm(data_cm)
load("data.Rdata")
load("data_cm.Rdata")


