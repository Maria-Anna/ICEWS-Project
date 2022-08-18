############################################################# Read ICEWS ###################################################################

#Load necessary packages
library(plyr)
library(dplyr)

#Set working directory
#Working directory should contain folder with: tsv and tab files with ICEWS data from 1995 to 2020

#----------------------------------------------------------------------------------------------------------------------------------------------
######################################################
#Read Data:
######################################################

#Generate list with all names of all tab and tsv files in the working directory 
names_files_tab = list.files(pattern= "*.tab")
names_files_tsv= list.files(pattern="*.tsv")
 
#Read all files 
list_all_ICEWS = lapply(c(names_files_tab, names_files_tsv), read.delim, quote="",na.strings=c("","NA"))

#Read 2017 file 
#Remark: 2017 should be read parallel because of coding errors that need to be corrected for
data<-read.delim("Events.2017.20201119.tab",na.strings=c("NULL","NA"))
#Convert Factor to character
data$CAMEO.Code<- as.character(data$CAMEO.Code)
#Hardcode CAMEO.Code "13y" to "137"
data<-data %>% 
  mutate(CAMEO.Code = replace(CAMEO.Code, CAMEO.Code == "13y", "137"))
#Convert CAMEO.Code to numeric
data$CAMEO.Code<- as.integer(as.numeric(data$CAMEO.Code))

#Replace old 2017 data set in list with new 2017 data set
list_all_ICEWS[[21]]<-data

#Read and add 2020 data set and keep months 5,6,7 and 8
#Remark: in future analysis 2020 can be used without month filter (in this project: month 1 to 8 needed)
data_2020<- read.delim("events.2020.20220623.tab",na.strings=c("","NA"))
#Cameo Code as integer
data_2020$CAMEO.Code<- as.character(data_2020$CAMEO.Code)
data_2020$CAMEO.Code<- as.integer(as.numeric(data_2020$CAMEO.Code))
#filter for 2020 months
data_2020$Event.Date<-as.Date(data_2020$Event.Date)
data_2020 <- data_2020 %>% filter(Event.Date>="2020-05-01")
data_2020 <- data_2020 %>% filter(Event.Date<"2020-09-01")

#----------------------------------------------------------------------------------------------------------------------------------------------
######################################################
#Relevant Country List:
######################################################

#Generate list with 54 African countries + Israel
states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo","Congo, DRC", "Democratic Republic of Congo", "Congo, Democratic Republic of the","Cote d'Ivoire","Republic of Cote d'Ivoire", "DRC Cote d'Ivoire",
                 "Congo, Republic of the Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya",
                 "Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria","Ruanda", "Rwanda", "Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan","Swaziland","Tanzania","The Gambia","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe", "Israel")

#Possible to add a variable to each data set to mark the data set year it was extracted from
#year <- 0
#for (year in 1995:2020){
  #temp <- list_all_ICEWS[[year-1994]]
  #temp$datasetYear <- year
  #list_all_ICEWS[[year-1994]]<-temp
#}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
######################################################
#Filter Data Set: Events within countries
######################################################

#Relevant for project: Events with Source.Country, Target.Country and Country == Africa and Source.Country==Target.Country==Country

#Filter ICEWS data set to contain events on the African continent
list_africa_total<-lapply(list_all_ICEWS, function(x) dplyr::filter(x, Country %in% states_africa & Source.Country %in% states_africa & Target.Country %in% states_africa))

#Generate data set from list
events_africa_total<-reshape::merge_all(list_africa_total)

#Keep only observations where Source.Country==Target.Country==Country
events_africa_total$Country<-as.character(events_africa_total$Country)
events_africa_total$Source.Country<-as.character(events_africa_total$Source.Country)
events_africa_total$Target.Country<-as.character(events_africa_total$Target.Country)
events_africa<- subset(events_africa_total, events_africa_total$Country==events_africa_total$Source.Country & events_africa_total$Country==events_africa_total$Target.Country)

#Save data set as csv
#Remark: relevant data set for future analysis
write.csv(events_africa, file= "~/events_africa.csv")

#for 2020 parallel analysis:
#filter for above mentioned condition
data_2020 <- data_2020 %>% filter(Country %in% states_africa & Source.Country %in% states_africa & Target.Country %in% states_africa)
data_2020<- subset(data_2020, data_2020$Country==data_2020$Source.Country & data_2020$Country==data_2020$Target.Country)
#save as 2020 data set as csv
#write.csv(data_2020, file="data_2020.csv", row.names = F)



#-------------------------------------------------------------------------------------------------------------------------------------------------------------
####################################################
#Full ICEWS data set directly from harvard dataverse
####################################################

#Upload Package from: https://www.andybeger.com/icews/reference/read_icews.html
#Remark: Variable names differ from the above used data sets

#Sys.setenv(DATAVERSE_SERVER = "dataverse.harvard.edu")
#library("icews")
#dir.create("~/Downloads/icews")
#download_data("~/Downloads/icews")
#events_icews <- read_icews("~/Downloads/icews")






