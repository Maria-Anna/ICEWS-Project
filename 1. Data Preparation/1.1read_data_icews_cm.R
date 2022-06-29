#############################################################Read Data Set###################################################################
library(plyr)
library(dplyr)


#Set working directory with all ICEWS Files 
#directory Clara
#directory<- "Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa/Downloaded"
#setwd(directory)
#Directory Maria-Anna
#setwd("C:/Users/mtsitsipa/Documents/Consulting/ICEWS")

#Set filename
file_name<- "data_icews_cm.csv"
#Assign Path were all ICEWS Files (Für die Zukunft wenn wir die Daten auf Github haben)
path<- "~/ICEWS-Project/Data/Raw Data/ICEWS"
#set Working Directory
setwd(path)

#List with all names of all tab and tsv files in the working directory 
names_files_tab = list.files( pattern= "*.tab")
names_files_tsv= list.files(pattern="*.tsv")

#Read all files 
list_all_ICEWS = lapply(c(names_files_tab, names_files_tsv), read.delim, quote="",na.strings=c("","NA"))

#Read 2017 File
data<-read.delim("Events.2017.20201119.tab",na.strings=c("NULL","NA"))
#Convert Factor to character
data$CAMEO.Code<- as.character(data$CAMEO.Code)
#Hardcode CAMEO.Code "13y" to "137"
data<-data %>% 
  mutate(CAMEO.Code = replace(CAMEO.Code, CAMEO.Code == "13y", "137"))
#Convert CAMEO.Code to numeric
data$CAMEO.Code<- as.integer(as.numeric(data$CAMEO.Code))


#Replace old dataframe with new
list_all_ICEWS[[21]]<-data


#Filter Africa (in total: 54 african countries without variation in the name)
states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo","Congo, DRC", "Democratic Republic of Congo", "Congo, Democratic Republic of the","Cote d'Ivoire","Republic of Cote d'Ivoire", "DRC Cote d'Ivoire",
                 "Congo, Republic of the Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya",
                 "Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria","Ruanda", "Rwanda", "Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan","Swaziland","Tanzania","The Gambia","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe", "Israel")

#Possible to add a variable to each dataset to mark the dataset year it was extracted from
#year <- 0
#for (year in 1995:2020){
  #temp <- list_all_ICEWS[[year-1994]]
  #temp$datasetYear <- year
  #list_all_ICEWS[[year-1994]]<-temp
#}



#-------------------------------------------------------------------------------------------------------------------------------------------------------------
######################################################
#Dataset: Events in Africa, from Africa and to Africa
######################################################

#Events with Source.Country, Target.Country and Country Africa

#List with datasets that fulfill above condition
list_africa_total<-lapply(list_all_ICEWS, function(x) dplyr::filter(x, Country %in% states_africa & Source.Country %in% states_africa & Target.Country %in% states_africa))

#Dataset of events that fulfill condition
events_africa_total<-reshape::merge_all(list_africa_total)

#Export dataset
#write.table(events_africa_total, file= "events_africa_total.tsv", sep= "\t", quote= FALSE )

#Keep only observations where source, target and country in the same country
events_africa_total$Country<-as.character(events_africa_total$Country)
events_africa_total$Source.Country<-as.character(events_africa_total$Source.Country)
events_africa_total$Target.Country<-as.character(events_africa_total$Target.Country)
events_africa<- subset(events_africa_total, events_africa_total$Country==events_africa_total$Source.Country & events_africa_total$Country==events_africa_total$Target.Country)

#Export dataset
write.table(events_africa, file= "events_africa.tsv", sep= "\t", quote= FALSE )

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
####################################################
#Full ICEWS dataset: directly from harvard dataverse
####################################################

#Upload Package from: https://www.andybeger.com/icews/reference/read_icews.html
#Remark: Variable names differ from the above used datasets

#Sys.setenv(DATAVERSE_SERVER = "dataverse.harvard.edu")
#library("icews")

#dir.create("~/Downloads/icews")
#download_data("~/Downloads/icews")
#events_icews <- read_icews("~/Downloads/icews")




