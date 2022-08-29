############################################################# Read ICEWS ###################################################################

#Load necessary packages
library(plyr)
library(dplyr)

#Assign Path with folder containing all ICEWS Files 
path<- "~/ICEWS-Project/Data/Datasets_ICEWS_Downloaded"
#Assign Path to save the created Data events_africa
path_events_africa<- "~/ICEWS-Project/Data"
#set Working Directory
setwd(path)


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
#Remark: Read 2017 again because there is an coding errors. We need to correct that
data<-read.delim("Events.2017.20201119.tab",na.strings=c("NULL","NA"))

#Convert Factor to character
data$CAMEO.Code<- as.character(data$CAMEO.Code)

#Hardcode CAMEO.Code "13y" to "137"
data<-data %>% mutate(CAMEO.Code = replace(CAMEO.Code, CAMEO.Code == "13y", "137"))

#Convert CAMEO.Code to numeric
data$CAMEO.Code<- as.integer(as.numeric(data$CAMEO.Code))

#Replace old 2017 data set in list with new 2017 data set
list_all_ICEWS[[21]]<-data


#-------------------------------------------------------------------------------------------------------------------------------------------------------------
######################################################
#Filter Data Set: Events within countries
######################################################

#Generate list with 54 African countries + Israel
states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo","Congo, DRC", "Democratic Republic of Congo", "Congo, Democratic Republic of the","Cote d'Ivoire","Republic of Cote d'Ivoire", "DRC Cote d'Ivoire",
                 "Congo, Republic of the Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya",
                 "Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria","Ruanda", "Rwanda", "Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan","Swaziland","Tanzania","The Gambia","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe", "Israel")





#Relevant for project: Events with Source.Country, Target.Country and Country == Africa and Source.Country==Target.Country==Country
#List with datasets that occur in Africa so  Source.Country, Target.Country and Country equals to Africa
list_africa_total<-lapply(list_all_ICEWS, function(x) dplyr::filter(x, Country %in% states_africa & Source.Country %in% states_africa & Target.Country %in% states_africa))

#Generate data set from list
events_africa_total<-reshape::merge_all(list_africa_total)

#New Format for some variables. They have to be characters
events_africa_total$Country<-as.character(events_africa_total$Country)
events_africa_total$Source.Country<-as.character(events_africa_total$Source.Country)
events_africa_total$Target.Country<-as.character(events_africa_total$Target.Country)


#Keep only observations where Source.Country==Target.Country==Country
events_africa<- subset(events_africa_total, events_africa_total$Country==events_africa_total$Source.Country & events_africa_total$Country==events_africa_total$Target.Country)

#Change Event.Date Format
events_africa$Event.Date <- as.Date(events_africa$Event.Date, format="%Y-%m-%d")

#Keep only observations till 31-08-2020
events_africa<- events_africa %>% filter(Event.Date < "2020-09-01")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
######################################################
# Export Data
######################################################

#Save data set as csv
#Remark: relevant data set for future analysis
write.csv(events_africa, file= paste(path_events_africa, "events_africa.csv", sep=""), row.names = FALSE)









