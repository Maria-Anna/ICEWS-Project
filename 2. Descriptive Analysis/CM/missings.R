################################################################THE MISSINGS####################################################################

#Set working directory
setwd("~/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa")

#Load events africa data set
events_africa<- read.delim("events_africa_no_israel.tsv",header = TRUE,sep= "\t")
events_africa$Year <- format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 

###############################
#Missings Summary Statistics
##############################

#Does the data set contain missings?
colSums(is.na(events_africa))
#Findings:
#Source Sector: 82123 --> now: 83936 
#Target Sector: 165284--> now:167940
#City: 359674--> now:365536
#District:  1137346--> now: 1151169 
#Province: 271525--> now:    275904 






