################################################################THE MISSINGS####################################################################

#Set working directory
setwd("~/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa")

#Load events africa data set
load("/Users/clarita/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa/Data Sets/data_icews_cm.Rdata")
min(data_icews_cm$Year_month)
events_africa<- read.delim("events_africa.tsv",header = TRUE,sep= "\t")
events_africa$Year <- format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 
events_africa<-data_icews_cm
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

#generate table with missings for latex
events_africa_na <-as.data.frame(colSums(is.na(events_africa)))
print(xtable(events_africa_na, type = "latex"), file = "missings_events_africa.tex")




