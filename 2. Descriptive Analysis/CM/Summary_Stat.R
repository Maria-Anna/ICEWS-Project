################################################################THE SUMMARY STAT###########################################################

#Set working directory
setwd("~/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa")

#Load events africa data set
events_africa<- read.delim("events_africa.tsv",header = TRUE,sep= "\t")
events_africa$Year <- format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 

stargazer(events_africa, summary.stat = c("mean", "median", "min", "max", "sd", "p25", "p75"))
