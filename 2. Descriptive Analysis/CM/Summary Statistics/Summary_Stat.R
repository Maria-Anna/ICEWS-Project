################################################################ The Summary Statistics ###########################################################

#Set working directory

#----------------------------------------------------------------------------------------------------------------------------#
############################
# Load Data Set
###########################

#Load events africa data set
events_africa<- read.delim("events_africa.tsv",header = TRUE,sep= "\t")
events_africa$Year <- format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 


#----------------------------------------------------------------------------------------------------------------------------#
#############################
# Summary Statistics Overall
############################

stargazer(events_africa, summary.stat = c("mean", "median", "min", "max", "sd", "p25", "p75"))

#----------------------------------------------------------------------------------------------------------------------------#
############################
# Missings
###########################

#Does the data set contain missings?
colSums(is.na(events_africa))
#Findings:
#Source Sector: 83936 
#Target Sector: 167940
#City: 365536
#District: 1151169 
#Province: 275904 

#Generate table with missings for latex
events_africa_na <-as.data.frame(colSums(is.na(events_africa)))
print(xtable(events_africa_na, type = "latex"), file = "missings_events_africa.tex")


#----------------------------------------------------------------------------------------------------------------------------#
############################
# Duplicates
###########################

#Check for duplicates in data set

#For all columns
#0 duplicates
sum(duplicated(events_africa))

#For Event.ID
#1436 duplicates
sum(duplicated(events_africa$Event.ID))
#Remark:
#The Event.ID is a non-unique identifier --> the event ID can be repeated in the data set (See also ICEWS Coded Event Data Read Me)


#For Story.ID
#489032 duplicates
table(duplicated(events_africa$Story.ID))
#Remark:
#From the same event different information of a story can be extracted

#Generate duplicate data set regarding: all columns
duplicates_all <- events_africa %>%
  group_by(events_africa[]) %>%
  filter(n()>1)

#Generate duplicate data set regarding: Event.ID
duplicates_event_id <- events_africa %>%
  group_by(Event.ID) %>%
  filter(n()>1)


#Generate duplicate data set regarding Story.ID
duplicates_story_id <- events_africa %>%
  group_by(Story.ID) %>%
  filter(n()>1)







