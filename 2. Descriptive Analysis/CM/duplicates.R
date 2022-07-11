################################################################THE DUPLICATES###########################################################

#Set working directory
setwd("~/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa")

#Load events africa data set
events_africa<- read.delim("events_africa.tsv",header = TRUE,sep= "\t")
events_africa$Year <- format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 

###############################
#Duplicates Summary Statistics
##############################

#Check for duplicates in data set

#for all columns
#0 duplicates
sum(duplicated(events_africa))

#for Event.ID
#1436 duplicates
sum(duplicated(events_africa$Event.ID))
#Remark:
#The Event.ID is a non-unique identifier --> the event ID can be repeated in the data set (See also ICEWS Coded Event Data Read Me)


#for Story.ID
#483193 duplicates
table(duplicated(events_africa$Story.ID))
#Remark:
#From the same event different information of a story can be extracted


#################################################
#Generate Datasets with non-unique observations
#################################################

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


###############################
#Analyse duplicate data set
##############################

#Analysis for: Event.ID






#Analysis for: Story.ID


#Nr 1: In which countries do the duplicate story IDs appear?



#Nr.2: In which year do the duplicate story IDs appear?















