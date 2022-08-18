################################################################ The Summary Statistics ###########################################################

#Set working directory

#----------------------------------------------------------------------------------------------------------------------------#
############################
# Load Data Set
###########################

#Load events ICEWS data set
events_africa<-fread("data_icews_cm.csv")

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
#Source Sector: 82949
#Target Sector: 166881
#City: 364747
#District: 1149096
#Province: 275203 

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
#487794 duplicates
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

#----------------------------------------------------------------------------------------------------------------------------#
############################
# CAMEO Root Code Freq
###########################

#Load packages
#library("remotes")
#remotes::install_github("andybega/icews")
#library(icews)
#library(DBI)

#Load CAMEO data set and keep relevant variables
data("cameo_codes")
cameo_codes<-cameo_codes[,c("cameo_code","name","lvl0","lvl1")]
cameo_codes<- cameo_codes %>% filter(cameo_code=="01" |cameo_code=="02"| cameo_code=="03"| cameo_code=="04"| cameo_code=="05"| cameo_code=="06"| cameo_code=="07"| cameo_code=="08"| cameo_code=="09"| cameo_code=="10" |
                                       cameo_code=="11"| cameo_code=="12"| cameo_code=="13"| cameo_code=="14"| cameo_code=="15"| cameo_code=="16"| cameo_code=="17" | cameo_code=="18" | cameo_code=="19" | cameo_code=="20")
data<-merge(events_africa,cameo_codes, by.x="CAMEO_root", by.y="lvl0")
cameo_freq<-data %>% group_by(CAMEO_root,name) %>% count(sort=TRUE)

#Export as latex document
print(xtable(cameo_freq, type = "latex"), file = "cameo_freq.tex")

#----------------------------------------------------------------------------------------------------------------------------#
############################
# Intensity Freq
###########################

#Generate Intensity Freq table
int_freq<-events_africa %>% group_by(Intensity) %>% count(sort=TRUE)

#Generate Freq bins
bins<-c(-10,-0.1,0,0.1,10)
int_freq$Intensity<- cut(int_freq$Intensity, breaks = bins,include.lowest = T)
int_freq_sum <- int_freq %>% group_by(Intensity) %>% summarise(Frequency = sum(n))

#Export as latex document
print(xtable(int_freq_sum, type = "latex"), file = "int_freq.tex")








