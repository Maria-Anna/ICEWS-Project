##############################################THE CAMEO EVENT TRAP#################################################

#Load necessary packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(grid)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggpmisc)
library(geometry)
library(cowplot)
library(gridBase)
library(gridGraphics)
library(data.table)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)

path_data_icews_cm<-"~/ICEWS-Project/Data/data_icews_cm.csv"
path_plot<- "~/ICEWS-Project/2. Descriptive Analysis/Plots"


###################################################
############ Prepare Dataset ######################
###################################################
data_icews_cm<- fread(path_data_icews_cm)

data<-data_icews_cm

#add variables
data$Year<-format(as.Date(data$Event.Date, format="%Y-%m-%d"),"%Y") 
data$Year_month<-format(as.Date(data$Event.Date, format="%Y-%m-%d"),"%Y-%m") 
data$Month<-as.numeric(format(as.Date(data$Event.Date, format="%Y-%m-%d"), "%m"))



#################################################################
############ Generate Data for Trap Matrix ######################
#################################################################

#####Create vectors:

cameo_hostile<-c("11","12","17","18","19","20","13","14","15","16")
cameo_peace<-c("1","2","3","4","5","6","7","8")
cameo_intensity_hostile <-c(seq(from=-0.1,to=-10, by=-0.1))
cameo_conflict<-c("18","19","20")


#####Create datasets for:

#hostile event
data_cameo_hostile<-data%>%group_by(Year_month,Country,Year)%>%count(CAMEO_root %in% cameo_hostile)%>%as.data.frame()

#peaceful events
data_cameo_peace<-data%>%group_by(Year_month,Country,Year)%>%count(CAMEO_root %in% cameo_peace)%>%as.data.frame()

#hostile intensity events
data_cameo_intensity<-data%>%group_by(Year_month,Country,Year)%>%count(Intensity %in% cameo_intensity_hostile)%>%as.data.frame()

#events with conflict
data_cameo_conflict<-data%>%group_by(Year_month,Country,Year)%>%count(CAMEO_root %in% cameo_conflict)%>%as.data.frame()



####Change data format:

################
##hostile events:
###############

data_cameo_hostile<-data_cameo_hostile %>% pivot_wider(names_from="CAMEO_root %in% cameo_hostile", values_from="n")

#change NA to 0:
data_cameo_hostile<-data_cameo_hostile %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

#rename columns
names(data_cameo_hostile)[names(data_cameo_hostile) == "FALSE"] <- "n_non"
names(data_cameo_hostile)[names(data_cameo_hostile) == "TRUE"] <- "n_hostile"

#create column with total number of events
data_cameo_hostile<-data_cameo_hostile %>% 
  rowwise() %>% 
  mutate(n_total = sum(n_non, n_hostile, na.rm = TRUE))

#create column with relative number of hostile events
data_cameo_hostile<-data_cameo_hostile %>% 
  rowwise() %>% 
  mutate(n_rel = n_hostile/n_total)

################
##peaceful events:
###############


data_cameo_peace<-data_cameo_peace %>% pivot_wider(names_from="CAMEO_root %in% cameo_peace", values_from="n")

#change NA to 0:
data_cameo_peace<-data_cameo_peace %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

#rename columns
names(data_cameo_peace)[names(data_cameo_peace) == "FALSE"] <- "n_non"
names(data_cameo_peace)[names(data_cameo_peace) == "TRUE"] <- "n_peace"

#create column with total number of events
data_cameo_peace<-data_cameo_peace %>% 
  rowwise() %>% 
  mutate(n_total = sum(n_non, n_peace, na.rm = TRUE))

#create column with relative number of hostile events
data_cameo_peace<-data_cameo_peace %>% 
  rowwise() %>% 
  mutate(n_rel = n_peace/n_total)

#########################
##Hostile Intensity events:
########################


data_cameo_intensity<-data_cameo_intensity %>% pivot_wider(names_from="Intensity %in% cameo_intensity_hostile", values_from="n")

#change NA to 0:
data_cameo_intensity<-data_cameo_intensity %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

#rename columns
names(data_cameo_intensity)[names(data_cameo_intensity) == "FALSE"] <- "n_non"
names(data_cameo_intensity)[names(data_cameo_intensity) == "TRUE"] <- "n_intensity_hostile"

#create column with total number of events
data_cameo_intensity<-data_cameo_intensity %>% 
  rowwise() %>% 
  mutate(n_total = sum(n_non, n_intensity_hostile, na.rm = TRUE))

#create column with relative number of hostile events
data_cameo_intensity<-data_cameo_intensity %>% 
  rowwise() %>% 
  mutate(n_rel = n_intensity_hostile/n_total)


##################
##conflict events:
#################


data_cameo_conflict<-data_cameo_conflict %>% pivot_wider(names_from="CAMEO_root %in% cameo_conflict", values_from="n")

#change NA to 0:
data_cameo_conflict<-data_cameo_conflict %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

#rename columns
names(data_cameo_conflict)[names(data_cameo_conflict) == "FALSE"] <- "n_non"
names(data_cameo_conflict)[names(data_cameo_conflict) == "TRUE"] <- "n_conflict"

#create column with total number of events
data_cameo_conflict<-data_cameo_conflict %>% 
  rowwise() %>% 
  mutate(n_total = sum(n_non, n_conflict, na.rm = TRUE))

#create column with relative number of hostile events
data_cameo_conflict<-data_cameo_conflict %>% 
  rowwise() %>% 
  mutate(n_rel = n_conflict/n_total)


#########################################################
############ The Cameo Trap Matrix ######################
########################################################


################
##hostile events:
###############

#keep only relevant columns for trap matrix
data_cameo_hostile<-select(data_cameo_hostile, c(Year_month,Country,n_rel))


#Create 6 Quantiles with Quantile 1 being 0 events

qlist <- list()

for(k in data_cameo_hostile$Year_month) {        
  subdata = subset(data_cameo_hostile, Year_month == k)
  subdata<-data.table(subdata)[,quantile:=cut(n_rel, breaks=quantile(unique(n_rel), probs = seq(0,1, by=0.2), na.rm=T), include.lowest = F, labels=1:5)]
  subdata$quantile <- factor(subdata$quantile, 
                              exclude = NULL, 
                              levels = c(levels(subdata$quantile), NA),
                              labels = c(levels(subdata$quantile), "0"))
  qlist[[k]] <- subdata
}


data_cameo_hostile <- do.call("rbind",qlist) 


#change format and convert NA to 1 (missings are treated as 0 events and thus being in quantile 1)
data_cameo_hostile<-dcast(data_cameo_hostile, Country ~ Year_month)
data_cameo_hostile[is.na(data_cameo_hostile)]<-0
data_cameo_hostile_tran<-subset(data_cameo_hostile,select = -c(Country))


###################
##peaceful events:
##################

#keep only relevant columns for trap matrix
data_cameo_peace<-select(data_cameo_peace, c(Year_month,Country,n_rel))


#Create 6 Quantiles with Quantile 1 being 0 events

qlist <- list()

for(k in data_cameo_peace$Year_month){        
  subdata = subset(data_cameo_peace, Year_month == k)
  subdata<-data.table(subdata)[,quantile:=cut(n_rel, breaks=quantile(unique(n_rel), probs = seq(0,1, by=0.2), na.rm=T), include.lowest = F, labels=1:5)]
  subdata$quantile <- factor(subdata$quantile, 
                             exclude = NULL, 
                             levels = c(levels(subdata$quantile), NA),
                             labels = c(levels(subdata$quantile), "0"))
  qlist[[k]] <- subdata
}


data_cameo_peace <- do.call("rbind",qlist) 


#change format and convert NA to 1 (missings are treated as 0 events and thus being in quantile 1)
data_cameo_peace<-dcast(data_cameo_peace, Country ~ Year_month)
data_cameo_peace[is.na(data_cameo_peace)]<-0
data_cameo_peace_tran<-subset(data_cameo_peace,select = -c(Country))


##########################
##hostile Intensity events:
#########################

#keep only relevant columns for trap matrix
data_cameo_intensity<-select(data_cameo_intensity, c(Year_month,Country,n_rel))


#Create 6 Quantiles with Quantile 1 being 0 events

qlist <- list()

for(k in data_cameo_intensity$Year_month) {        
  subdata = subset(data_cameo_intensity, Year_month == k)
  subdata<-data.table(subdata)[,quantile:=cut(n_rel, breaks=quantile(unique(n_rel), probs = seq(0,1, by=0.2), na.rm=T), include.lowest = F, labels=1:5)]
  subdata$quantile <- factor(subdata$quantile, 
                             exclude = NULL, 
                             levels = c(levels(subdata$quantile), NA),
                             labels = c(levels(subdata$quantile), "0"))
  qlist[[k]] <- subdata
}


data_cameo_intensity <- do.call("rbind",qlist) 


#change format and convert NA to 1 (missings are treated as 0 events and thus being in quantile 1)
data_cameo_intensity<-dcast(data_cameo_intensity, Country ~ Year_month)
data_cameo_intensity[is.na(data_cameo_intensity)]<-0
data_cameo_intensity_tran<-subset(data_cameo_intensity,select = -c(Country))


#######################
##conflict events:
######################

#keep only relevant columns for trap matrix
data_cameo_conflict<-select(data_cameo_conflict, c(Year_month,Country,n_rel))


#Create 6 Quantiles with Quantile 1 being 0 events

qlist <- list()

for(k in data_cameo_conflict$Year_month) {        
  subdata = subset(data_cameo_conflict, Year_month == k)
  subdata<-data.table(subdata)[,quantile:=cut(n_rel, breaks=quantile(unique(n_rel), probs = seq(0,1, by=0.2), na.rm=T), include.lowest = F, labels=1:5)]
  subdata$quantile <- factor(subdata$quantile, 
                             exclude = NULL, 
                             levels = c(levels(subdata$quantile), NA),
                             labels = c(levels(subdata$quantile), "0"))
  qlist[[k]] <- subdata
}


data_cameo_conflict <- do.call("rbind",qlist) 


#change format and convert NA to 1 (missings are treated as 0 events and thus being in quantile 1)
data_cameo_conflict<-dcast(data_cameo_conflict, Country ~ Year_month)
data_cameo_conflict[is.na(data_cameo_conflict)]<-0
data_cameo_conflict_tran<-subset(data_cameo_conflict,select = -c(Country))



##########################################################
############ The Probability Matrix ######################
#########################################################

###Probability Transition Matrix
#Explanation:
#System can be modeled as a Markov chain
#In discrete time Markov Chains, transitions are described in terms of probabilities
#Transition Matrix: P
#Pij contains the transition probability i->j (e.g the transition probability from Quintile 1 to 2)
#Calculate as follows:
#Count numer of times 1 follows 1, 2 follows 1 and so on
#Table with absoulte numbers
#Divide each absolute number (for instance number of times 2 follows 1) by the row sum

#Allows for First Order Transition Matrix (Markov Chain)
trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

#Allows for First Order Transition Matrix (Markov Chain) and a Transition Matrix with a Lag
Markovmatrix <- function(X,l=1){
  tt <- table(X[,-c((ncol(X)-l+1):ncol(X))] , c(X[,-c(1:l)]))
  tt <- tt / rowSums(tt)
  return(tt)
}


#create probability table and convert into data frame

#for hostile events
prob_table_hostile= trans.matrix(as.matrix(data_cameo_hostile_tran))
prob_table_hostile<-as.data.frame(prob_table_hostile)
#for peaceful events
prob_table_peace= trans.matrix(as.matrix(data_cameo_peace_tran))
prob_table_peace<-as.data.frame(prob_table_peace)
#for conflict intensity events
prob_table_intensity= trans.matrix(as.matrix(data_cameo_intensity_tran))
prob_table_intensity<-as.data.frame(prob_table_intensity)
#for conflict events
prob_table_conflict= trans.matrix(as.matrix(data_cameo_conflict_tran))
prob_table_conflict<-as.data.frame(prob_table_conflict)

prob_table_hostile

#########################################################
############ Plot Transition Matrix #####################
########################################################

#Assign saving path for Map plots
path_plot<-"~/ICEWS-Project/2. Descriptive Analysis/Plots"

#Plot 1: CAMEO EVENT TRAP for hostile events
ggplot(prob_table_hostile, aes(x = Var2, y = Var1, z = as.numeric(Freq))) +
  stat_summary_2d(bins = 20, color = "white", fun = mean) +  
  scale_fill_gradient(low="#56B1F7", high="#132B43")+
  xlab("Relative Number of Hostile Events Quintile in T+1")+ylab("Relative Number of Hostile Events Quintile in T")+
  labs(fill='Probability')+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))+
  theme_classic(base_size = 16)

ggsave(filename="Cameo_Event_Hostile.png", path=path_plot, width =10, height = 7 )


#Plot 2: CAMEO EVENT TRAP for peaceful events
ggplot(prob_table_peace, aes(x = Var2, y = Var1, z = as.numeric(Freq))) +
  stat_summary_2d(bins = 20, color = "white", fun = mean) +  
  scale_fill_gradient(low="#56B1F7", high="#132B43")+
  xlab("Relative Number of Peaceful Events Quintile in T+1")+ylab("Relative Number of peaceful Events Quintile in T")+
  labs(fill='Probability')+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))+
        theme_classic(base_size = 16)

ggsave(filename = "Cameo_Event_Peace.png", path= path_plot,width =10, height = 7 )

#Plot 3: CAMEO EVENT TRAP for hostile intensity events
ggplot(prob_table_intensity, aes(x = Var2, y = Var1, z = as.numeric(Freq))) +
  stat_summary_2d(bins = 20, color = "white", fun = mean) +  
  scale_fill_gradient(low="#56B1F7", high="#132B43")+
  xlab("Relative Number of Hostile Events Quintile in T+1")+ylab("Relative Number of Hostile Events Quintile in T")+
  labs(fill='Probability')+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=16,colour = "black"))+
        theme_classic(base_size = 16)

ggsave(filename="Cameo_Event_Intensity.png", path = path_plot,width =10, height = 7)


#Plot 4: CAMEO EVENT TRAP for conflict events
ggplot(prob_table_conflict, aes(x = Var2, y = Var1, z = as.numeric(Freq))) +
  stat_summary_2d(bins = 20, color = "white", fun = mean) +  
  scale_fill_gradient(low="#56B1F7", high="#132B43")+
  xlab("Relative Number of Conflict Events Quintile in T+1")+ylab("Relative Number of Conflict Events Quintile in T")+
  labs(fill='Probability')+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))+
  theme_classic(base_size = 16)

ggsave(filename = "Cameo_Event_Conflict.png", path = path_plot,width =10, height = 7)











































































