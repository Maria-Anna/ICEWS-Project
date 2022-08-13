#######################################Descriptive Analysis Plots: 1995-2020###############################################

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

#Set working directory

#for Clara
setwd("~/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa")
#for Maria-Anna
setwd("~/Consulting Bewaffnete Konflikte/Data")

#load data set and format

events_africa<- read.delim("events_africa.tsv",header = TRUE,sep= "\t")
events_africa$Event.Date <- as.Date(events_africa$Event.Date, format="%Y-%m-%d")
data<-events_africa

#add variables
data$Jahr<-format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 

#merge with country_cm

#Load Country data set
cm_data = fread("cm_data.csv")

#Drop all observations till 1994
cm_data$date <- as.Date(cm_data$date, format="%Y-%m-%d")
cm_data<-cm_data[date>="1995-01-01",]

#make common variable between cm_data and data for merging
#common:"%Y-%m"

cm_data$Year_month<-format(as.Date(cm_data$date, format="%Y-%m-%d"),"%Y-%m")
data$Year_month<-format(as.Date(data$Event.Date, format="%Y-%m-%d"),"%Y-%m") 

#keep relevant variables of cm_data
#relevant: ged_dummy_sb,avr_long_avrglat,gdp, population, year month, date, country_name

country_data<-cm_data[,c("ged_dummy_sb","ged_dummy_os","ged_dummy_ns","Year_month","fvp_gdp200","fvp_population200","fvp_gdppc200","country_name")]

#merge two data sets by Year_Month and Country name

#subset country_data has other key, drop duplicates
country<-country_data %>% filter(!duplicated(country_data[,c("country_name","Year_month")]))

#merge
data_extended<-left_join(data,country, by=c("Country"="country_name","Year_month"),all.x=TRUE)


###################################################Load CAMEO CODE #########################################


#Install package from github
library("remotes")
remotes::install_github("andybega/icews")
library(icews)
library(DBI)

#load CAMEO dataset and keep relevant variables
data("cameo_codes")
cameo_codes<-cameo_codes[,c("cameo_code","name","lvl0","lvl1")]

#merge with data by event text
data_extended<-merge(data_extended,cameo_codes, by.x="Event.Text", by.y="name")


########################################Number of hostile events and state-based conflict#######################

#creaste list with hostile and peaceful events
cameo_hostile<-c("17","18","19","20","13","14","15","16")
cameo_peace<-c("3","5","6","7","8")

#create new data set "data_came" with sb Boolean, number of hostile,peacefull and total events
data_cameo<-data_extended%>%group_by(Year_month,Country,ged_dummy_sb,Jahr)%>%count(lvl0 %in% cameo_hostile)%>%as.data.frame()
#chnage NA to 0
data_cameo<-data_cameo %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

#change data format
data_cameo<-data_cameo %>% pivot_wider(names_from="dummy_hostile", values_from="n")

#rename columns
names(data_cameo)[names(data_cameo) == "FALSE"] <- "n_non"
names(data_cameo)[names(data_cameo) == "TRUE"] <- "n_hostile"

#create column with total number of events
data_cameo<-data_cameo %>% 
  rowwise() %>% 
  mutate(n_total = sum(n_non, n_hostile, na.rm = TRUE))

#create column with relative number of hostile events
data_cameo<-data_cameo %>% 
  rowwise() %>% 
  mutate(n_rel = n_hostile/n_total)

############################################################Plot###########################################

#Wie verändert sich die relative Häugifkeiten an Events mit 17/18/19 über die Monate in einem Jahr
#für ein Land, mit Linien die angeben ob ein state-based Konflikt stattfindet oder nicht

data_cameo %>% filter(Country=="Somalia",Jahr=="2008") %>% 
  ggplot(aes(Year_month,n_hostile/n_total))+ geom_point()+ geom_line()+
  geom_vline(aes(xintercept = Year_month), data = ~ filter(data_cameo,Country=="Somalia",Jahr=="2008",ged_dummy_sb=="TRUE"))+
  xlab("Year-Month")+ ylab("Number of Events")+
  ggtitle("North-Eastern African Countries till January 2019")+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="black" ), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

#############################################The EVENT TRAP=TRANSITION PROBABILITY MATRIX###############################

#data_cameo<-data_cameo%>%filter(Jahr==1995| Jahr==1996| Jahr==1997| Jahr==1998| Jahr==1999| Jahr==2000, Country=="Somalia" | Country=="Uganda" | Country=="Eritrea"| Country=="South Sudan"| Country=="Kenia"| Country=="South Africa")

###Format

#keep only relevant columns
data_cameo<-data_cameo%>%select(Year_month,Country,n_rel)


###Create 5 Quantiles

qlist <- list()

for(k in data_cameo$Year_month) {        
  subdata = subset(data_cameo, Year_month == k)
  subdata$quantile =ntile(subdata$n_rel, 6)
  qlist[[k]] <- subdata
}


data_cameo <- do.call("rbind",qlist) 

#change format 
data_cameo<-dcast(data_cameo, Country ~ Year_month)
data_cameo_tran<-subset(data_cameo,select = -c(Country))


###Probability Transition Matrix
#Explanation:
#System can be modeled as a Markov chain
#In discrete time Markov Chains, transitions are described in terms of probabilities
#Transition Matrix: P
#Pij contains the transition probability i->j (e.g the transition probability from Quantile 1 to 2)
#Calculate as follows:
#Count numer of times 1 follows 1, 2 follows 1 and so on
#Table with absoulte numbers
#Divide each absolute number (for instance number of times 2 follows 1) by the row sum


trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

prob_table= trans.matrix(as.matrix(data_cameo_tran))
prob_table

###Plot Probability Transition Matrix

#probability table as data frame
prob_table<-as.data.frame(prob_table)

ggplot(prob_table, aes(x = Var1, y = Var2, z = as.numeric(Freq))) +
  stat_summary_2d(bins = 20, color = "grey", fun = mean) +  
  scale_fill_gradient(low="#56B1F7", high="#132B43")+
  xlab("Relative Number of Hostile Events Quantile in T+1")+ylab("Relative Number of Hostile Events Quantile in T")
  theme_classic()

ggsave("Transition_Event_Trap.png")









#############################################OLD####################################################

# the loop that create the list with quartile for each date
for(k in data_cameo$Year_month) {        
  subdata = subset(data_cameo, Year_month == k)
  subdata$quartile = cut(subdata$n_rel,3,labels=F)
  qlist[[k]] <- subdata
}

# have it as a df
data_cameo <- do.call("rbind",qlist) 



#########################################


##########
names(data_cameo)[names(data_cameo) == "Country"] <- "id"
data_cameo<-data_cameo %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


getTMatrix(dat = data_cameo, col_x = "1995-01", col_y = "2020-04", 
           type = "relative", num_ranks = 5, probs = TRUE,strict=FALSE)
#########
























































































