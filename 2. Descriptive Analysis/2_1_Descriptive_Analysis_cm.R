######################################Descriptive Analysis Plots: 1995-2021###############################################

#Load necessary packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(grid)
library(sf)
library(ggpmisc)
library(geometry)
library(cowplot)
library(gridBase)
library(gridGraphics)
library(data.table)
library(cshapes)
library(stargazer)
library(xtable)
library(stringr)

#Assign paths
path_data_icews_cm<-"~/ICEWS-Project/Data/data_icews_cm.csv"
path_plots<- "~/ICEWS-Project/2. Descriptive Analysis/Plots"


#Load data sets: load and format correctly
data_icews_cm<- read.csv(path_data_icews_cm)
data_icews_cm$Event.Date <- as.Date(data_icews_cm$Event.Date, format="%Y-%m-%d")

#Save as data
data<-data_icews_cm


#create African map
states<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso (Upper Volta)", "Burundi","Cape Verde", "Cameroon","Central African Republic",
          "Chad", "Comoros","Congo", "Congo, Democratic Republic of (Zaire)","Cote D'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia",
          "Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar (Malagasy)","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
          "Nigeria", "Rwanda", "Swaziland (Eswatini)","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
          "Sudan","Tanzania (Tanganyika)","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe (Rhodesia)" )

africa <- cshp(date=as.Date("2012-1-01"), useGW=TRUE)
africa <- st_as_sf(africa, sf_use_s2(FALSE))
africa <- africa[africa$country_name %in% states,]


#Count number of observations per country and year
counts_events_by_country<-data %>% group_by(Country,year) %>% count(sort=TRUE)

#count number of events by year
counts_events_by_year<-data %>% group_by(year) %>% count(sort=TRUE)


#Count average number of observationy per country over the years
average_events_by_country<- data %>% group_by(Country,year) %>% count() %>% ungroup (year) %>% summarise(mean(n)) 


#create median
median<- data %>% group_by(Country, year) %>% count() %>% ungroup (Country) %>% summarise(median(n))  
average_median<-mean(median$`median(n)`)

#Rename columns for dataset counts_events_by_country
colnames(counts_events_by_country)[which(names(counts_events_by_country) == "n")] <- "count"
counts_events_by_country$count<-as.numeric(counts_events_by_country$count)
#Rename columns for dataset average_events_by_country
colnames(average_events_by_country)[which(names(average_events_by_country) == "mean(n)")] <- "mean"


#------------------------------------------------------------------------------------------------------------------------------------------------------
#############################
# Summary Statistics Overall
############################

stargazer(data, summary.stat = c("mean", "median", "min", "max", "sd", "p25", "p75"))


#Generate table with missings for latex
data_icews_cm_na <-as.data.frame(colSums(is.na(data)))
#print(xtable(data_icews_cm_na, type = "latex"), file = paste(path_plots,"/missings_data_icews_cm.tex", sep=""))

#Check for duplicates in data set


#For Event.ID
sum(duplicated(data$Event.ID))
#Remark:
#The Event.ID is a non-unique identifier --> the event ID can be repeated in the data set (See also ICEWS Coded Event Data Read Me)


#For Story.ID
table(duplicated(data$Story.ID))
#Remark:
#From the same event different information of a story can be extracted


#Generate duplicate data set regarding: Event.ID
duplicates_event_id <- data %>%
  group_by(Event.ID) %>%
  filter(n()>1)


#Generate duplicate data set regarding Story.ID
duplicates_story_id <- data %>%
  group_by(Story.ID) %>%
  filter(n()>1)


#CAMEO Root Code Freq
cameo_freq<-data %>% group_by(CAMEO_root) %>% count(sort=TRUE)

#Export as latex document
#print(xtable(cameo_freq, type = "latex"), file = paste(path_plots,"/cameo_freq.tex", sep=""))

#--------------------------------------------------------------------------------------------------------------
###############################
## Frequency Analysis #########
###############################

#Lineplot: Evolution Plot

#Evolution of number of events for every year 1995-2020 and across all countries
#Lineplot with Number of events for each year
counts_events_by_year%>%
  ggplot(aes(year,n,group=1))+ geom_point()+ geom_line()+
  stat_peaks(colour = "#440154", shape=1, size=6) +
  xlab("Year")+ ylab("Number of Events")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 125000),breaks=seq(0,120000,30000), labels = seq(0,120000,30000))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave("Evolution_Events.png", path=path_plots, width = 15, height=9)


#--------------------------
#Lineplot: Relative Frequency of Events per country per year

#If interested only in one specific Country, run line 92 and 99
#country<-"Somalia"

#If only interested in north eastern counties
states_africa_north_east<-c("Ethiopia","Somalia","Kenya","South Sudan","Eritrea", "Uganda")


##If interested in specific months replace counts_events_by_country in line 97 with counts_events_by_country_month
#and run additionaly line 96 and 100 
#counts_events_by_country_month<-data %>% group_by(Country,Year_month) %>% count(sort=TRUE)

counts_events_by_country %>% 
  filter(Country %in% states_africa_north_east) %>%
  #filter(Year_month>="2017-06"& Year_month<="2019-01" & Country == country)%>%
  ggplot(aes(x=year,y=count/sum(count),group=Country,color=Country))+ geom_point()+ geom_line()+
  xlab("Year")+ ylab("Relative Frequency")+
  ggtitle("Relative Frequency of Events")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave(filename="Evolution_Events_Country.png", width = 15, height=9, path=path_plots) 


#---------------------
#Dotplot: Averange Frequency


#Average Frequency of observations for every country accross all years 1995-2021
average_events_by_country%>%arrange(mean)%>%mutate(Country=factor(Country,levels = Country))%>%
  ggplot(aes(y=Country, x=mean)) +
  geom_point(size=1)+
  geom_vline(aes(xintercept =average_median,color="Average Median"),size=0.5)+
  ylab(label="Country")+xlab(label="Average Event Number between 1995 and 2020")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.grid.major.y = element_line(colour = "lightgrey"),
        panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")
        ) +
  theme(legend.position = c(.9,.1),
        legend.title=element_blank(),
        legend.text=element_text(size=16),
        legend.key.size=unit(1.5,"lines"))+
  scale_x_continuous( breaks = c(0,1000,2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000,13000))+
  scale_color_manual(values="#440154")
  
ggsave("Average_Number_Events.png", path= path_plots, width = 15, height = 9) 

#-----------------------
#Barplot: Difference of Events over Time

#Define Country of Interest
country<- "Somalia"

#Keep only events in Country of interest, order them by year in an increasing order
#and calculate the difference of events in the previus year
events_country<-counts_events_by_country %>% filter(Country== country)
events_country<-events_country[order(events_country$year),]
events_country$dif<-c(0,diff(events_country$count))

#Barplot 
ggplot(events_country, aes(y=dif, x=year))+geom_col()+
  ggtitle(paste("Difference of Events over Time in", country, sep=" "))+
  ylab(label="Difference")+xlab(label="Year")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave("Difference_of_Events_over_Time.png", path= path_plots, width = 15, height = 9) 


#------------------------
#Map with Bins

#Define year of interest
#For a Plot over all years replace  filter(data, year==Year) in line 123 with data
Year<-2019

#Map with all events that took place in a specific year, one bin can contain more than one event
ggplot(data=africa) + 
    geom_sf(col = "black", alpha = 0.00001) + 
    geom_bin2d(data= dplyr::filter(data, year== Year), aes(x = Longitude, y = Latitude), bins=150) + 
    xlab("Latitude") + ylab("Longitude") + 
    ggtitle(paste("Events in Africa: Density of Events in",Year))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))+
    scale_fill_gradient2(low="deepskyblue", mid="dodgerblue3", high = "firebrick1")
  

ggsave(filename=paste("Event_Density_Africa_",Year,".png", sep=""),path= path_plots, width = 9, height = 9) 
  



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   
###############################
### CAMEO CODE Analysis  ######
###############################
  
#Define Country of Interest
country<-"Somalia"
#Define tree years to compare the Cameo Codes between them
year<-c(2005, 2006, 2007)

#Create 3 Barplots with the Absolute Frequency of the cameo codes for 3 years in a specific Country
#For the relative Frequency 
#replace geom_bar(stat = "count",fill="darkblue") with geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")
# in lines  177, 192, 2017 

#Barplot for first year
p<-data%>%filter(Country=="Somalia" & year==2005) %>% ggplot(aes(x=as.character(CAMEO_root)))+
    geom_bar(stat = "count",fill="#440154")+
    xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0,800), breaks=seq(0,800,100))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(country,year[1]))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
p
#Barplot for second year  
q<-data%>%filter(Country=="Somalia" & year==2006) %>%ggplot(aes(x=as.character(CAMEO_root)))+
    geom_bar(stat="count",fill="#440154")+
    xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 800), breaks=seq(0,800,100))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(country,year[2]))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))

#Barolot for third year  
d<-data%>%filter(Country=="Somalia" & year==2007) %>%ggplot(aes(x=as.character(CAMEO_root)))+
    geom_bar(stat="count",fill="#440154")+
    xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 800), breaks=seq(0,800,100))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(country,year[3]))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
          
plot_grid(p,q,d,ncol = 3, align="hv")

ggsave(filename=paste("Root_Cameo_Code_Evolution_Absolute_",country,".png", sep=""), path=path_plots, width = 15, heigh=9) 
  
#----------------------------------  
#Pie Chart for Cameo Codes 

#Count frequency of Cameo Codes
cameo_freq<-data %>% group_by(Country,year,CAMEO.Code,CAMEO_root,Event.Text) %>% count(sort=TRUE)
cameo_freq_month<-data %>% group_by(Country,year,CAMEO.Code,CAMEO_root,Event.Text, month) %>% count(sort=TRUE)

#Define a specific Country
country<-"Somalia"
#Define a specific Cameo Code
cameo<-18
  
cameo_freq %>% filter(Country== country &CAMEO_root==cameo) %>% 
    ggplot(aes(x="", y=n ,fill=Event.Text))+
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    theme_void()+
    labs(fill="Cameo Code 18")+
    ggtitle(paste("Frequencies of Cameo Code 18 in",country))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank())
  
ggsave(filename=paste("Cameo_Code",cameo, country,".png", sep=""), path=path_plots, width = 15, height = 9) 
  



#---------------------------------------------------------------------------------------------------------------------------------------------------------

################################
### Intensity Analysis #########
################################


states_africa_north_east<-c("Ethiopia","Somalia","Kenya","South Sudan","Eritrea", "Uganda")
north_eastern_africa <- data%>% filter(Country %in% states_africa_north_east)

#dsitribution of intensity in these countries 2016-2018
Year<-2019

north_eastern_africa %>% filter(year==Year& Country %in% states_africa_north_east) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle(paste("Intensity Comparison in", Year, sep=" "))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


ggsave(filename=paste("Distribution_intensity",Year,".png", sep=""), path=path_plots, width = 15, heigh=9) 

#------------------
#Barplot Intensity

#Define Country of Interest
country<- "Somalia"

#Define Year of Interest
Year<-2019

#Barplot Absolute Frequency of the Intensity of Events in a specific year, in a specific Country
data%>%filter(Country==country & year==Year)%>%
  ggplot(aes(x=as.character(Intensity)))+
  geom_bar(stat="count",fill="darkblue")+
  xlab("Intensity")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  ggtitle(paste("Intensity of Events in",country, Year, sep=" ") )+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


ggsave(filename=paste("Intensity_of_Events_in_",country, Year,".png", sep=""), path=path_plots, width = 15, heigh=9) 

#CAMEO Root Frequency Table
library("remotes")
remotes::install_github("andybega/icews")
library(icews)
library(DBI)
data("cameo_codes")
cameo_codes<-cameo_codes[,c("cameo_code","name","lvl0","lvl1")]
cameo_codes<- cameo_codes %>% filter(cameo_code=="01" |cameo_code=="02"| cameo_code=="03"| cameo_code=="04"| cameo_code=="05"| cameo_code=="06"| cameo_code=="07"| cameo_code=="08"| cameo_code=="09"| cameo_code=="10" |
                                       cameo_code=="11"| cameo_code=="12"| cameo_code=="13"| cameo_code=="14"| cameo_code=="15"| cameo_code=="16"| cameo_code=="17" | cameo_code=="18" | cameo_code=="19" | cameo_code=="20")
data<-merge(data,cameo_codes, by.x="CAMEO_root", by.y="lvl0")

#generate CAMEO freq table
cameo_freq<-data %>% group_by(CAMEO_root,name) %>% count(sort=TRUE)

#change capital letters to small letters
cameo_freq$name<-tolower(cameo_freq$name)
cameo_freq$name<-str_to_title(cameo_freq$name)

#save table
print(xtable(cameo_freq, type = "latex"), file = "cameo_freq.tex",include.rownames=FALSE)

#Intensity Frequency Table
int_freq<-data %>% group_by(Intensity) %>% count(sort=TRUE)
bins<-c(-10,-0.1,0,0.1,10)
str(int_freq$Intensity)
int_freq$Intensity<- cut(int_freq$Intensity, breaks = bins,include.lowest = T)
int_freq_sum <- int_freq %>% group_by(Intensity) %>% summarise(Frequency = sum(n))
print(xtable(int_freq_sum, type = "latex"), file = "int_freq.tex")


