######################################Descriptive Analysis Plots: 1995-2020###############################################

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

#Load data sets: load and format correctly

events_africa_total <- read.delim("events_africa_total.tsv",header = TRUE,sep= "\t")
events_africa<- read.delim("events_africa.tsv",header = TRUE,sep= "\t")
events_target_and_source_is_africa<-read.delim("events_target_and_source_is_africa.tsv",header = TRUE,sep= "\t")
events_target_or_source_is_africa<-read.delim("events_target_or_source_is_africa.tsv",header = TRUE,sep= "\t")

events_africa_total$Event.Date <- as.Date(events_africa_total$Event.Date, format="%Y-%m-%d")
events_africa$Event.Date <- as.Date(events_africa$Event.Date, format="%Y-%m-%d")
events_target_and_source_is_africa$Event.Date <- as.Date(events_target_and_source_is_africa$Event.Date, format="%Y-%m-%d")
events_target_or_source_is_africa$Event.Date <- as.Date(events_target_or_source_is_africa$Event.Date, format="%Y-%m-%d")

#Save as data

data<-events_africa_total
data<-events_africa

#Prepare for loop

data$Jahr<-format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 
Jahre<-seq(1995, 2020, by= 1)
states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo","Congo, DRC", "Democratic Republic of Congo", "Congo, Democratic Republic of the","Cote d'Ivoire","Republic of Cote d'Ivoire", "DRC Cote d'Ivoire",
                 "Congo, Republic of the Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya",
                 "Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria","Ruanda", "Rwanda", "Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan","Swaziland","Tanzania","The Gambia","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe" )


#create african map

world_map <- ne_countries(scale = "small", returnclass = "sf")
africa <- world_map %>% filter(continent %in% "Africa")

#Count number of observations per country and year

counts_events_by_country<-data %>% group_by(Country,Jahr) %>% count(sort=TRUE)

#count number of events by year

counts_events_by_year<-data %>% group_by(year) %>% count(sort=TRUE)


#Count average number of observationy per country over the years

average_events_by_country<- data %>% group_by(Country,year) %>% count() %>% ungroup (year) %>% summarise(mean(n)) 


#create median

median<- data %>% group_by(Country,year) %>% count() %>% ungroup (Country) %>% summarise(median(n))  
average_median<-mean(median$`median(n)`)

#add column (in total 25 columns) whether country above median or below median event number in the specific year

for(i in Jahre) {
countries_danger<-counts_events_by_country%>%filter(Jahr==i)%>%group_by(Country)%>% filter(n>median[median$Jahr==i,2])
countries_danger<-countries_danger$Country
danger_median<-ifelse(data$Country %in% countries_danger, 1, 0)
data[,ncol(data)+1]<-as.factor(danger_median)
colnames(data)[ncol(data)]<-paste0("danger_median_",i)
}


#Assign Quarter Value
data$Event.Quarter <- paste0(quarter(data$Event.Date))


###############################Geographical Plots#########################

#Evolution of number of events across time 1995-2020

counts_events_by_year%>%
  ggplot(aes(year,n,group=1))+ geom_point()+ geom_line()+
  xlab("Year")+ ylab("Number of Events")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=12,colour = "black"))


counts_events_by_year%>%
  ggplot(aes(year,n,group=1))+ geom_point()+ geom_line()+
  xlab("Year")+ ylab("Number of Events")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave("Evolution_Events.png")


#Events on the African Continent 1995-2020

for(i in 2019){
  
   ggplot(data=africa) + 
   geom_sf(col = "black", alpha = 0.00001) + 
   geom_point(data= dplyr::filter(data, Jahr== i), aes(x = Longitude, y = Latitude), col = "red", size=0.5, fill=20) + 
   xlab("Latitude") + ylab("Longitude") + 
   ggtitle(paste("Number of events on the African continent in",i))+
   theme(plot.title = element_text(color = "black", size=20, hjust=0.5),
         axis.title.x = element_text(hjust=0.5),
         axis.title.y= element_text(hjust=0.5)) 
  
  ggsave(filename=paste("Events in Africa_",i,".png", sep="")) 
  
}

ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data=data, aes(x = Longitude, y = Latitude), col = "red", size=0.000000000000001, fill=20) + 
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle("Number of events on the African continent in")+
  theme(plot.title = element_text(color = "black", size=20, hjust=0.5),
        axis.title.x = element_text(hjust=0.5),
        axis.title.y= element_text(hjust=0.5)) 

events_in_africa<-st_join(data,africa, left=F)


counts_events_by_country %>% filter(Jahr==2019) %>%arrange(sort(n,decreasing=FALSE))

#Events on the African Continent 1995-2020: Comparison between Countries with many Events to few Events

for(i in Jahre){

p<-ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]==1),aes(x = Longitude, y = Latitude), col = "red", size=0.5, fill=20) + 
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle(paste("Events>Median in",i))+
  theme( plot.title = element_text(color = "black", size=14, hjust=0.5),
         axis.title.x = element_text(hjust=0.5),
         axis.title.y= element_text(hjust=0.5)) 

q<-ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data= dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]==0) ,aes(x = Longitude, y = Latitude), col = "red", size=0.5, fill=20) + 
  xlab("Latitude") + ylab("Longitude") +
  ggtitle(paste("Events<Median in",i))+
  theme( plot.title = element_text(color = "black", size=14, hjust=0.5),
         axis.title.x = element_text(hjust=0.5),
         axis.title.y= element_text(hjust=0.5)) 

ggsave(plot=plot_grid(p,q,ncol = 2, align="hv"),filename=paste("Events_Comparison_",i,".png", sep="")) 

}


#Frequency of Latitude and Longitude 1995-2020

for(i in Jahre){
  
ggplot(data= dplyr::filter(data, Jahr== i)) + geom_density(aes(Latitude, fill="Latitude"), alpha=0.2) +
  geom_density(aes(Longitude, fill="Longitude"), alpha=0.2) + 
  scale_fill_manual(name = "Geographic Coordinate", values = c(Latitude = "red", Longitude = "green"))+
  xlab(label = "Geographic Coordinate")+
  ylab(label="Density")

ggsave(filename=paste("Density_Latitude_Longitude_",i,".png", sep="")) 

}


#Frequency of observations in each country 1995-2020

colnames(counts_events_by_country)[which(names(counts_events_by_country) == "n")] <- "count"
counts_events_by_country$count<-as.numeric(counts_events_by_country$count)

for(i in 2019){

counts_events_by_country%>%arrange(count)%>%mutate(Country=factor(Country,levels = Country))%>%filter(Jahr==i)%>%
ggplot(aes(y=Country, x=count)) +
    geom_point(size=1)+
    geom_vline(xintercept =771.5,linetype="dotted", color="red")+
    ggtitle(paste("Absolute Number of Events in",i))+
  ylab(label="Country")+xlab(label="Absolute Number of Events")+
    theme( plot.title = element_text(color = "black", size=14, hjust=0.5))

ggsave(filename=paste("Number_Events_Countries_",i,".png", sep="")) 

}


#Average Frequency of observations by country from 1995-2010

colnames(average_events_by_country)[which(names(average_events_by_country) == "mean(n)")] <- "mean"

#with label
average_events_by_country%>%arrange(mean)%>%mutate(Country=factor(Country,levels = Country))%>%
ggplot(aes(x=mean, y=Country)) +
  geom_point(size=1)+
  geom_vline(aes(xintercept =average_median,color="median"),linetype="dotted")+
  ylab(label="Country")+xlab(label="Average Event Number between 1995-2020")+
  scale_color_manual(values = c(median= "red"), labels="Median")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=12,colour = "black"),
        legend.position = c(0.9,0.5),
        legend.title = element_blank(),
        legend.key.size = unit(1.5, 'cm'),
        legend.background = element_blank(),
        legend.text=element_text(size=12))

ggsave("Average_Number_Events_label.png") 

#without label
average_events_by_country%>%arrange(mean)%>%mutate(Country=factor(Country,levels = Country))%>%
  ggplot(aes(x=mean, y=Country)) +
  geom_point(size=1)+
  geom_vline(xintercept =average_median,linetype="dotted", colour="red")+
  ggtitle("Average Number of Events between 1995 and 2020")+
  ylab(label="Country")+xlab(label="Average Number of Events between the Years")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=12,colour = "black"))

ggsave("Average_Number_Events.png") 



#Density of events in Africa 1995-2020

for(i in 2019){
  
ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_bin2d(data= dplyr::filter(data, Jahr== i), aes(x = Longitude, y = Latitude), bins=150) + 
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle(paste("Events in Africa: Density of Events in",i))+
  theme( plot.title = element_text(color = "black", size=14, hjust=0.5),
         axis.title.x = element_text(hjust=0.5),
         axis.title.y= element_text(hjust=0.5)) +
  scale_fill_gradient2(low="deepskyblue", mid="dodgerblue3", high = "firebrick1")

ggsave(filename=paste("Event_Density_Africa_",i,".png", sep="")) 
  
}

#What are 





##Distribution in count data/density of the intensity numbers for countries with many and few event

for (i in Jahre) {
p<- ggplot(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]), aes(x = Intensity, colour==data[[paste0("danger_median_",i)]])) + 
  geom_histogram(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]),position="dodge", bins=50, fill="transparent") + 
  scale_color_manual(name="Number Events", labels = c("Low", "High"), values=c("red", "darkgreen"))+
  xlab("Intensity") + ylab("Absolute Frequency")

q<-ggplot(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]),aes(x = Intensity, colour=data[[paste0("danger_median_",i)]])) +
  geom_density(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]),aes(colour=data[[paste0("danger_median_",i)]])) +
  scale_color_manual(name="Number Events", labels = c("Low", "High"), values=c("red", "darkgreen"))+
  xlab("Intensity") + ylab("Density")

ggsave(plot=plot_grid(p,q,ncol = 2, align="hv"),filename=paste("Intensity_Country_Median",i,".png", sep="")) 

}

#Events in First Quarter compared to last Quarter

for(i in Jahre){

p<-ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data=dplyr::filter(data, Jahr== i & Event.Quarter==min(Event.Quarter)), aes(x = Longitude, y = Latitude, colour = Event.Quarter), size=0.9) + 
  scale_color_manual(name="Event Quarter", labels = c("First Quarter"), values=c("red"))+
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle(paste("Events in Africa: First Quarter",i))+
  theme( plot.title = element_text(color = "black", size=14, hjust=0.5),
         axis.title.x = element_text(hjust=0.5),
         axis.title.y= element_text(hjust=0.5)) 

q<-ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data=dplyr::filter(data, Jahr== i & Event.Quarter==max(Event.Quarter)), aes(x = Longitude, y = Latitude, colour = Event.Quarter), size=0.9) + 
  scale_color_manual(name="Event Quarter", labels = c("Last Quarter"), values=c("red"))+
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle(paste("Events in Africa: Last Quarter",i))+
  theme( plot.title = element_text(color = "black", size=14, hjust=0.5),
         axis.title.x = element_text(hjust=0.5),
         axis.title.y= element_text(hjust=0.5)) 

ggsave(plot=plot_grid(p,q,ncol = 2, align="hv"),filename=paste("Events_Quarter_Comparison_",i,".png", sep="")) 

}

###################################################### CAMEO CODE ########################################################

#Brainstorming

#number of missings in Country variables
sum(is.na(data$Country)) #0

#create variable equal=True if the same source, target and country
data$Equal<-ifelse(data$Source.Country == data$Target.Country &  data$Target.Country == data$Country,TRUE,FALSE)

##extract root codes from specific CAMEO event codes

#Install package from github
library("remotes")
remotes::install_github("andybega/icews")
library(icews)
library(DBI)

#load CAMEO dataset and keep relevant variables
data("cameo_codes")
cameo_codes<-cameo_codes[,c("cameo_code","name","lvl0","lvl1")]


#merge with data by Event Text
data<-merge(data,cameo_codes, by.x="Event.Text", by.y="name")


############################################Focus on North-Eastern African Countries#########################################

#Countries:Eritrea, Ethiopia,Uganda,Somalia,Kenya,South Sudan
#State Based Conflicts in 2018 at country level: Somalia, South Sudan, Ethiopia*, Kenya*
#No State Based Conflict in 2018 at country level: Eritrea, Uganda


#prepare for loop
states_africa_north_east<-c("Eritrea", "Ethiopia","Uganda","Somalia","Kenya","South Sudan")

#compare absolute frequency of Root Cameo Code between 2016 and 2018 in each country (Brandt et al. 2022)

for(i in states_africa_north_east){

p<-data%>%filter(Country==i & Jahr=="2016") %>% ggplot(aes(x=as.character(lvl0)))+
  geom_bar(stat = "count",fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle(paste(i,"(2016)"))+
  theme(plot.title = element_text(hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


q<-data%>%filter(Country==i & Jahr=="2017") %>%ggplot(aes(x=as.character(lvl0)))+
  geom_bar(stat="count",fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle(paste(i,"(2017)"))+
  theme(plot.title = element_text(hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

d<-data%>%filter(Country==i & Jahr=="2018") %>%ggplot(aes(x=as.character(lvl0)))+
  geom_bar(stat="count",fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle(paste(i,"(2018)"))+
  theme(plot.title = element_text( hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggsave(plot=plot_grid(p,q,d,ncol = 3, align="hv"),filename=paste("Root_Cameo_Code_Evolution_Absolute_",i,".png", sep="")) 


}

data%>%filter(Country=="Somalia" & Jahr=="2007") %>% ggplot(aes(x=as.character(lvl0)))+
  geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle(paste(i,"(2018)"))+
  theme(plot.title = element_text( hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))




#compare relative frequency of Root Cameo Code between 2016 and 2018 in each country (Brandt et al. 2022)


for(i in "Somalia"){
  
  p<-data%>%filter(Country==i & Jahr=="2016") %>% ggplot(aes(x=as.character(lvl0)))+
    geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
    xlab("Root Cameo Code")+ ylab("Relative Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0,NA))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(i,"(2016)"))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  
  q<-data%>%filter(Country==i & Jahr=="2017") %>%ggplot(aes(x=as.character(lvl0)))+
    geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
    xlab("Root Cameo Code")+ ylab("Relative Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(i,"(2017)"))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  d<-data%>%filter(Country==i & Jahr=="2018") %>%ggplot(aes(x=as.character(lvl0)))+
    geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
    xlab("Root Cameo Code")+ ylab("Relative Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(i,"(2018)"))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size=5),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  ggsave(plot=plot_grid(p,q,d,ncol = 3, align="hv"),filename=paste("Root_Cameo_Code_Evolution_Relative_",i,".png", sep="")) 
  
  
}

#compare CAMEO CODE before sb conflict 19-05, 18-04, 18.06


p<-data%>%filter(Country=="Ethiopia" & Month=="18-07") %>%ggplot(aes(x=as.character(lvl0)))+
  geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle("Uganda 18-04")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

q<-data%>%filter(Country=="Ethiopia" & Month=="18-08") %>%ggplot(aes(x=as.character(lvl0)))+
  geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle("Uganda 18-05")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  
d<-data%>%filter(Country=="Ethiopia" & Month=="18-09") %>%ggplot(aes(x=as.character(lvl0)))+
  geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle("Uganda 18-06")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
ggsave(plot=plot_grid(p,q,d,ncol = 3, align="hv"),filename=paste("Uganda_Evolution_Relative_",i,".png", sep="")) 


#compare evolution of events in each country (Mueller and Rauh 2022)


#seperated for each country
#time span: 1995-2020

counts_events_by_country %>% filter(Country=="Somalia") %>% 
  ggplot(aes(Jahr,n,group=1))+ geom_point(colour="darkgreen")+ geom_line(colour="darkgreen")+
  stat_peaks(colour = "black",shape="circle open") +
  xlab("Year")+ ylab("Number of Events")+
  ggtitle("Somalia")+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="black" ), 
        axis.line = element_line(colour = "black"))

#all countries in one plot
#time span on yearly level: 1995-2020

counts_events_by_country %>% filter(Country %in% states_africa_north_east) %>% 
  ggplot(aes(x=Jahr,y=stat(n)/sum(n),group=Country,color=Country))+ geom_point()+ geom_line()+
  xlab("Year")+ ylab("Number of Events")+
  ggtitle("North-Eastern African Countries")+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="black" ), 
        axis.line = element_line(colour = "black"))

ggsave(filename="Evolution_Events_1995_2020.png") 

#all countries in one plot
#time span on monthly level: 2016-November 2018

data$Month<-format(as.Date(data$Event.Date, format="%Y-%m-%d"),"%y-%m") 
counts_events_by_country_month<-data %>% group_by(Country,Month) %>% count(sort=TRUE)


counts_events_by_country_month %>% filter(Month>="17-06"& Month<="19-01" & Country %in% states_africa_north_east) %>% 
  ggplot(aes(Month,n,group=Country,color=Country))+ geom_point()+ geom_line()+
  geom_vline(xintercept ="18-06",linetype="dotted", color="red")+
  stat_peaks(colour = "black",shape="circle open",size=2,aes(colour=Country)) +
  xlab("Year-Month")+ ylab("Number of Events")+
  ggtitle("North-Eastern African Countries till January 2019")+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="black" ), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

ggsave(filename="Evolution_Events_2017_2019.png") 


#pie chart for each country with subcategories of cameo root code in 2017 and 2018 (Salam et al. 2020)

cameo_freq<-data %>% group_by(Country,Jahr,CAMEO.Code,lvl0,lvl1,Event.Text) %>% count(sort=TRUE)
cameo_freq_month<-data %>% group_by(Country,Jahr,CAMEO.Code,lvl0,lvl1,Event.Text, Month) %>% count(sort=TRUE)

for(i in states_africa_north_east){
  
cameo_freq %>% filter(Country==i & Jahr==2017 & lvl0==18) %>% 
  ggplot(aes(x="", y=n ,fill=Event.Text))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Cameo Code 18")+
  ggtitle(paste("Frequencies of Cameo Code 18 in",i,"2017"))+
  theme(plot.title = element_text(hjust = 0.5))

  ggsave(filename=paste("Cameo_Code_18_2017_",i,".png", sep="")) 
  
}

#pie chart for 2017 and Months 01-06.2018

#Somalia
p<-cameo_freq %>% filter(Country=="Somalia" & Jahr==2017 & lvl0==18) %>% 
  ggplot(aes(x="", y=n ,fill=Event.Text))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Cameo Code 18")+
  ggtitle(paste("Cameo Code 18 Somalia 2017"))+
  theme(plot.title = element_text(hjust = 0.5))
q<-cameo_freq_month %>% filter(Country=="Somalia" & Month>="18-01" & Month<="18-06" & lvl0==18) %>% 
  ggplot(aes(x="", y=n ,fill=Event.Text))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Cameo Code 18")+
  ggtitle(paste("Cameo Code 18 Somalia 18-01 till 18-06"))+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(plot=plot_grid(p,q,ncol = 2),filename=paste("Cameo_Code_18_Pie_Somalia",i,".png", sep="")) 

#Uganda
p<-cameo_freq %>% filter(Country=="Uganda" & Jahr==2017 & lvl0==18) %>% 
  ggplot(aes(x="", y=n ,fill=Event.Text))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Cameo Code 18")+
  ggtitle(paste("Cameo Code 18 Uganda 2017"))+
  theme(plot.title = element_text(hjust = 0.5))
q<-cameo_freq_month %>% filter(Country=="Uganda" & Month>="18-01" & Month<="18-06" & lvl0==18) %>% 
  ggplot(aes(x="", y=n ,fill=Event.Text))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Cameo Code 18")+
  ggtitle(paste("Cameo Code 18 Uganda 18-01 till 18-06"))+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(plot=plot_grid(p,q,ncol = 2, align = "hv"),filename=paste("Cameo_Code_18_Pie_Uganda",i,".png", sep="")) 





####For Analysis: keep data only if North Eastern country


north_eastern_africa <- data%>% filter(Country %in% states_africa_north_east)
states_africa_north_east_danger<-c("Ethiopia","Somalia","Kenya","South Sudan")
states_africa_north_east_no_danger<-c("Eritrea", "Uganda")



#dsitribution of intensity in these countries 2016-2018

#in 2016
p<-north_eastern_africa %>% filter(Jahr==2016 & Country %in% states_africa_north_east_no_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2016")

p1<-north_eastern_africa %>% filter(Jahr==2016 & Country %in% states_africa_north_east_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2016")


#in 2017

q<-north_eastern_africa %>% filter(Jahr==2017 & Country %in% states_africa_north_east_no_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2017")
q1<-north_eastern_africa %>% filter(Jahr==2017  & Country %in% states_africa_north_east_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2017")


#in 2018

d<-north_eastern_africa %>% filter(Jahr==2018 & Country %in% states_africa_north_east_no_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2018")
d1<-north_eastern_africa %>% filter(Jahr==2018  & Country %in% states_africa_north_east_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2018")

grid.arrange(p,q,d,p1,q1,d1, ncol=3,nrow=2)


#duplicates role: same story ID but varying in othervariables

#in which year the highest number of duplicates
north_eastern_africa %>% group_by(Jahr) %>%
  filter(duplicated(Story.ID)) 

#in which country the highest number of duplicates
north_eastern_africa %>% group_by(Country) %>%
  filter(duplicated(Story.ID)) 


#Pie Chart for CAMEO ROOT Frequency
data("cameo_codes")
cameo_codes<-cameo_codes[,c("cameo_code","name","lvl0","lvl1")]
cameo_codes<- cameo_codes %>% filter(cameo_code=="01" |cameo_code=="02"| cameo_code=="03"| cameo_code=="04"| cameo_code=="05"| cameo_code=="06"| cameo_code=="07"| cameo_code=="08"| cameo_code=="09"| cameo_code=="10" |
                                    cameo_code=="11"| cameo_code=="12"| cameo_code=="13"| cameo_code=="14"| cameo_code=="15"| cameo_code=="16"| cameo_code=="17" | cameo_code=="18" | cameo_code=="19" | cameo_code=="20")
data<-merge(data,cameo_codes, by.x="CAMEO_root", by.y="lvl0")


cameo_freq<-data %>% group_by(CAMEO_root,name) %>% count(sort=TRUE)
#plot
library(RColorBrewer)
colourCount <- length(unique(data$name)) # number of levels
getPalette <- colorRampPalette(brewer.pal(9, "Set2"))
cameo_freq %>%
    ggplot(aes(x="", y=n, fill=name))+
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    theme_void()+
    labs(fill="CAMEO Root Code")+
    scale_fill_manual(values = getPalette(colourCount)) +
    theme(plot.title = element_text(hjust = 0.5))
#table
print(xtable(cameo_freq, type = "latex"), file = "cameo_freq.tex")


#Pie Chart for Intensity Frequency
int_freq<-data %>% group_by(Intensity) %>% count(sort=TRUE)
int_freq %>%
  ggplot(aes(x="", y=n, fill=Intensity))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="CAMEO Root Code")+
  scale_fill_manual(values = getPalette(colourCount)) +
  theme(plot.title = element_text(hjust = 0.5))
#table with intensity range
bins<-c(-10,-0.1,0,0.1,10)
str(int_freq$Intensity)
int_freq$Intensity<- cut(int_freq$Intensity, breaks = bins,include.lowest = T)
int_freq_sum <- int_freq %>% group_by(Intensity) %>% summarise(Frequency = sum(n))
print(xtable(int_freq_sum, type = "latex"), file = "int_freq.tex")









