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


path_events_africa<-"~/ICEWS Project/Data/data_icews_cm.csv"
path_plots<- "~/ICEWS-Project/2. Descriptive Analysis/Plots"


#Load data sets: load and format correctly
events_africa<- read.csv(path_events_africa)
events_africa$Event.Date <- as.Date(events_africa$Event.Date, format="%Y-%m-%d")

#Save as data
data<-events_africa


#Drop not correctly geocoded observations
false_coded<-data %>% filter( Longitude < -50 | Longitude > 64 | Latitude> 40)
data<- filter(data, !Event.ID %in% false_coded$Event.ID)


#Prepare for loop
data$Jahr<-format(as.Date(events_africa$Event.Date, format="%Y-%m-%d"),"%Y") 
Jahre<-seq(1995, 2021, by= 1)
states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo","Congo, DRC", "Democratic Republic of Congo", "Congo, Democratic Republic of the","Cote d'Ivoire","Republic of Cote d'Ivoire", "DRC Cote d'Ivoire",
                 "Congo, Republic of the Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya",
                 "Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria","Ruanda", "Rwanda", "Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan","Swaziland","Tanzania","The Gambia","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe" )


#create african map
states<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso (Upper Volta)", "Burundi","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo", "Congo, Democratic Republic of (Zaire)","Cote D'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia",
                 "Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar (Malagasy)","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria", "Rwanda", "Swaziland (Eswatini)","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan","Tanzania (Tanganyika)","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe (Rhodesia)" )

africa <- cshp(date=as.Date("2012-1-01"), useGW=TRUE)
africa <- st_as_sf(africa, sf_use_s2(FALSE))
africa <- africa[africa$country_name %in% states,]


#Count number of observations per country and year

counts_events_by_country<-data %>% group_by(Country,Jahr) %>% count(sort=TRUE)

#count number of events by year

counts_events_by_year<-data %>% group_by(Jahr) %>% count(sort=TRUE)


#Count average number of observationy per country over the years

average_events_by_country<- data %>% group_by(Country,Jahr) %>% count() %>% ungroup (Jahr) %>% summarise(mean(n)) 


#create median

median<- data %>% group_by(Country,Jahr) %>% count() %>% ungroup (Country) %>% summarise(median(n))  
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

#Evolution of number of events across time 1995-2021

counts_events_by_year%>%
  ggplot(aes(Jahr,n,group=1))+ geom_point()+ geom_line()+
  xlab("Year")+ ylab("Number of Events")+
  ggtitle("Evolution of Event Numbers Across Time")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave("Evolution_Events.png", path=path_plots)

#Beispielplot
#i<-2019

#Events on the African Continent 1995-2021

for(i in Jahre){
   print(i)
  
   ggplot(data=africa) + 
   geom_sf(col = "black", alpha = 0.00001) + 
   geom_point(data= dplyr::filter(data, Jahr== i), aes(x = Longitude, y = Latitude), col = "black", size=0.5, fill=20) + 
   xlab("Latitude") + ylab("Longitude") + 
   ggtitle(paste("Number of events on the African continent in",i))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
  ggsave(filename=paste("Events in Africa_",i,".png", sep=""), path=path_plots) 
  
}

ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data=data, aes(x = Longitude, y = Latitude), col = "black", size=0.000000000000001, fill=20) + 
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle("Number of events on the African continent in")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#events_in_africa<-st_join(data,africa, left=F)
#counts_events_by_country %>% filter(Jahr==2019) %>%arrange(sort(n,decreasing=FALSE))

#Events on the African Continent 1995-2021: Comparison between Countries with many Events to few Events

for(i in Jahre){

p<-ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]==1),aes(x = Longitude, y = Latitude), col = "red", size=0.5, fill=20) + 
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle(paste("Events>Median in",i))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

q<-ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data= dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]==0) ,aes(x = Longitude, y = Latitude), col = "red", size=0.5, fill=20) + 
  xlab("Latitude") + ylab("Longitude") +
  ggtitle(paste("Events<Median in",i))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave(plot=plot_grid(p,q,ncol = 2, align="hv"),filename=paste("Events_Comparison_",i,".png", sep=""), path= path_plots, width = 15, height = 9) 

}


#Frequency of Latitude and Longitude 1995-2021

for(i in Jahre){
  
ggplot(data= dplyr::filter(data, Jahr== i)) + geom_density(aes(Latitude, fill="Latitude"), alpha=0.2) +
  geom_density(aes(Longitude, fill="Longitude"), alpha=0.2) + 
  scale_fill_manual(name = "Geographic Coordinate", values = c(Latitude = "red", Longitude = "green"))+
  xlab(label = "Geographic Coordinate")+
  ylab(label="Density")+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))

ggsave(filename=paste("Density_Latitude_Longitude_",i,".png", sep=""), path= path_plots, width = 15, height = 9) 

}


#Frequency of observations in each country 1995-2021

colnames(counts_events_by_country)[which(names(counts_events_by_country) == "n")] <- "count"
counts_events_by_country$count<-as.numeric(counts_events_by_country$count)

for(i in 2019){

counts_events_by_country%>%arrange(count)%>%mutate(Country=factor(Country,levels = Country))%>%filter(Jahr==i)%>%
ggplot(aes(y=Country, x=count)) +
    geom_point(size=1)+
    geom_vline(xintercept =771.5,linetype="dotted", color="red")+
    ggtitle(paste("Absolute Number of Events in",i))+
  ylab(label="Country")+xlab(label="Absolute Number of Events")+
    theme( plot.title = element_text(color = "black", size=14, hjust=0.5))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))

ggsave(filename=paste("Number_Events_Countries_",i,".png", sep=""), path= path_plots, width = 15, height = 9) 

}


#Average Frequency of observations by country from 1995-2010

colnames(average_events_by_country)[which(names(average_events_by_country) == "mean(n)")] <- "mean"

#with label
average_events_by_country%>%arrange(mean)%>%mutate(Country=factor(Country,levels = Country))%>%
ggplot(aes(x=mean, y=Country)) +
  geom_point(size=1)+
  geom_vline(aes(xintercept =average_median,color="median"),linetype="dotted")+
  ggtitle("Average Number of Events between 1995 and 2020")+
  ylab(label="Country")+xlab(label="Average Number of Events between the Years")+
  scale_color_manual(name = "Average Median Number of Events", values = c(median= "red"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave("Average_Number_Events_label.png", path= path_plots, width = 15, height = 9) 

#without label
average_events_by_country%>%arrange(mean)%>%mutate(Country=factor(Country,levels = Country))%>%
  ggplot(aes(x=mean, y=Country)) +
  geom_point(size=1)+
  geom_vline(xintercept =average_median,linetype="dotted", colour="red")+
  ggtitle("Average Number of Events between 1995 and 2020")+
  ylab(label="Country")+xlab(label="Average Number of Events between the Years")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave("Average_Number_Events.png", path= path_plots, width = 15, height = 9) 



#Density of events in Africa 1995-2021

for(i in 2019){
  
ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_bin2d(data= dplyr::filter(data, Jahr== i), aes(x = Longitude, y = Latitude), bins=150) + 
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle(paste("Events in Africa: Density of Events in",i))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))+
  scale_fill_gradient2(low="deepskyblue", mid="dodgerblue3", high = "firebrick1")

ggsave(filename=paste("Event_Density_Africa_",i,".png", sep=""),path= path_plots, width = 15, height = 9) 
  
}


##Distribution in count data/density of the intensity numbers for countries with many and few event

for (i in Jahre) {
p<- ggplot(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]==1), aes(x = Intensity, colour=data[[paste0("danger_median_",i)]]==0)) + 
  geom_histogram(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]==1),position="dodge", bins=50, fill="transparent") + 
  scale_color_manual(name="Number Events", labels = c("Low", "High"), values=c("red", "darkgreen"))+
  xlab("Intensity") + ylab("Absolute Frequency")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

q<-ggplot(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]==0),aes(x = Intensity, colour=data[[paste0("danger_median_",i)]]==0)) +
  geom_density(data=dplyr::filter(data, Jahr==i & data[[paste0("danger_median_",i)]]==0),aes(colour=data[[paste0("danger_median_",i)]])) +
  scale_color_manual(name="Number Events", labels = c("Low", "High"), values=c("red", "darkgreen"))+
  xlab("Intensity") + ylab("Density")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave(plot=plot_grid(p,q,ncol = 2, align="hv"),filename=paste("Intensity_Country_Median",i,".png", sep="",path= path_plots, width = 15, height = 9)) 

}

#Events in First Quarter compared to last Quarter

for(i in Jahre){

p<-ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data=dplyr::filter(data, Jahr== i & Event.Quarter==min(Event.Quarter)), aes(x = Longitude, y = Latitude, colour = Event.Quarter), size=0.9) + 
  scale_color_manual(name="Event Quarter", labels = c("First Quarter"), values=c("red"))+
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle(paste("Events in Africa: First Quarter",i))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

q<-ggplot(data=africa) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_point(data=dplyr::filter(data, Jahr== i & Event.Quarter==max(Event.Quarter)), aes(x = Longitude, y = Latitude, colour = Event.Quarter), size=0.9) + 
  scale_color_manual(name="Event Quarter", labels = c("Last Quarter"), values=c("red"))+
  xlab("Latitude") + ylab("Longitude") + 
  ggtitle(paste("Events in Africa: Last Quarter",i))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave(plot=plot_grid(p,q,ncol = 1, align="hv"),filename=paste("Events_Quarter_Comparison_",i,".png", sep=""),path= path_plots, width = 9, height = 9) 

}

###################################################### CAMEO CODE ########################################################

#Brainstorming

#number of missings in Country variables
sum(is.na(data$Country)) #0

#create variable equal=True if the same source, target and country
data$Equal<-ifelse(data$Source.Country == data$Target.Country &  data$Target.Country == data$Country,TRUE,FALSE)


############################################Focus on North-Eastern African Countries#########################################

#Countries:Eritrea, Ethiopia,Uganda,Somalia,Kenya,South Sudan
#State Based Conflicts in 2018 at country level: Somalia, South Sudan, Ethiopia*, Kenya*
#No State Based Conflict in 2018 at country level: Eritrea, Uganda


#prepare for loop
states_africa_north_east<-c("Eritrea", "Ethiopia","Uganda","Somalia","Kenya","South Sudan")

#compare absolute frequency of Root Cameo Code between 2016 and 2018 in each country (Brandt et al. 2022)

#Example for Loop
i<-"Somalia"

for(i in states_africa_north_east){

p<-data%>%filter(Country==i & Jahr=="2016") %>% ggplot(aes(x=as.character(lvl0)))+
  geom_bar(stat = "count",fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle(paste(i,"(2016)"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

q<-data%>%filter(Country==i & Jahr=="2017") %>%ggplot(aes(x=as.character(lvl0)))+
  geom_bar(stat="count",fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle(paste(i,"(2017)"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

d<-data%>%filter(Country==i & Jahr=="2018") %>%ggplot(aes(x=as.character(lvl0)))+
  geom_bar(stat="count",fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle(paste(i,"(2018)"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave(plot=plot_grid(p,q,d,ncol = 3, align="hv"),filename=paste("Root_Cameo_Code_Evolution_Absolute_",i,".png", sep="")) 


}

data%>%filter(Country=="Somalia" & Jahr=="2007") %>% ggplot(aes(x=as.character(lvl0)))+
  geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
  xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(breaks=1:20,limits=factor(1:20))+
  ggtitle(paste(i,"(2018)"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))



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
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
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
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
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
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
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
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

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
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))
  
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
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave(plot=plot_grid(p,q,d,ncol = 3, align="hv"),filename=paste("Uganda_Evolution_Relative_",i,".png", sep="")) 


#compare evolution of events in each country (Mueller and Rauh 2022)


#seperated for each country
#time span: 1995-2020

counts_events_by_country %>% filter(Country=="Somalia") %>% 
  ggplot(aes(Jahr,n,group=1))+ geom_point(colour="darkgreen")+ geom_line(colour="darkgreen")+
  stat_peaks(colour = "black",shape="circle open") +
  xlab("Year")+ ylab("Number of Events")+
  ggtitle("Somalia")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#all countries in one plot
#time span on yearly level: 1995-2020

counts_events_by_country %>% filter(Country %in% states_africa_north_east) %>% 
  ggplot(aes(x=Jahr,y=stat(n)/sum(n),group=Country,color=Country))+ geom_point()+ geom_line()+
  xlab("Year")+ ylab("Number of Events")+
  ggtitle("North-Eastern African Countries")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

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
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

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
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))

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
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


q<-cameo_freq_month %>% filter(Country=="Somalia" & Month>="18-01" & Month<="18-06" & lvl0==18) %>% 
  ggplot(aes(x="", y=n ,fill=Event.Text))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Cameo Code 18")+
  ggtitle(paste("Cameo Code 18 Somalia 18-01 till 18-06"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave(plot=plot_grid(p,q,ncol = 2),filename=paste("Cameo_Code_18_Pie_Somalia",i,".png", sep="")) 

#Uganda
p<-cameo_freq %>% filter(Country=="Uganda" & Jahr==2017 & lvl0==18) %>% 
  ggplot(aes(x="", y=n ,fill=Event.Text))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Cameo Code 18")+
  ggtitle(paste("Cameo Code 18 Uganda 2017"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

q<-cameo_freq_month %>% filter(Country=="Uganda" & Month>="18-01" & Month<="18-06" & lvl0==18) %>% 
  ggplot(aes(x="", y=n ,fill=Event.Text))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Cameo Code 18")+
  ggtitle(paste("Cameo Code 18 Uganda 18-01 till 18-06"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave(plot=plot_grid(p,q,ncol = 2, align = "hv"),filename=paste("Cameo_Code_18_Pie_Uganda",i,".png", sep="")) 





####For Analysis: keep data only if North Eastern country


north_eastern_africa <- data%>% filter(Country %in% states_africa_north_east)
states_africa_north_east_danger<-c("Ethiopia","Somalia","Kenya","South Sudan")
states_africa_north_east_no_danger<-c("Eritrea", "Uganda")



#dsitribution of intensity in these countries 2016-2018

#in 2016
p<-north_eastern_africa %>% filter(Jahr==2016 & Country %in% states_africa_north_east_no_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2016")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

p1<-north_eastern_africa %>% filter(Jahr==2016 & Country %in% states_africa_north_east_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2016")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


#in 2017

q<-north_eastern_africa %>% filter(Jahr==2017 & Country %in% states_africa_north_east_no_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2017")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

q1<-north_eastern_africa %>% filter(Jahr==2017  & Country %in% states_africa_north_east_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2017")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


#in 2018

d<-north_eastern_africa %>% filter(Jahr==2018 & Country %in% states_africa_north_east_no_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2018")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

d1<-north_eastern_africa %>% filter(Jahr==2018  & Country %in% states_africa_north_east_danger) %>% ggplot() + 
  geom_density(aes(Intensity, colour= Country))+ 
  ggtitle("Intensity Comparison in 2018")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

grid.arrange(p,q,d,p1,q1,d1, ncol=3,nrow=2)


#duplicates role: same story ID but varying in othervariables

#in which year the highest number of duplicates
north_eastern_africa %>% group_by(Jahr) %>%
  filter(duplicated(Story.ID)) 

#in which country the highest number of duplicates
north_eastern_africa %>% group_by(Country) %>%
  filter(duplicated(Story.ID)) 








