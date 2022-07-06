library(readr)
library(data.table)
library(ggplot2)
library(dplyr)
library(cshapes)
library(sf)
library(gridExtra)

#Assign saving path for Map plots
path_map<-"~/ICEWS-Project/2. Descriptive Analysis/Plots/Map"
#Assign saving path for all other Plots
path_plot<-"~/ICEWS-Project/2. Descriptive Analysis/Plots"

#Load data sets: load and format correctly
data_icews_cm<- fread("~/ICEWS-Project/Data/data_icews_cm.csv")

capital<- read.csv("~/ICEWS-Project/Data/country-capitals.csv", sep=",")

################################
##### Data Preparation #########
###############################

#Data
data<-data_icews_cm

#Modify Data with Year
data$year<-format(as.Date(data$Event.Date, format="%Y-%m-%d"),"%Y") 


#Drop not correctly geocoded observations
false_coded<-data %>% filter( Longitude < -50 | Longitude > 64 | Latitude> 40)
data<- filter(data, !Event.ID %in% false_coded$Event.ID)

#Keep only capitals in Africa
capital<-capital %>% filter(ContinentName == "Africa")
capital$CapitalLatitude<-as.numeric(capital$CapitalLatitude)
capital$CapitalLongitude<-as.numeric(capital$CapitalLongitude)
capital<-capital[c(-45,-42,-58),]
capital$City<-"Capital"

########################################
########## African Map ################
#######################################

#Prepare for Map
#States to create the map, names must map the names from chsp Function
states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso (Upper Volta)", "Burundi","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo", "Congo, Democratic Republic of (Zaire)","Cote D'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia",
                 "Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar (Malagasy)","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria", "Rwanda", "Swaziland (Eswatini)","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan","Tanzania (Tanganyika)","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe (Rhodesia)" )



#create african map
map <- cshp(date=as.Date("2012-1-01"), useGW=TRUE)
map <- st_as_sf(map)
map <- map[map$country_name %in% states_africa ,]

#############################
##### Plots with Map#########
#############################

#Number of Events 
Year<-seq(1995, 2020, by= 1)

for(i in Year){

 #Plot 
 ggplot(data=map) + 
  geom_sf(col = "black", alpha = 0.00001) + 
  geom_bin2d(data= dplyr::filter(data, year== i), aes(x = Longitude, y = Latitude), bins=150) + 
  xlab("Latitude") + ylab("Longitude") + 
  labs(fill='Absolute Frequency')+ 
  scale_fill_gradient2(low="deepskyblue", mid="dodgerblue3", high = "firebrick1")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))
  
  #Save Plot
  ggsave(filename=paste("Events in Africa_",i,".png", sep=""), path= path_map) 
  
}


#Number of Events + Intensity + Capital
Year<-seq(1995, 2020, by= 1)

for(i in Year){
  
  #Plot
  ggplot(data=map) + 
    geom_sf(col = "black", alpha = 0.00001) +
    geom_point(data=dplyr::filter(data, year== i), aes(x = Longitude, y = Latitude,colour = Intensity),size= 0.7) + 
    geom_point(data= capital, aes(x= CapitalLongitude, y=CapitalLatitude, fill= City),size=0.7, col ="red" )+
    #geom_text(data= capital, aes(x= CapitalLongitude, y=CapitalLatitude, label= CapitalName))+
    ylab(label="Latitude")+xlab(label="Longitude")+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
  #Save Plot
  ggsave(filename=paste("Map_Intensity_with_all_Events_in_" , i,".png", sep=""), path = path_map, width = 15, height = 7)
  
}

################################################
##### Averange Number Events + MEDIAN  #########
################################################

#Create Median
median<- data %>% group_by(Country,year) %>% count() %>% ungroup (Country) %>% summarise(median(n))  
average_median<-mean(median$`median(n)`)

#Create Dataframe with Average Events
average_events_by_country<- data %>% group_by(Country,year) %>% count() %>% ungroup (year) %>% summarise(mean(n)) 
colnames(average_events_by_country)[which(names(average_events_by_country) == "mean(n)")] <- "mean"

#Plot Averange Number of Events
average_events_by_country%>%arrange(mean)%>%mutate(Country=factor(Country,levels = Country))%>%
  ggplot(aes(x=mean, y=Country)) +
  geom_point(size=1)+
  geom_vline(aes(xintercept =average_median,color="median"),linetype="dotted")+
  ylab(label="Country")+xlab(label="Average Number of Events between the Years")+
  scale_color_manual(name = "", values = c(median= "red"))+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Save Plot
ggsave("Frequency_events.png", width=15, heigh=10, path= path_plot)

################################################
##### Count Events Lineplot (Duplicates) #######
################################################

#Dataframe with count events
counts_events_by_year<-data %>% group_by(year) %>% count(sort=TRUE)

#Plot
counts_events_by_year%>%
  ggplot(aes(year,n,group=1))+ geom_point()+ geom_line()+
  xlab("Year")+ ylab("Number of Events")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Save Plot
ggsave("Count_events_lineplot.png", width=15, height = 7,path =path_plot)

######################################################
############ Descriptive Cameo Root Code ############
#####################################################

states_africa_north_east<-c("Eritrea", "Ethiopia","Uganda","Somalia","Kenya","South Sudan")

for(i in states_africa_north_east){
  
  #Plot for 2016
  p<-data%>%filter(Country==i & year=="2016") %>% ggplot(aes(x=as.character(CAMEO_root)))+
    geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
    xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0,0.5))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(i,"(2016)"))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
  #Plot for 2017
  q<-data%>%filter(Country==i & year=="2017") %>%ggplot(aes(x=as.character(CAMEO_root)))+
    geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
    xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 0.5))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(i,"(2017)"))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
  #Plot for 2018
  d<-data%>%filter(Country==i & year=="2018") %>%ggplot(aes(x=as.character(CAMEO_root)))+
    geom_bar(aes(y=stat(count)/sum(count)),fill="darkblue")+
    xlab("Root Cameo Code")+ ylab("Absolute Frequency")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 0.5))+
    scale_x_discrete(breaks=1:20,limits=factor(1:20))+
    ggtitle(paste(i,"(2018)"))+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
  ggsave(plot=grid.arrange(p,q,d,ncol = 3),filename=paste("Root_Cameo_Code_Evolution_Absolute_",i,".png", sep=""), width=15, height = 7,path=path_plot) 
  
  
}

##############################################
####### Density Intensity#####################
#############################################
states_africa_north_east<-c("Eritrea", "Ethiopia","Uganda","Somalia","Kenya","South Sudan")

for(j in seq_along(states_africa_north_east)){
  
  #Plot
  ggplot(data=dplyr::filter(data,  Country== states_africa_north_east[j])) + geom_density(aes(Intensity, colour = year))+
    geom_density(aes(Intensity),lwd = 0.5)+
    labs(fill='Year')+ 
    xlab("Intensity") + ylab("Density")+
    theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(hjust=0.5, size=16),
          axis.title.y= element_text(hjust=0.5,size=16),
          axis.text = element_text(size=12,colour = "black"))
  
  #Save Plot
  ggsave(filename=paste("density_", states_africa[j],"1995_2020.png", sep=""), width= 15, height = 7.5,path=path_plot)
  
}
