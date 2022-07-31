library(sf)
library(dplyr)
library(vroom)
library(ggplot2)
library(tidyr)
library(crsuggest)
library(giscoR)
library(cshapes)
library(readr)
library(ggpubr)
library(viridis)
library(gganimate)
library(gifski)
library(transformr)

#Read ICEWS
data_icews_pgm<-readRDS("~/ICEWS-Project/Data/data_icews_pgm")
#Path were the plots should be saved
path<- "~/ICEWS-Project/2. Descriptive Analysis/Plots"

#Drop False Coded
false_coded<-data_icews_pgm %>% filter( Longitude < -50 | Longitude > 64 | Latitude> 40)
data_icews_pgm<- filter(data_icews_pgm, !Event.ID %in% false_coded$Event.ID)


#Read Polygons
#Set working directory with PRIO Grid Shapefiles
folder_cellshp<- "~/ICEWS-Project/Data/priogrid_cellshp"
prio_grid_polygons <- st_read(dsn = folder_cellshp, layer = "priogrid_cell", stringsAsFactors = F, quiet=T) %>% mutate(gid = as.character(gid))


rm(folder_cellshp)
rm(false_coded)
#--------------------------------------------------------------------------------------------------------
#############################################
######## Create empty Map ##################
############################################

#Wir brauchen das um die Grids für ganz Afrika zu bekommen und für die Koordinaten der Hauptstädte 
#Read Map
states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso (Upper Volta)", "Burundi","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo", "Congo, Democratic Republic of (Zaire)","Cote D'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia",
                 "Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar (Malagasy)","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria", "Rwanda","Sao Tome and Principe ","Swaziland (Eswatini)","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan", "Tanzania (Tanganyika)","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe (Rhodesia)" )



#create african map
map <- cshp(date=as.Date("2012-1-01"), useGW=TRUE)
map <- st_as_sf(map, sf_use_s2(FALSE))
map <- map[map$country_name %in% states_africa ,]


#Grid Map for Africa Grids
map<- st_as_sf(map, coords = c("caplong", "caplat"), crs = st_crs(prio_grid_polygons))

#Fehlermeldung, muss aber ausgeführt werden damit angenommen wird dass die Koordinaten planar sind
map_africa <- st_join(map, prio_grid_polygons, sf_use_s2(FALSE))
#Join map and polygons
map_africa <- st_join(map, prio_grid_polygons)


#Drop geometry variable
map_africa<- as.data.frame(map_africa)
map_africa<- map_africa[,!(names(map_africa) %in% c("geometry"))]



#Assign Grid to every Capital
map_c<-as.data.frame(map)
map_c<- map_c[,!(names(map_c) %in% c("geometry"))]
map_c<- st_as_sf(map_c, coords = c("caplong", "caplat"), crs = st_crs(prio_grid_polygons))
map_capital <- st_join(map_c, prio_grid_polygons)


#Tranform geometry into longitude and latitude
map_capital<-map_capital %>% extract(geometry, c('caplong', 'caplat'), '\\((.*), (.*)\\)', convert = TRUE) 


#Drop geometry variable
map_capital<- as.data.frame(map_capital)
map_capital<- map_capital[,!(names(map_capital) %in% c("geometry"))]

rm(map_c)

#----------------------------------------------------------------------------------------------------------------------------------

##################################################
###### Make Prio-Grid Map #######################
##################################################

#Keep only polygons in Africa
africa_polygons<-semi_join(prio_grid_polygons, map_africa)

#Create Borders for ggplot
borders<-borders(database = "world", xlim= c(min(data_icews_pgm$Longitude), max(data_icews_pgm$Longitude)), ylim= c(min(data_icews_pgm$Latitude), max(data_icews_pgm$Latitude)))
borders<-borders$data
unique(borders$region)
not_needed<-c("Yemen", "Uzbekistan", "Turkey", "Turkmenistan", "Syria", "Ascension Island", "Saint Helena" , "Western Sahara",
              "Qatar","Palestine","Madeira Islands", "Portugal", "Oman", "Pakistan", "Kuwait", "Jordan", "Italy" ,"Iran","Iraq",
              "Greece", "Mayotte", "Reunion" , "Canary Islands","Spain","Cyprus" ,"Ivory Coast","Bahrain", "Israel",
              "Afghanistan","Guinea-Bissau","Saudi Arabia","Lebanon", "United Arab Emirates")
states<- borders %>% filter(!region %in% not_needed)%>% select(region)
states<-unique(states$region)
borders<-borders(database = "world", region = states)

#Count Events per git per country
count_events<-data_icews_pgm %>%
  as.data.frame() %>%
  group_by(gid, Country) %>%
  count()

#Count Events per Country
count_events_country<-data_icews_pgm %>%
  as.data.frame() %>%
  group_by( Country) %>%
  count()

#Merge and Calculate relative Frequency
count_events<- count_events %>% left_join(count_events_country, by="Country")
count_events$rel<- count_events$n.x / count_events$n.y

#Assign every Polygon a relative Number
count_events_poly<-left_join(africa_polygons,count_events)
count_events_poly[is.na(count_events_poly)] <- 0 #Assign 0 to NA

#Plot with all Events across all years
#Relative Frequency
ggplot(data= map)+
  geom_sf(data = count_events_poly, aes(fill = rel),col = "grey") +
  borders+
  xlab("Longitude")+ylab("Latitude")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right") +
  scale_fill_viridis(option = "B","Relative Frequency",discrete = F, direction=-1, 
                     begin= min(count_events_poly$rel, na.rm=TRUE ), end=max(count_events_poly$rel, na.rm = TRUE))

ggsave(filename=paste("Relarive_Map_ScalaB",".png", sep=""), width=9, height = 9, path=path)


#Plot with all Events across all years
#Absolute Frequency
ggplot(data=map) + 
  geom_sf(col = "black", alpha = 0.00001) +
  geom_sf(data = count_events_poly, aes(fill = n.x),col = "grey") +
  borders+
  xlab("Longitude")+ylab("Latitude")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right") +
  scale_fill_viridis(option = "B","Absolute Frequency",discrete = F, direction=-1)

ggsave(filename=paste("Absolute_Prio_Map_Events in Africa",".png", sep=""), width= 9, height = 9, path=path)


#Polygons where rel > 0.01
count_events_poly_001<-count_events_poly %>% filter(rel > 0.01)

#Ggplot only events with relative frequency over 0.01
ggplot(data=map) + 
  geom_sf(col = "black", alpha = 0.00001) +
  geom_sf(data = count_events_poly_001, aes(fill = rel),col = "grey") +
  borders+
  xlab("Longitude")+ylab("Latitude")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right") +
  scale_fill_viridis(option = "B","Relative Frequency",discrete = F, direction=-1)

#Save plot
ggsave(filename=paste("Relative_Prio_Map_Events_001_B",".png", sep=""), width= 9, height = 9, path=path)


rm(count_events_country, count_events_poly, count_events_poly_001, count_events)
rm(not_needed, states, states_africa, borders)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###############################################
########### Capital Analysis Somalia  #########
###############################################


capital<- map_capital %>% select(gid, country_name, capname, caplong, caplat)

#County names are not maching in map and in icews data
#We need this only for the loom
capital<- capital %>% 
  mutate(Country = replace(country_name, country_name ==  "Burkina Faso (Upper Volta)", "Burkina Faso"),
         country_name = replace(country_name, country_name ==   "Congo, Democratic Republic of (Zaire)"  ,"Congo, DRC"),
         country_name == replace(country_name, country_name ==  "Cote D'Ivoire", "Cote d'Ivoire "),
         country_name ==replace(country_name, country_name ==   "Madagascar (Malagasy)", "Madagascar"),
         country_name == replace(country_name, country_name ==  "Swaziland (Eswatini)", "Eswatini"),
         country_name == replace(country_name, country_name ==  "Tanzania (Tanganyika)", "Tanzania "),
         country_name == replace(country_name, country_name ==  "The Gambia", "Gambia"),
         country_name == replace(country_name, country_name ==  "Zimbabwe (Rhodesia)", "Zimbabwe"))




state<- "Somalia"
#Year<-2019

#Count Events per git per country
count_events<-data_icews_pgm %>%
  as.data.frame() %>%
  #filter(year== Year)%>%
  group_by(gid, Country) %>%
  count()

#Count Events per Country
count_events_country<-data_icews_pgm %>%
  as.data.frame() %>%
  #filter(year== Year)%>%
  group_by( Country) %>%
  count()

#Merge and Calculate relative Frequency
count_events<- count_events %>% left_join(count_events_country, by="Country")
count_events$rel<- count_events$n.x / count_events$n.y


#Filter Capital  
capital_new<- capital %>% filter(country_name  == state)


#ggplot Barplot
count_events %>% filter(Country== state ) %>%
  ggplot(aes(y=gid, x=rel, fill=factor(ifelse(gid== capital_new$gid ,capital_new$capname,"Other")))) +
  geom_bar(stat="identity") +
  scale_fill_manual(name = capital_new$country_name, values=c("darkblue", "darkred")) +
  ylab("Prio Grid") + xlab("Relative Frequency")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Save plot
ggsave(filename=paste("Prio_Events in Africa",i,j,".png", sep="_"), width= 15, height = 9)


#-------------------

#Make map for Single Country
map_noth_east<-map_africa %>% filter(country_name == state)
africa_polygons<-semi_join(prio_grid_polygons, map_noth_east)
#Assign every Polygon a relative Number
count_events_poly<-left_join(africa_polygons,count_events)
count_events_poly[is.na(count_events_poly)] <- 0 #Assign 0 to NA


#Ggplot for single Country with polygons
ggplot() + 
  geom_sf(data = count_events_poly, aes(fill = rel),col = "grey") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right") +
  scale_fill_viridis(option = "B","Relative Frequency",discrete = F, na.value= "white", direction =-1)

#Save plot
ggsave(filename=paste("Prio_Map_Events in Africa B",i,"north_east",".png", sep=""), width= 9, height = 9, path = path)


