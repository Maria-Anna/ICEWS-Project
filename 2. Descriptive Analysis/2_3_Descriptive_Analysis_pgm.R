############################################ Descriptive Analysis Prio Grid ############################################################

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


rm(list=ls())

#Assign Path
path_data_icews_pgm<-"~/ICEWS-Project/Data/data_icews_pgm"
path_folder_cellshp<-"~/ICEWS-Project/Data/priogrid_cellshp"

#Read ICEWS
data_icews_pgm<-readRDS(path_data_icews_pgm)

#Path were the plots should be saved
path<- "~/ICEWS-Project/2. Descriptive Analysis/Plots"

#Read Polygons
prio_grid_polygons <- st_read(dsn = path_folder_cellshp, layer = "priogrid_cell", stringsAsFactors = F, quiet=T) %>% mutate(gid = as.character(gid))


#Drop duplicates
#Coordinate is on edge of four PRIO grids, with equal distance to all of the four grid cells
#Just keep one of the four randomly
data_icews_pgm<-data_icews_pgm[!duplicated(data_icews_pgm$ID),] 


#Drop False Coded
false_coded<-data_icews_pgm%>% filter(Longitude < -50 | Longitude > 64 | Latitude> 40)
data_icews_pgm<- filter(data_icews_pgm, !Event.ID %in% false_coded$Event.ID)



#rm(folder_cellshp, false_coded)

#--------------------------------------------------------------------------------------------------------
#############################################
######## Create empty Map ##################
############################################

#Read Map

#Define the States from which we need the Coordinates of their Capitals
states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso (Upper Volta)", "Burundi","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo", "Congo, Democratic Republic of (Zaire)","Cote D'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia",
                 "Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar (Malagasy)","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria", "Rwanda","Sao Tome and Principe ","Swaziland (Eswatini)","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan", "Tanzania (Tanganyika)","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe (Rhodesia)" )


#Create African map
map <- cshp(date=as.Date("2012-1-01"), useGW=TRUE)
map <- st_as_sf(map, sf_use_s2(FALSE))
map <- map[map$country_name %in% states_africa ,]


#Grid Map for Africa Grids
map<- st_as_sf(map, coords = c("caplong", "caplat"), crs = st_crs(prio_grid_polygons))

#The above code results in an error but we need to run it, so R can assume that the Coordinates are planar
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

#rm(map_c)

#----------------------------------------------------------------------------------------------------------------------------------

##################################################
###### Make Prio-Grid Map #######################
##################################################

#Keep only polygons in Africa
africa_polygons<-semi_join(prio_grid_polygons, map_africa)

#If interested in the events in a specific year
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



#Assign every Polygon to a relative Number
count_events_poly<-left_join(africa_polygons,count_events)
count_events_poly[is.na(count_events_poly)] <- 0 #Assign 0 to NA


#----------------------------------
#Plot the relative Frequency of events accross all Years that are not NA, so here >0 because
#we replaced above the NAs with zero

#Polygons where rel > 0
count_events_poly_0<-count_events_poly %>% filter(rel > 0)

#Ggplot only events with relative frequency over zero
#For Absoliute Frequency replace fill= rel with fill= n.x in line 149
#To display the border of the grids replace col= "transparent" with col="grey" in line 149
ggplot() + 
  geom_sf(data = count_events_poly_0, aes(fill = rel),col = "transparent") +
  geom_sf(data=map, col="black", fill= NA)+
  xlab("Longitude")+ylab("Latitude")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right",       
        legend.key.height = unit(10,"cm")) +
  scale_fill_gradient2("Relative Frequency",low="#91bfeb", mid="#91bfeb", high = "#ff000d", 
                       breaks= c(0, 0.25, 0.5, 0.75, 1), labels=c(">0", 0.25, 0.5, 0.75, 1), 
                       limits= c(0,1))+
  guides(fill=guide_colorbar(title.vjust=2.5))+
  theme_classic(base_size = 16)

#Save plot
ggsave(filename=paste("Relative_Prio_Map_Events_noNA",".png", sep=""), width= 9, height = 9, path=path)


#rm(count_events_country, count_events_poly, count_events_poly_0, count_events)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###############################################
########### Capital Analysis Somalia  #########
###############################################


capital<- map_capital %>% select(gid, country_name, capname, caplong, caplat)

#County names are not matching in map and in icews data
capital<- capital %>% 
          mutate(Country = replace(country_name, country_name ==  "Burkina Faso (Upper Volta)", "Burkina Faso"),
                                   country_name = replace(country_name, country_name ==   "Congo, Democratic Republic of (Zaire)"  ,"Congo, DRC"),
                                   country_name == replace(country_name, country_name ==  "Cote D'Ivoire", "Cote d'Ivoire "),
                                   country_name ==replace(country_name, country_name ==   "Madagascar (Malagasy)", "Madagascar"),
                                   country_name == replace(country_name, country_name ==  "Swaziland (Eswatini)", "Eswatini"),
                                   country_name == replace(country_name, country_name ==  "Tanzania (Tanganyika)", "Tanzania "),
                                   country_name == replace(country_name, country_name ==  "The Gambia", "Gambia"),
                                   country_name == replace(country_name, country_name ==  "Zimbabwe (Rhodesia)", "Zimbabwe"))



#Filter State of Interest
state<- "Somalia"

#If interested in the events that took place in that specific state in a specific year
Year<-2019

#Count Events per git per country
  count_events<-data_icews_pgm %>%
    as.data.frame() %>%
    filter(year== Year)%>%
    group_by(gid, Country) %>%
    count()
  
#Count Events per Country
  count_events_country<-data_icews_pgm %>%
    as.data.frame() %>%
    filter(year== Year)%>%
    group_by( Country) %>%
    count()
  
#Merge and Calculate relative Frequency
  count_events<- count_events %>% left_join(count_events_country, by="Country")
  count_events$rel<- count_events$n.x / count_events$n.y
  
  
#Filter Capital  
capital_new<- capital %>% filter(country_name  == state)


#ggplot Barplot
count_events %>% filter(Country== state) %>%
  ggplot(aes(y=gid, x=rel, fill=factor(ifelse(gid== capital_new$gid, "Mogadishu","Other")))) +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Somalia", values=c("#440154", "darkgrey")) +
  ylab("Prio Grid ID") + xlab("Relative Frequency")+
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position=c(0.9, 0.9),
        legend.text=element_text(size=16),
        legend.title =element_text(size=16) )

#Save plot
ggsave(filename=paste("Relative_Prio_Events_in_",state,"_capital",".png", sep=""), width= 30, height = 25, path=path)


#-----------------------

#Make map for Single Country
  map_noth_east<-map_africa %>% filter(country_name == state)
  country_polygons<-semi_join(prio_grid_polygons, map_noth_east)
#Assign every Polygon a relative Number
  count_events_poly<-left_join(country_polygons,count_events)
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
    theme(legend.position="right",       
          legend.key.height = unit(10,"cm")) +
    scale_fill_viridis(option = "D","Relative Frequency",discrete = F, direction=1, 
                       breaks= c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 0.25, 0.5, 0.75, 1), 
                       limits= c(0,1))+
    guides(fill=guide_colorbar(title.vjust=2.5))+
    theme_classic(base_size = 16) 

  #Save plot
  ggsave(filename=paste("Relative_Prio_Map_Events_in_",state,".png", sep=""), width= 9, height = 9, path = path)

  
  
#-----------------------------------------------------------------------------------------------------------------
#############################################
############## Animation Plots  #############
#############################################
  
#Count Events per git per country, per year and per gid
count_events<-data_icews_pgm %>%
    as.data.frame() %>%
    group_by(gid, Country, year) %>%
    count()
  
#Count Events per Country, per year
  count_events_country<-data_icews_pgm %>%
    as.data.frame() %>%
    group_by( Country, year) %>%
    count()
  
#Merge and Calculate relative Frequency
  count_events<- count_events %>% left_join(count_events_country, by=c("Country", "year"))
  count_events$rel<- count_events$n.x / count_events$n.y
  

  #Make a data set such that every gid is assigned to every possible year
  #we do this because otherwise the animated plot has breaks. For the years that 
  #is no data available we assign them to a relative frequency of 0
  
  #Data frame with all gids that are part of the african map
  gid<-data.frame(gid=africa_polygons$gid)
  #Data frame with all years that appear in the icews dataset
  year<-data.frame(year=seq(1995, 2021, by=1))
  #Evers gid appears 27 times, so for each available year
  data<-gid %>% merge(year)
  
  #Join the above created dataframe with the counts per year dataframe
  count_events<-data %>% left_join(count_events, by=c("gid","year"))
  
  #The borders of some Countries changend over time, so some grid ids appear more than 27 times
  #Drop the wrong grids
  count_events<-count_events[!duplicated(count_events[c(1,2)]),]  
  
  
    
#Assign every Polygon to a relative Number
count_events_poly<-left_join(africa_polygons,count_events)
count_events_poly[is.na(count_events_poly)]<- 0 #Assign 0 to NA
count_events_poly$year<-as.character(count_events_poly$year) #Year as Character  
  

#-----------------------------------
#Plot the relative Frequency of events accross all Years that are not NA, so here >0 because
#we replaced above the NAs with zero

#Absolute Events Polygons where rel>0
count_events_poly_0<-count_events_poly %>% filter(rel > 0)


plot<-ggplot() + 
  geom_sf(data = count_events_poly_0, aes(fill = rel),col = NA) +
  geom_sf(data=map, col="black", fill= NA)+
  xlab("Longitude")+ylab("Latitude")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key.size = unit(5, 'cm'),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right",       
        legend.key.height = unit(20,"cm")) +
  scale_fill_gradient2("Relative Frequency",low="#91bfeb", mid="#91bfeb", high = "#ff000d", 
                       breaks= c(0, 0.25, 0.5, 0.75, 1), labels=c(">0", 0.25, 0.5, 0.75, 1), 
                       limits= c(0,1))+
  guides(fill=guide_colorbar(title.vjust=2.5))+
  theme_classic(base_size = 16)


#Animate Plot
plot_animation<-plot + transition_manual(year)+ labs(subtitle = "Year: {current_frame}")
animate(plot_animation, nframes= length(unique(count_events_poly$year)), fps=1, height = 1172, width =1900,
        #For Latex
        #renderer = file_renderer( prefix = "Animation_Map_noNA", overwrite = TRUE)
)


#Save Plot
anim_save("Animation_Map_noNA.gif", path=path)



#--------------------------------------
#Plot only for a specific state
#For example only for Somalia


#Filter State of Interest
state<- "Somalia"

#Make map for Single Country
map_noth_east<-map_africa %>% filter(country_name %in% state)
country_polygons<-semi_join(prio_grid_polygons, map_noth_east)

#Assign every Polygon a relative Number
count_events_poly_state<-left_join(country_polygons,count_events)
count_events_poly_state[is.na(count_events_poly_state)] <- 0 #Assign 0 to NA
count_events_poly_state$year<-as.character(count_events_poly_state$year) #Year as character


#Ggplot for single Country with polygons
plot<-ggplot() + 
  geom_sf(data = count_events_poly_state, aes(fill = rel),col = "grey") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right",       
        legend.key.height = unit(10,"cm")) +
  scale_fill_viridis(option = "D","Relative Frequency",discrete = F, direction=1, 
                     breaks= c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 0.25, 0.5, 0.75, 1), 
                     limits= c(0,1))+
  guides(fill=guide_colorbar(title.vjust=2.5))+
  theme_classic(base_size = 16) 


#Animate Plot
plot_animation<-plot + transition_manual(year)+ labs(subtitle = "Year: {current_frame}")
animate(plot_animation, nframes= length(unique(count_events_poly$year)), fps=1,
        #For Latex
        #renderer = file_renderer( prefix = "Animation_Map_Somalia", overwrite = TRUE)
)



#Save Plot
anim_save(paste("Animation_map_Somalia",state,".gif",sep=""), path=path)

 