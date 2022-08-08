########################################## Animated Prio Grid Plots #####################################################

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



#Assign Paths
path_data_icews_pgm<-"~/ICEWS-Project/Data/data_icews_pgm"
path_folder_cellshp<- "~/ICEWS-Project/Data/priogrid_cellshp"
#Path were the plots should be saved
path<- "~/ICEWS-Project/2. Descriptive Analysis/Plots"


#Read ICEWS
data_icews_pgm<-readRDS(path_data_icews_pgm)

#Drop False Coded
false_coded<-data_icews_pgm %>% filter( Longitude < -50 | Longitude > 64 | Latitude> 40)
data_icews_pgm<- filter(data_icews_pgm, !Event.ID %in% false_coded$Event.ID)

#Drop Duplicates
data_icews_pgm<-data_icews_pgm[!duplicated(data_icews_pgm),] 

#Read Polygons
prio_grid_polygons <- st_read(dsn = path_folder_cellshp, layer = "priogrid_cell", stringsAsFactors = F, quiet=T) %>% mutate(gid = as.character(gid))


#create African map
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

#Keep only polygons in Africa
africa_polygons<-semi_join(prio_grid_polygons, map_africa)


#Delete no longer needed objects
#rm(folder_cellshp)
#rm(false_coded)

#---------------------------------------------------------------------------------------------------------------------------

#############################################
##### Data Preparation for Plots ############
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

#Delete no longer needed object
#rm(gid, year, data, count_events_country)

#-----------------------------------------------------------------------------------------------------------------
#############################################
############## Animation Plots  ##############
#############################################

#Plot with all Events 
#The NAs are replaced with 0

#Plot all Events across all years
#Relative Frequency within one state to make the Frequency comparable accross the states
#For Absolute Frequency replace in line 119 fill=rel with fill=n.x
plot<-ggplot()+
  geom_sf(data = count_events_poly, aes(fill = rel),col = NA) +
  geom_sf(data= map, col= "black", fill= NA)+
  xlab("Longitude")+ylab("Latitude")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right" , 
        legend.key.height = unit(10,"cm")) +
  scale_fill_viridis(option = "D","Relative Frequency",discrete = F, direction=1, 
                     breaks= c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 0.25, 0.5, 0.75, 1), 
                     limits= c(0,1))+
  guides(fill=guide_colorbar(title.vjust=2.5))+
  theme_classic(base_size = 16)


#Animate Plot
plot_animation<-plot +transition_manual(year)+ labs(subtitle = "Year: {current_frame}")
animate(plot_animation, nframes= length(unique(count_events_poly$year)), fps=1)

#Save Plot
anim_save("Animation_Map.gif", path=path)






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


#Animate Plot
plot_animation<-plot + transition_manual(year)+ labs(subtitle = "Year: {current_frame}")
animate(plot_animation,nframes= length(unique(count_events_poly$year)),fps=1)


#Save Plot
anim_save("Animation_Map_noNA.gif", path=path)





#--------------------------------------
#Plot only for a specific state
#For example only for Somalia

#Filter State of Interest
state<- "Somalia"

#Make map for Single Country
map_noth_east<-map_africa %>% filter(country_name %in% state)
africa_polygons<-semi_join(prio_grid_polygons, map_noth_east)
#Assign every Polygon a relative Number
count_events_poly_state<-left_join(africa_polygons,count_events)
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
plot.animation<-plot + transition_manual(year)+ labs(subtitle = "Year: {current_frame}")
animate(plot.animation,nframes= length(unique(count_events_poly$year)),fps=1)


#Save Plot
anim_save(paste("Animation_map_",state,".gif",sep=""), path=path)
