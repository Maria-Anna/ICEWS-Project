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

#read Data
#UCDP Data
GEDEvent_v22_1<- read.csv("~/ICEWS-Project/Data/GEDEvent_v22_1.csv")
#Read real forecast
Jan_2021<- read.csv("~/ICEWS-Project/3. Model/Prediction/Predicion_ICEWS_Esc/real_mcw_forecast_t_2021_01_01_s_5.csv")

#Februar
#Jan_2021<- read.csv("~/ICEWS-Project/3. Model/Prediction/Predicion_ICEWS_Esc/real_mcw_forecast_t_2021_02_01_s_6.csv")


#Filter only African Countries and 2021
a<- GEDEvent_v22_1 %>% filter(year== "2021" & region == "Africa") %>% arrange(country, date_start)
#Column with year_month
a$year_month<- substr(a$date_start, start = 1, stop = 7)
#Select columns of interest and insert key
a<- a %>% select( year, priogrid_gid, country, region, best, year_month) %>% mutate(key= paste(year_month, priogrid_gid, sep="_"))

#Sometimes there is more than one conflict for one month_year in the same prio grid. 
#Summarize the fatalities such that for every grid and every year_month there is one observation
a<-a %>% group_by(key, country, year_month, priogrid_gid) %>% summarise(best= sum(best))
colnames(a)[4]<-"pg_id"
#Now we are interested only in the fatalities in January 2021
a_jan<- a %>% filter(year_month == "2021-01")

#-------------------------------------------------------------------------------------------------------------------
#Plot
#Assign Path
path_data_icews_pgm<-"~/ICEWS-Project/Data/data_icews_pgm"
path_folder_cellshp<-"~/ICEWS-Project/Data/priogrid_cellshp"

#Read Polygons
prio_grid_polygons <- st_read(dsn = path_folder_cellshp, layer = "priogrid_cell", stringsAsFactors = F, quiet=T) %>% mutate(gid = as.character(gid))

#Create African map

states_africa<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso (Upper Volta)", "Burundi","Cape Verde", "Cameroon","Central African Republic",
                 "Chad", "Comoros","Congo", "Congo, Democratic Republic of (Zaire)","Cote D'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia",
                 "Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar (Malagasy)","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
                 "Nigeria", "Rwanda","Sao Tome and Principe ","Swaziland (Eswatini)","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
                 "Sudan", "Tanzania (Tanganyika)","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe (Rhodesia)" )

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

#Rename Columns
colnames(africa_polygons)[1]<-"pg_id"
africa_polygons$pg_id<-as.integer(africa_polygons$pg_id)

#Assign every Polygon to a relative Number
count_events_poly_pred<-left_join(africa_polygons, Jan_2021)
counts_events_ucdp<-left_join(africa_polygons, a_jan)

#------------------------------------------------------------
#Assign the zeros to NA
count_events_poly_pred$prediction[count_events_poly_pred$prediction == 0] <- NA

ggplot() + 
  geom_sf(data = count_events_poly_pred, aes(fill = prediction),col = NA) +
  geom_sf(data=map, col="black", fill= NA)+
  xlab("Longitude")+ylab("Latitude")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_fill_gradient2(low="#91bfeb", mid="#91bfeb", high = "#ff000d")+
  theme(legend.position="right",       
        legend.key.height = unit(10,"cm")) +
  guides(fill=guide_colorbar(title.vjust=2.5))+
  theme_classic(base_size = 16)

ggplot() + 
  geom_sf(data = counts_events_ucdp , aes(fill = best),col = NA) +
  geom_sf(data=map, col="black", fill= NA)+
  xlab("Longitude")+ylab("Latitude")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_fill_gradient2(low="#91bfeb", mid="#91bfeb", high = "#ff000d")+
  theme(legend.position="right",       
        legend.key.height = unit(10,"cm")) +
  guides(fill=guide_colorbar(title.vjust=2.5))+
  theme_classic(base_size = 16)
