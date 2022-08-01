#############################################
############## Animation Plot  ##############
#############################################

count_events_year<-data_icews_pgm %>%
  as.data.frame() %>%
  group_by(gid, Country, year) %>%
  count()

count_events_poly_year<-left_join(africa_polygons,count_events_year)

#Assign NA to 0
count_events_poly_year[is.na(count_events_poly_year)] <- 0

plot<-ggplot(data=map) + 
  geom_sf(col = "black", alpha = 0.00001) +
  geom_sf(data = count_events_poly_year, aes(fill = n, col=year),col = "grey") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right") +
  scale_fill_viridis(option = "B","Absolute Frequency",discrete = F, direction = -1)



plot.animation<-plot + transition_time(year)+ labs(subtitle = "Year: {frame_time}")
animate(plot.animation)

anim_save("animation_map.gif", path=path)

#Absolute Events Polygons where n > 5
count_events_poly_year_5<-count_events_poly_year %>% filter(n > 5)


plot<-ggplot(data=map) + 
  geom_sf(col = "black", alpha = 0.00001) +
  geom_sf(data = count_events_poly_year_all, aes(fill = n),col = "grey") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  theme(legend.position="right") +
  scale_fill_viridis(option = "B","Absolute Frequency",discrete = F, direction = -1)


plot.animation<-plot + transition_time(year)+ labs(subtitle = "Year: {frame_time}")
animate(plot.animation)

anim_save("animation_map.gif", path=path)

#Code der Mädels für Animation
#animation<-animate(plot, nframes= length(unique(count_events_poly_year$year))*2)