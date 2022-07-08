############################################### The Dynamic Process ############################################

#Remark: plot the dynamic of escalation

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
library(expss)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##################
#Data Preparation
##################

#Set working directory

#for Clara
setwd("~/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa/Data sets")

#Load data set
cm_data = fread("cm_icews_data.csv")

#Order columns
cm_data<- cm_data %>% dplyr::relocate( gov_reb_low_level, reb_gov_low_level, opp_gov_low_level, .after = gov_opp_low_level)
cm_data<- cm_data %>% dplyr::relocate( gov_reb_accommodations, .after = gov_opp_accommodations)
cm_data<- cm_data %>% dplyr::relocate( gov_reb_nonviol_repression, .after = gov_opp_nonviol_repression)
cm_data<- cm_data %>% dplyr::relocate( reb_gov_demands, .after = opp_gov_demands)
cm_data<- cm_data %>% dplyr::relocate( reb_gov_demands, opp_gov_demands, .after = opp_gov_low_level)
cm_data<- cm_data %>% dplyr::relocate( gov_reb_accommodations, gov_opp_accommodations, .after =opp_gov_demands)

#Replace NA with 0 (missing events on country-month level are interpreted as 0 events)
cm_data[is.na(cm_data),]<-0

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##################
#Plot: Burundi 2015
##################


#Filter for Burundi 2015-04 till 2015-06
cm_data_bur<- cm_data%>% filter(country_name=="Burundi") %>% filter(year_month=="2015-04" |year_month=="2015-05" | year_month=="2015-06")
cm_data_bur<- cm_data_bur[,c(3,30:39)]
cm_data_bur2<- cm_data_bur %>%
  pivot_longer(!year_month, values_to = "count")

cm_data_bur2$name <- recode_factor(cm_data_bur2$name, reb_gov_demands="Reb-Gov Demands", 
                                   opp_gov_demands="Opp-Gov Demands",
                                   gov_reb_nonviol_repression="Gov-Reb Non-Violent Repression",
                                   gov_opp_nonviol_repression="Gov-Opp Non-Violent Repression",
                                   gov_reb_accommodations="Gov-Reb Accommodation",
                                   gov_opp_accommodations="Gov-Opp Accommodation",
                                   gov_reb_low_level="Gov-Reb Low Level Violence",
                                   gov_opp_low_level="Gov-Opp Low Level Violence",
                                   opp_gov_low_level="Opp-Gov Low Level Violence",
                                   reb_gov_low_level="Reb-Gov Low Level Violence")


a<-cm_data_bur2%>% filter(year_month=="2015-04")%>% ggplot(aes(y=name, x=count)) +
  geom_bar(stat="identity",aes(fill=name)) +
  scale_x_continuous(limits=c(0,100),breaks =seq(0,120,10))+
  ggtitle("Burundi (2015-04)")+
  xlab("Absolute Frequency") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
                                                  panel.background = element_blank(),
                                                  axis.line = element_line(colour = "black"),
                                                  axis.title.x = element_text(hjust=0.5, size=16),
                                                  axis.title.y= element_blank(),
                                                  axis.text = element_text(size=12,colour = "black"),
                                                  legend.position = "none")

b<-cm_data_bur2%>% filter(year_month=="2015-05")%>% ggplot(aes(y=name, x=count)) +
  geom_bar(stat="identity",aes(fill=name)) +
  scale_x_continuous(limits=c(0,100),breaks =seq(0,120,10))+
  ggtitle("Burundi (2015-05)")+
  xlab("Absolute Frequency") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_blank(),
        axis.text = element_text(size=12,colour = "black"),
        legend.position = "none")

c<-cm_data_bur2%>% filter(year_month=="2015-06")%>% ggplot(aes(y=name, x=count)) +
  geom_bar(stat="identity",aes(fill=name)) +
  scale_x_continuous(limits=c(0,100),breaks =seq(0,120,10))+
  ggtitle("Burundi (2015-06)")+
  xlab("Absolute Frequency") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_blank(),
        axis.text = element_text(size=12,colour = "black"),
        legend.position = "none")


ggsave(a, filename = "/Users/clarita/Desktop/Consulting\ Bewaffnete\ Konflikte/Descriptives\ and\ Plots/Plots/Escalation\ Plots/Dynamics/dynamic_burundi_4.png")
ggsave(b, filename = "/Users/clarita/Desktop/Consulting\ Bewaffnete\ Konflikte/Descriptives\ and\ Plots/Plots/Escalation\ Plots/Dynamics/dynamic_burundi_5.png")
ggsave(c, filename = "/Users/clarita/Desktop/Consulting\ Bewaffnete\ Konflikte/Descriptives\ and\ Plots/Plots/Escalation\ Plots/Dynamics/dynamic_burundi_6.png")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##################
#Plot: Egypt 2011
##################


#Filter for Egypt 2010-12 till 2011-02
cm_data_egy<- cm_data%>% filter(country_name=="Egypt") %>% filter(year_month=="2010-12" |year_month=="2011-01" | year_month=="2011-02")
cm_data_egy<- cm_data_egy[,c(3,30:39)]
cm_data_egy2<- cm_data_egy %>%
  pivot_longer(!year_month, values_to = "count")

cm_data_egy2$name <- recode_factor(cm_data_egy2$name, reb_gov_demands="Reb-Gov Demands", 
                                   opp_gov_demands="Opp-Gov Demands",
                                   gov_reb_nonviol_repression="Gov-Reb Non-Violent Repression",
                                   gov_opp_nonviol_repression="Gov-Opp Non-Violent Repression",
                                   gov_reb_accommodations="Gov-Reb Accommodation",
                                   gov_opp_accommodations="Gov-Opp Accommodation",
                                   gov_reb_low_level="Gov-Reb Low Level Violence",
                                   gov_opp_low_level="Gov-Opp Low Level Violence",
                                   opp_gov_low_level="Opp-Gov Low Level Violence",
                                   reb_gov_low_level="Reb-Gov Low Level Violence")


a<-cm_data_egy2%>% filter(year_month=="2010-12")%>% ggplot(aes(y=name, x=count)) +
  geom_bar(stat="identity",aes(fill=name)) +
  scale_x_continuous(limits=c(0,150),breaks =seq(0,160,10))+
  ggtitle("Egypt (2010-12)")+
  xlab("Absolute Frequency") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_blank(),
        axis.text = element_text(size=12,colour = "black"),
        legend.position = "none")

b<-cm_data_egy2%>% filter(year_month=="2011-01")%>% ggplot(aes(y=name, x=count)) +
  geom_bar(stat="identity",aes(fill=name)) +
  scale_x_continuous(limits=c(0,150),breaks =seq(0,160,10))+
  ggtitle("Egypt (2011-01)")+
  xlab("Absolute Frequency") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_blank(),
        axis.text = element_text(size=12,colour = "black"),
        legend.position = "none")

c<-cm_data_egy2%>% filter(year_month=="2011-02")%>% ggplot(aes(y=name, x=count)) +
  geom_bar(stat="identity",aes(fill=name)) +
  scale_x_continuous(limits=c(0,150),breaks =seq(0,160,10))+
  ggtitle("Egypt (2011-02)")+
  xlab("Absolute Frequency") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_blank(),
        axis.text = element_text(size=12,colour = "black"),
        legend.position = "none")


ggsave(a, filename = "/Users/clarita/Desktop/Consulting\ Bewaffnete\ Konflikte/Descriptives\ and\ Plots/Plots/Escalation\ Plots/Dynamics/dynamic_egypt_12.png")
ggsave(b, filename = "/Users/clarita/Desktop/Consulting\ Bewaffnete\ Konflikte/Descriptives\ and\ Plots/Plots/Escalation\ Plots/Dynamics/dynamic_egypt_1.png")
ggsave(c, filename = "/Users/clarita/Desktop/Consulting\ Bewaffnete\ Konflikte/Descriptives\ and\ Plots/Plots/Escalation\ Plots/Dynamics/dynamic_egypt_2.png")








