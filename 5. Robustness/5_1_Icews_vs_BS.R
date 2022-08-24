#################################################### ROBUSTNESS CHECK: ICEWS VS. BLAIR AND SAMBANIS (2020) ########################

#Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)

#Set paths
path_data_escalation<- "~/ICEWS-Project/Data/data_escalation.csv"
path_data_BlairSambanis<-"~/ICEWS-Project/Data/cm_data_BlairSambanis.csv"
path_data_icews_cm<-"~/ICEWS-Project/Data/data_icews_cm.csv"

#Set path for Plots
path_plots<- "~/ICEWS-Project/5. Robustness/Plots/"
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
############################################
#Load Data Sets
###########################################

#By Blair and Samabanis (2020)
X1mo_data <-read.csv(path_data_BlairSambanis)

#Escalation Data on CM level
data_sum<-fread(path_data_escalation)
data<-fread(path_data_icews_cm)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###################################################
#Compare Escalation Variable Values with BS (2020)
##################################################

#Set data to equal country names
data<- data %>% mutate(Country = replace(Country, Country ==   "The Gambia"  ,"Gambia"),
                             Country = replace(Country, Country ==   "Congo, DRC"  ,"Democratic Republic of Congo"))

#Filter BS (2020) data set
X1mo_data_check<-X1mo_data %>% select(year, month,country_name, gov_opp_accommodations, gov_opp_low_level, gov_opp_nonviol_repression, opp_gov_demands, opp_gov_low_level,gov_reb_accommodations, gov_reb_low_level, gov_reb_nonviol_repression, reb_gov_demands, reb_gov_low_level ) %>%
  filter(country_name %in%  levels(factor(data$Country))) %>%
  mutate(key_cameo= paste(year, month, country_name, sep = "_"))

#Keep observations from 2001 to 2015
data_sum_filtered <- data_sum %>% semi_join(X1mo_data_check, by="key_cameo")

#Compare both data sets
escalation_variables<- c("key_cameo",
                         "gov_opp_accommodations",
                         "gov_opp_low_level",
                         "gov_opp_nonviol_repression",
                         "opp_gov_demands",
                         "opp_gov_low_level",
                         "gov_reb_accommodations",
                         "gov_reb_low_level",
                         "gov_reb_nonviol_repression",
                         "reb_gov_demands",
                         "reb_gov_low_level")

#Data set with all observations that are in ICEWS but not in Blair and Sambanis (2020)
data_sum_not_in_BS<- data_sum_filtered %>% anti_join(X1mo_data_check, by=escalation_variables)

#Data set with all observations that are in ICEWS and in Blair and Sambanis (2020)
data_sum_in_BS<- data_sum_filtered %>% semi_join(X1mo_data_check, by=escalation_variables)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###############################################
#Plot Difference to BS (2020)
###############################################

#Generate two data sets: BS and ICEWS
data_diff<- data_sum %>% filter(key_cameo %in% data_sum_not_in_BS$key_cameo)
X1_diff<- X1mo_data_check %>% filter(key_cameo %in% data_sum_not_in_BS$key_cameo)

#Keep relevant columns
X1_diff<-X1_diff[,4:14]

#Order both data sets by key_cameo
X1_diff<-X1_diff[order(X1_diff$key_cameo),]
data_diff<-data_diff[order(data_diff$key_cameo),]


#Generate difference between data sets
diff<- data.frame(idx = seq(1,792, by= 1),
                  key_cameo=data_diff$key_cameo,
                  gov_opp_accommodations= (data_diff$gov_opp_accommodations - X1_diff$gov_opp_accommodations),
                  gov_opp_low_level= (data_diff$gov_opp_low_level - X1_diff$gov_opp_low_level),
                  gov_opp_nonviol_repression= (data_diff$gov_opp_nonviol_repression - X1_diff$gov_opp_nonviol_repression),
                  opp_gov_demands= (data_diff$opp_gov_demands - X1_diff$opp_gov_demands),
                  opp_gov_low_level= (data_diff$opp_gov_low_level - X1_diff$opp_gov_low_level),
                  gov_reb_low_level= (data_diff$gov_reb_low_level - X1_diff$gov_reb_low_level),
                  gov_reb_accommodations= (data_diff$gov_reb_accommodations - X1_diff$gov_reb_accommodations),
                  gov_reb_nonviol_repression= (data_diff$gov_reb_nonviol_repression - X1_diff$gov_reb_nonviol_repression),
                  reb_gov_demands= (data_diff$reb_gov_demands - X1_diff$reb_gov_demands),
                  reb_gov_low_level= (data_diff$reb_gov_low_level - X1_diff$reb_gov_low_level))

#Generate Plots

#Demands
p<-ggplot(diff, aes(idx,opp_gov_demands))+geom_point()+
  scale_y_continuous(limits = c(-10, 10), breaks = c(-10:10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

p1<-ggplot(diff, aes(idx,reb_gov_demands))+geom_point()+
  scale_y_continuous(limits = c(-10, 10), breaks = c(-10:10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


#Accommodation
p2<-ggplot(diff, aes(idx,gov_opp_accommodations))+geom_point()+
  scale_y_continuous(limits = c(-10, 10), breaks = c(-10:10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

p3<-ggplot(diff, aes(idx,gov_reb_accommodations))+geom_point()+
  scale_y_continuous(limits = c(-10, 10), breaks = c(-10:10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


#Non-Violent Repression
p4<-ggplot(diff, aes(idx,gov_opp_nonviol_repression))+geom_point()+
  scale_y_continuous(limits = c(-10, 10), breaks = c(-10:10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

p5<-ggplot(diff, aes(idx,gov_reb_nonviol_repression))+geom_point()+
  scale_y_continuous(limits = c(-10, 10), breaks = c(-10:10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


#Low Level Violence
p6<-ggplot(diff, aes(idx,opp_gov_low_level))+geom_point()+
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100,100,10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

p7<-ggplot(diff, aes(idx,reb_gov_low_level))+geom_point()+
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100,100,10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

p8<-ggplot(diff, aes(idx,gov_opp_low_level))+geom_point()+
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100,100,10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

p9<-ggplot(diff, aes(idx,gov_reb_low_level))+geom_point()+
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100,100,10))+
  scale_x_continuous(limits = c(0, 828), breaks =seq(0,828,100))+
  ylab("Event Number Difference") + xlab("Index") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


#Grid arrange plots

#demands
ggsave(p,filename=paste0(path_plots, "opp_gov_demands.png"))
ggsave(p1,filename=paste0(path_plots,"reb_gov_demands.png"))

#accommodation
ggsave(p2,filename=paste0(path_plots,"gov_opp_acc.png"))
ggsave(p3,filename=paste0(path_plots,"gov_reb_acc.png"))

#non-violent
ggsave(p4,filename=paste0(path_plots,"gov_opp_non.png"))
ggsave(p5,filename=paste0(path_plots,"gov_reb_non.png"))

#low-level violence
ggsave(p6,filename=paste0(path_plots,"opp_gov_low.png"))
ggsave(p7,filename=paste0(path_plots,"reb_gov_low.png"))
ggsave(p8,filename=paste0(path_plots,"gov_opp_low.png"))
ggsave(p9,filename=paste0(path_plots,"gov_reb_low.png"))




