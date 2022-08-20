############################################### Summary Statistics and Further Descriptives ############################################

#Load necessary packages
library(data.table)
library(dplyr)
library(xtable)
library(stargazer)
library(moments)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(viridis)
library(cshapes)
library(sf)
library(corrplot)
library(ggplot2)
library(ggcorrplot)


#Assign path
path_cm_icews_data<-"~/ICEWS-Project/Data/cm_icews_data.csv"

path_plots<- "~/ICEWS-Project/2. Descriptive Analysis/Plots"


#-------------------------------------------------------------------------------------------------------------------------------------#
########################
#Data Preparation
########################

#Load data sets
cm_data <- fread(path_cm_icews_data)

#Replace NA with 0 (missing events on country-month level are interpreted as 0 events)
cm_data[is.na(cm_data),]<-0

#Order columns
cm_data<- cm_data %>% dplyr::relocate( gov_reb_low_level, reb_gov_low_level, opp_gov_low_level, .after = gov_opp_low_level)
cm_data<- cm_data %>% dplyr::relocate( gov_reb_accommodations, .after = gov_opp_accommodations)
cm_data<- cm_data %>% dplyr::relocate( gov_reb_nonviol_repression, .after = gov_opp_nonviol_repression)
cm_data<- cm_data %>% dplyr::relocate( reb_gov_demands, .after = opp_gov_demands)
cm_data<- cm_data %>% dplyr::relocate( reb_gov_demands, opp_gov_demands, .after = opp_gov_low_level)
cm_data<- cm_data %>% dplyr::relocate( gov_reb_accommodations, gov_opp_accommodations, .after =opp_gov_demands)

#-------------------------------------------------------------------------------------------------------------------------------------#
#################################
#The Missings
#################################

#See unique country names
country_names<-as.data.frame(unique(cm_data$country_name))

#Generate data frame with missings
colSums(is.na(cm_data))
cm_data_icews_na <- cm_data[rowSums(is.na(cm_data)) > 0,]
#save as csv
#write.csv(cm_data_icews_na, file="cm_data_icews_na.csv", row.names = F)

#Do all variables have the same number of missings?
colSums(is.na(cm_data_icews_na))

#How many missings does each country have?
missings_cm<-as.data.frame(table(sort((cm_data_icews_na$country_name))))

#Generate table with country and missing months (for latex)
missings_cm<-missings_cm[order(missings_cm$Freq),]
print(xtable(missings_cm, type = "latex"), file = "missings_cm.tex")

#Generate Plot with missings by country
missings_cm$Freq<-missings_cm$Freq/12
missings_cm%>%arrange(Freq)%>%mutate(Var1=factor(Var1,levels = Var1))%>%
  ggplot(aes(x=Freq, y=Var1)) +
  geom_point(size=1)+
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

#-------------------------------------------------------------------------------------------------------------------------------------#
#################################
#Summary Statistics
#################################

#Generate summary statistics table
stargazer(cm_data, summary.stat = c("mean", "median", "min", "max", "sd", "p25", "p75"))

#Generate skewness table (compare linear vs. log transformation)
cm_data_sub<-cm_data[,30:39]
a<-as.data.frame(skewness(cm_data_sub))
b<-as.data.frame(skewness(log1p(cm_data_sub)))
a<-cbind(a,b)
a
#Save Table
print(xtable(a, type = "latex"), file = paste(path_pgm_icews_data, "/skewness_comp.tex", sep=""))


#Generate distribution plots for each Variable

# Plot for reb_gov_low_level
a<-cm_data%>% filter(reb_gov_low_level>"0") %>% ggplot(aes(reb_gov_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$reb_gov_low_level)+1), breaks =seq(0,max(cm_data$reb_gov_low_level)+1,10))+
  ylab("Freq") + xlab("Reb-Gov Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Plot for opp_gov_low_level
b<-cm_data%>% filter(opp_gov_low_level>"0") %>% ggplot(aes(opp_gov_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$opp_gov_low_level)+1), breaks =seq(0,max(cm_data$opp_gov_low_level)+1,10))+
  ylab("Freq") + xlab("Opp-Gov Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Plot for gov_reb_low_level
c<-cm_data%>% filter(gov_reb_low_level>"0") %>% ggplot(aes(gov_reb_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, 10), breaks =seq(0,10))+
  ylab("Freq") + xlab("Opp-Reb Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Plot for gov_opp_low_level
d<-cm_data%>% filter(gov_opp_low_level>"0") %>% ggplot(aes(gov_opp_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_low_level)+1), breaks =seq(0,max(cm_data$gov_opp_low_level)+1,10))+
  ylab("Freq") + xlab("Gov-Opp Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Plot for gov_opp_accommodations
e<-cm_data%>% filter(gov_opp_accommodations>"0") %>% ggplot(aes(gov_opp_accommodations)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_accommodations)+1), breaks =seq(0,max(cm_data$gov_opp_accommodations)+1,1))+
  ylab("Freq") + xlab("Gov-Opp Accommodation Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Plot for gov_reb_accommodations
f<-cm_data%>% filter(gov_reb_accommodations>"0") %>% ggplot(aes(gov_reb_accommodations)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_reb_accommodations)+1), breaks =seq(0,max(cm_data$gov_reb_accommodations)+1,1))+
  ylab("Freq") + xlab("Gov-Reb Accommodation Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Plot for gov_opp_nonviol_repression
g<-cm_data%>% filter(gov_opp_nonviol_repression>"0") %>% ggplot(aes(gov_opp_nonviol_repression)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_nonviol_repression)+1), breaks =seq(0,max(cm_data$gov_opp_nonviol_repression)+1,1))+
  ylab("Freq") + xlab("Gov-Opp Non-Violent Repression Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

#Plot for gov_reb_nonviol_repression
h<-cm_data%>% filter(gov_reb_nonviol_repression>"0") %>% ggplot(aes(gov_reb_nonviol_repression)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_reb_nonviol_repression)+1), breaks =seq(0,max(cm_data$gov_reb_nonviol_repression)+1,1))+
  ylab("Freq") + xlab("Gov-Reb Non-Violent Repression Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))


#All Plots in one 
p<-grid.arrange(a,b,c,d,e,f,g,h,ncol=2,nrow=4)
#Display Plots
p

#Save Plot
ggsave(p,filename=paste(path_plots, "/Freq_Events_Esc.png", sep=""))


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
###########################
#Time Series: One Country
###########################


#Filter for example for  Burundi 2015-04 till 2015-06
#Choose a Country
country<- "Burundi"
#Choose three months
m<- c("2015-04","2015-05","2015-06")

cm_data_filt<- cm_data %>% filter(country_name == country) %>% filter(year_month== m[1] | year_month== m[2] | year_month== m[3])
cm_data_filt<- cm_data_filt[,c(3,30:39)]
cm_data_filt<- cm_data_filt %>% pivot_longer(!year_month, values_to = "count")

cm_data_filt$name <- recode_factor(cm_data_filt$name, reb_gov_demands="Reb-Gov Demands", 
                                   opp_gov_demands="Opp-Gov Demands",
                                   gov_reb_nonviol_repression="Gov-Reb Non-Violent Repression",
                                   gov_opp_nonviol_repression="Gov-Opp Non-Violent Repression",
                                   gov_reb_accommodations="Gov-Reb Accommodation",
                                   gov_opp_accommodations="Gov-Opp Accommodation",
                                   gov_reb_low_level="Gov-Reb Low Level Violence",
                                   gov_opp_low_level="Gov-Opp Low Level Violence",
                                   opp_gov_low_level="Opp-Gov Low Level Violence",
                                   reb_gov_low_level="Reb-Gov Low Level Violence")


a<-cm_data_filt%>% filter(year_month== m[1])%>% ggplot(aes(y=name, x=count)) +
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

b<-cm_data_filt%>% filter(year_month==m[2])%>% ggplot(aes(y=name, x=count)) +
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

c<-cm_data_filt%>% filter(year_month==m[3])%>% ggplot(aes(y=name, x=count)) +
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

#Display Plots
a
b
c

#Save Plots
ggsave(a, filename = paste(path_plots, "/dynamic_burundi_4.png", sep=""))
ggsave(b, filename = paste(path_plots,"/dynamic_burundi_5.png", sep=""))
ggsave(c, filename = paste(path_plots,"/dynamic_burundi_6.png", sep=""))




#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##############################
#Time Series: Egypt 2001-2012
##############################

#Plot for each escalation covariate the Evolution of the absolute frequency event number over the months
#lines indicate whether a state-based fatality in CM happened or not

#Choose a Country
country<- "Egypt"

cm_egy<-cm_data %>% filter(country_name== country,year %in% (2013:2016))

#Plot for reb_gov_low_level
a<-ggplot(cm_egy, aes(year_month,reb_gov_low_level))+ geom_point()+ geom_line(aes(group=1))+
  geom_vline(aes(xintercept = year_month), data = ~ filter(cm_data,country_name=="Egypt",year %in% (2013:2016),ged_dummy_sb=="TRUE"),linetype="dotted")+
  xlab("Year")+ ylab("Freq")+
  ggtitle("Reb-Gov Low Level Violence")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_x_discrete(breaks = unique(cm_egy$year_month)[seq(1,48,6)])

#Plot for gov_reb_low_level
b<-ggplot(cm_egy, aes(year_month,gov_reb_low_level))+ geom_point()+ geom_line(aes(group=1))+
  geom_vline(aes(xintercept = year_month), data = ~ filter(cm_data,country_name=="Egypt",year %in% (2013:2016),ged_dummy_sb=="TRUE"),linetype="dotted")+
  xlab("Year")+ ylab("Freq")+
  ggtitle("Gov-Reb Low Level Violence")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_x_discrete(breaks = unique(cm_egy$year_month)[seq(1,48,6)])

#Plot for opp_gov_low_level
c<-ggplot(cm_egy, aes(year_month,opp_gov_low_level))+ geom_point()+ geom_line(aes(group=1))+
  geom_vline(aes(xintercept = year_month), data = ~ filter(cm_data,country_name=="Egypt",year %in% (2013:2016),ged_dummy_sb=="TRUE"),linetype="dotted")+
  xlab("Year")+ ylab("Freq")+
  ggtitle("Opp-Gov Low Level Violence")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_x_discrete(breaks = unique(cm_egy$year_month)[seq(1,48,6)])

#Plot for gov_opp_low_level
d<-ggplot(cm_egy, aes(year_month,gov_opp_low_level))+ geom_point()+ geom_line(aes(group=1))+
  geom_vline(aes(xintercept = year_month), data = ~ filter(cm_data,country_name=="Egypt",year %in% (2013:2016),ged_dummy_sb=="TRUE"),linetype="dotted")+
  xlab("Year")+ ylab("Freq")+
  ggtitle("Gov-Opp Low Level Violence")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_x_discrete(breaks = unique(cm_egy$year_month)[seq(1,48,6)])


#Plot for gov_opp_accommodations
e<-ggplot(cm_egy, aes(year_month,gov_opp_accommodations))+ geom_point()+ geom_line(aes(group=1))+
  geom_vline(aes(xintercept = year_month), data = ~ filter(cm_data,country_name=="Egypt",year %in% (2013:2016),ged_dummy_sb=="TRUE"),linetype="dotted")+
  xlab("Year")+ ylab("Freq")+
  ggtitle("Gov-Opp Accommodations")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_x_discrete(breaks = unique(cm_egy$year_month)[seq(1,48,6)])

#Plot for gov_reb_accommodations
f<-ggplot(cm_egy, aes(year_month,gov_reb_accommodations))+ geom_point()+ geom_line(aes(group=1))+
  geom_vline(aes(xintercept = year_month), data = ~ filter(cm_data,country_name=="Egypt",year %in% (2013:2016),ged_dummy_sb=="TRUE"),linetype="dotted")+
  xlab("Year")+ ylab("Freq")+
  ggtitle("Gov-Reb Accommodations")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_x_discrete(breaks = unique(cm_egy$year_month)[seq(1,48,6)])+
  scale_y_continuous(limits = c(0, 6), breaks = c(0,2,4,6))

#Plot for gov_opp_nonviol_repression
g<-ggplot(cm_egy, aes(year_month,gov_opp_nonviol_repression))+ geom_point()+ geom_line(aes(group=1))+
  geom_vline(aes(xintercept = year_month), data = ~ filter(cm_data,country_name=="Egypt",year %in% (2013:2016),ged_dummy_sb=="TRUE"),linetype="dotted")+
  xlab("Year")+ ylab("Freq")+
  ggtitle("Gov-Opp Non-Violent Repression")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_x_discrete(breaks = unique(cm_egy$year_month)[seq(1,48,6)])+
  scale_y_continuous(limits = c(0, 3), breaks = c(0,1,2,3))

#Plot for gov_reb_nonviol_repression
h<-ggplot(cm_egy, aes(year_month,gov_reb_nonviol_repression))+ geom_point()+ geom_line(aes(group=1))+
  geom_vline(aes(xintercept = year_month), data = ~ filter(cm_data,country_name=="Egypt",year %in% (2013:2016),ged_dummy_sb=="TRUE"),linetype="dotted")+
  xlab("Year")+ ylab("Freq")+
  ggtitle("Gov-Reb Non-Violent Repression")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black")) +
  scale_x_discrete(breaks = unique(cm_egy$year_month)[seq(1,48,6)])


#Arrange all Plots in one graphic
p<-grid.arrange(a,b,c,d, e, f,g,h,ncol=2,nrow=4)

#Display Plots
p

#Save  Plots
ggsave(p,filename=paste(path_plots, "/Egypt_Escalation_Time_Series.png", sep=""))

#-------------------------------------------------------------------------------------------------------------------------------------#
#################################
# Correlation Plot
#################################

#Plot correlation between escalation proxies
corr_data<-cm_data[,30:39]
cor<-round(cor(corr_data),3)
head(cor)
ggcorrplot(cor, hc.order = TRUE, type = "lower",
           outline.col = "white")


#-------------------------------------------------------------------------------------------------------------
######################################
# Map for Variables
######################################

#Summarize the events for each variable in each country
count_events_country<-cm_data %>%
  as.data.frame() %>%
  group_by(country_name) %>%
  summarise(gov_opp_low_level = sum(gov_opp_low_level), 
            reb_gov_demands= sum(reb_gov_demands),
            opp_gov_demands= sum(opp_gov_demands),
            gov_opp_accommodations = sum(gov_opp_accommodations),
            gov_reb_accommodations = sum(gov_reb_accommodations),
            gov_opp_nonviol_repression = sum(gov_opp_nonviol_repression),
            gov_reb_nonviol_repression = sum(gov_reb_nonviol_repression),
            reb_gov_low_level = sum(reb_gov_low_level),
            opp_gov_low_level = sum(opp_gov_low_level),
            gov_reb_low_level = sum(gov_reb_low_level))


#Summarize all Events across all Variables in each country
count_events_country$n<-rowSums(count_events_country[,2:11])


#Make Dataframe with the Relative Frequency
count_events_country_rel<- data.frame(
  country_name= count_events_country$country_name,
  gov_opp_low_level = count_events_country$gov_opp_low_level/count_events_country$n,
  reb_gov_demands= count_events_country$reb_gov_demands/count_events_country$n,
  opp_gov_demands= count_events_country$opp_gov_demands/count_events_country$n,
  gov_opp_accommodations = count_events_country$gov_opp_accommodations/count_events_country$n,
  gov_reb_accommodations =  count_events_country$gov_reb_accommodations/count_events_country$n,
  gov_opp_nonviol_repression = count_events_country$gov_opp_nonviol_repression/count_events_country$n,
  gov_reb_nonviol_repression = count_events_country$gov_reb_nonviol_repression/count_events_country$n,
  reb_gov_low_level = count_events_country$reb_gov_low_level/count_events_country$n,
  opp_gov_low_level = count_events_country$opp_gov_low_level/count_events_country$n,
  gov_reb_low_level = count_events_country$gov_reb_low_level/count_events_country$n)



#create African map
states<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso (Upper Volta)", "Burundi","Cape Verde", "Cameroon","Central African Republic",
          "Chad", "Comoros","Congo", "Congo, Democratic Republic of (Zaire)","Cote D'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia",
          "Ghana","Guinea","Guinea-Bissau","Israel","Kenya","Lesotho","Liberia","Libya","Madagascar (Malagasy)","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
          "Nigeria", "Rwanda", "Swaziland (Eswatini)","Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia", "South Africa", "South Sudan",
          "Sudan","Tanzania (Tanganyika)","Togo","Tunisia", "Uganda", "Zambia", "Zimbabwe (Rhodesia)" )

africa <- cshp(date=as.Date("2012-1-01"), useGW=TRUE)
africa <- st_as_sf(africa, sf_use_s2(FALSE))
africa <- africa[africa$country_name %in% states,]

africa<- africa %>% 
  mutate(country_name = replace(country_name, country_name ==  "Burkina Faso (Upper Volta)", "Burkina Faso"),
         country_name = replace(country_name, country_name ==  "Congo, Democratic Republic of (Zaire)"  ,"Democratic Republic of Congo"),
         country_name = replace(country_name, country_name ==  "Cote D'Ivoire", "Cote d'Ivoire"),
         country_name = replace(country_name, country_name ==  "Madagascar (Malagasy)", "Madagascar"),
         country_name = replace(country_name, country_name ==  "Swaziland (Eswatini)", "Swaziland"),
         country_name = replace(country_name, country_name ==  "Tanzania (Tanganyika)", "Tanzania"),
         country_name = replace(country_name, country_name ==  "The Gambia", "Gambia"),
         country_name = replace(country_name, country_name ==  "Zimbabwe (Rhodesia)", "Zimbabwe"))


#Join Data with relative Frequency and African Map
data<- left_join(africa, count_events_country_rel)

#We can do the plot with all Variables, change fill= reb_gov_low_level
#GGPLOT
ggplot(filter(data))+
  geom_sf(aes(group=country_name, fill= reb_gov_low_level))+
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
  scale_fill_viridis(option = "D", discrete = F, direction=1, breaks= c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 0.25, 0.5, 0.75, 1), 
                     limits= c(0,1))+
  guides(fill=guide_colorbar(title.vjust=2.5))+
  theme_classic(base_size = 16)



ggsave(filename = paste(path_plots,"/Map_escalation_variable.png", sep=""))

#--------------------------------------------------------------------------------------------------------
######################################
# Correlation Plot 
#####################################

#Plot correlation between escalation proxies

#Round Correlation 
cor<-round(cor(cm_data_sub),3)

#Display Correlation Matrix
head(cor)

#Make Correlation Plot
ggcorrplot(cor, hc.order = TRUE, type = "lower",
           outline.col = "white")

ggsave(filename = paste(path_plots,"/correlation_plot.png", sep=""))
