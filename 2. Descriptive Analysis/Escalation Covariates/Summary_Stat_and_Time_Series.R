############################################### Summary Statistics and Further Descriptives ############################################

#Load necessary packages
library(mgcv)
library(MASS)
library(grid)
library(data.table)
library(countreg)
library(lubridate)
library(pryr)
library(DEoptim)
library(dplyr)
library(xtable)
library(stargazer)
library(moments)

#Set working directory

#-------------------------------------------------------------------------------------------------------------------------------------#
########################
#Data Preparation
########################

#Load data sets
cm_data = fread("cm_icews_data.csv")
pgm_data = fread("pgm_icews_data.csv")

#Replace NA with 0 (missing events on country-month level are interpreted as 0 events)
cm_data[is.na(cm_data),]<-0
pgm_data[is.na(pgm_data),]<-0

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

#See unique country-months with missings
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
print(xtable(a, type = "latex"), file = "skewness_comp.tex")


#Generate distribution plots

a<-cm_data%>% filter(reb_gov_low_level>"0") %>% ggplot(aes(reb_gov_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$reb_gov_low_level)+1), breaks =seq(0,max(cm_data$reb_gov_low_level)+1,10))+
  ylab("Freq") + xlab("Reb-Gov Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

b<-cm_data%>% filter(opp_gov_low_level>"0") %>% ggplot(aes(opp_gov_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$opp_gov_low_level)+1), breaks =seq(0,max(cm_data$opp_gov_low_level)+1,10))+
  ylab("Freq") + xlab("Opp-Gov Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

c<-cm_data%>% filter(gov_reb_low_level>"0") %>% ggplot(aes(gov_reb_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, 10), breaks =seq(0,10))+
  ylab("Freq") + xlab("Opp-Reb Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

d<-cm_data%>% filter(gov_opp_low_level>"0") %>% ggplot(aes(gov_opp_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_low_level)+1), breaks =seq(0,max(cm_data$gov_opp_low_level)+1,10))+
  ylab("Freq") + xlab("Gov-Opp Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

e<-cm_data%>% filter(gov_opp_accommodations>"0") %>% ggplot(aes(gov_opp_accommodations)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_accommodations)+1), breaks =seq(0,max(cm_data$gov_opp_accommodations)+1,1))+
  ylab("Freq") + xlab("Gov-Opp Accommodation Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

f<-cm_data%>% filter(gov_reb_accommodations>"0") %>% ggplot(aes(gov_reb_accommodations)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_reb_accommodations)+1), breaks =seq(0,max(cm_data$gov_reb_accommodations)+1,1))+
  ylab("Freq") + xlab("Gov-Reb Accommodation Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

g<-cm_data%>% filter(gov_opp_nonviol_repression>"0") %>% ggplot(aes(gov_opp_nonviol_repression)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_nonviol_repression)+1), breaks =seq(0,max(cm_data$gov_opp_nonviol_repression)+1,1))+
  ylab("Freq") + xlab("Gov-Opp Non-Violent Repression Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

h<-cm_data%>% filter(gov_reb_nonviol_repression>"0") %>% ggplot(aes(gov_reb_nonviol_repression)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_reb_nonviol_repression)+1), breaks =seq(0,max(cm_data$gov_reb_nonviol_repression)+1,1))+
  ylab("Freq") + xlab("Gov-Reb Non-Violent Repression Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))



p<-grid.arrange(a,b,c,d,e,f,g,h,ncol=2,nrow=4)
ggsave(p,filename=paste("Freq_Events_Esc",".png", sep=""))


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
###########################
#Time Series:  Burundi 2015
###########################


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

#ggsave(a, filename = "/dynamic_burundi_4.png")
#ggsave(b, filename = "/dynamic_burundi_5.png")
#ggsave(c, filename = "/dynamic_burundi_6.png")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
########################
#Time Series: Egypt 2011
########################


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


#ggsave(a, filename = "/dynamic_egypt_12.png")
#ggsave(b, filename = "/dynamic_egypt_1.png")
#ggsave(c, filename = "/dynamic_egypt_2.png")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##############################
#Time Series: Egypt 2001-2012
##############################

#Plot for each escalation covariate the Evolution of the absolute frequency event number over the months
#lines indicate whether a state-based fatality in CM happened or not

cm_egy<-cm_data %>% filter(country_name=="Egypt",year %in% (2013:2016))

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



p<-grid.arrange(a,b,c,d, e, f,g,h,ncol=2,nrow=4)
#ggsave(p,filename=paste("Egypt_Escalation_Time_Series",".png", sep=""))





#-------------------------------------------------------------------------------------------------------------------------------------#
#################################
# Exkurs: NA Imputation
#################################

#Check first if:
#the majority of missing observations are followed or lead by a missing, 0 or a number >0
cm_data_lag<- cm_data
cm_data_lag$gov_opp_accommodations<- cm_data_lag$gov_opp_accommodations %>% replace(is.na(.), 0)
cm_data_lag<- cm_data_lag %>% group_by(country_name) %>% arrange(date) %>% mutate(previous_value=lag(gov_opp_accommodations))
cm_data_lag<- cm_data_lag %>% group_by(country_name) %>% arrange(date) %>% mutate(next_value=lead(gov_opp_accommodations))
cm_data_lag<- cm_data_lag %>% mutate(is_hole = if_else(previous_value > 0 & next_value > 0 & gov_opp_accommodations==0,1,0))

#Findings:
#in the majority of cases a missing is lead and followed by missings


#Possibility in other analysis:
#Linear Moving Average (12 Months Prior and 12 Months Post)
#Remark: for some countries, consecutive years with missings (e.g Botswana 1995 and 1996)
#Idea:

#Imputation by linear interpolation
##Try:
cm_data_fil<-cm_data%>%filter(country_name=="Zimbabwe" | country_name=="Somalia")
cm_data_som<-cm_data%>%filter( country_name=="Somalia")
cm_data_som<-cm_data_som[order(cm_data_som$date),]

cm_data_som[1,30]<-400
cm_data_som[2,30]<-300
cm_data_som[3,30]<-123
cm_data_som[4,30]<-NA
cm_data_som[5,30]<-NA
cm_data_som[6,30]<-NA

cm_data_som$gov_opp_accommodations<-na_interpolation(cm_data_som$gov_opp_accommodations,option="linear", maxgap = 2)
cm_data_bots<-cm_data%>%filter( country_name=="Botswana")
cm_data_bots<-cm_data_bots[order(cm_data_bots$date),]
cm_data_bots$gov_opp_accommodations<-na_interpolation(cm_data_bots$gov_opp_accommodations,option="linear")







