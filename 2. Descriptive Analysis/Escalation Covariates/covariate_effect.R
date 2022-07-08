############################################### The Covariates Effect ############################################

#Remark: find relationship between the covariates and the outcome variable

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

#-------------------------------------------------------------------------------------------------------------------------------------#
##################
#Data Preparation
##################

#Set working directory

#for Clara
setwd("~/Desktop/Consulting Bewaffnete Konflikte/Datasets_Africa/Data sets")

#Run helper functions script
rm(list=ls())
source('helper_functions.R')

#Load data sets
cm_data_cor = fread("cm_data.csv")
cm_data = fread("cm_icews_data.csv")
pgm_data = fread("pgm_icews_data.csv")

#Replace NA with 0 (missing events on country-month level are interpreted as 0 events)
cm_data[is.na(cm_data),]<-0
pgm_data[is.na(pgm_data),]<-0


#-------------------------------------------------------------------------------------------------------------------------------------#
#################################
#Nas Descriptives
#################################

#Control for missing

#see unique country-months with missings
country_names<-as.data.frame(unique(cm_data$country_name))
#generate data frame with missings
colSums(is.na(cm_data))
cm_data_icews_na <- cm_data[rowSums(is.na(cm_data)) > 0,]
#save
write.csv(cm_data_icews_na, file="cm_data_icews_na.csv", row.names = F)
#do all variables have the same number of missings?
colSums(is.na(cm_data_icews_na))
#how many missings does each country have?
missings_cm<-as.data.frame(table(sort((cm_data_icews_na$country_name))))
#generate table with country and missing months (for latex)
missings_cm<-missings_cm[order(missings_cm$Freq),]
print(xtable(missings_cm, type = "latex"), file = "missings_cm.tex")


#-------------------------------------------------------------------------------------------------------------------------------------#
#################################
#Nas Imputation
#################################

#Linear Moving Average (12 Months Prior and 12 Months Post)?
#Remark: for some countries, consecutive years with missings (e.g Botswana 1995 and 1996)
#Idea:

#Imputation by linear interpolation
##try:
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


#-------------------------------------------------------------------------------------------------------------------------------------#
#################################
#Covariates and Outcome Variable
#################################

#generate summary statistics table
stargazer(cm_data, summary.stat = c("mean", "median", "min", "max", "sd", "p25", "p75"))

#generate skewness table (compare linear vs. log transformation)
cm_data_sub<-cm_data[,30:39]
a<-as.data.frame(skewness(cm_data_sub))
b<-as.data.frame(skewness(log1p(cm_data_sub)))
a<-cbind(a,b)
print(xtable(a, type = "latex"), file = "skewness_comp.tex")


#generate distribution plots (necessary to see distribution)
a<-cm_data%>% filter(reb_gov_low_level>"0") %>% ggplot(aes(reb_gov_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$reb_gov_low_level)+1), breaks =seq(0,max(cm_data$reb_gov_low_level)+1,10))+
  ylab("Absolute Frequency") + xlab("Reb-Gov Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

b<-cm_data%>% filter(opp_gov_low_level>"0") %>% ggplot(aes(opp_gov_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$opp_gov_low_level)+1), breaks =seq(0,max(cm_data$opp_gov_low_level)+1,10))+
  ylab("Absolute Frequency") + xlab("Opp-Gov Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

c<-cm_data%>% filter(gov_reb_low_level>"0") %>% ggplot(aes(gov_reb_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, 10), breaks =seq(0,10))+
  ylab("Absolute Frequency") + xlab("Opp-Reb Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

d<-cm_data%>% filter(gov_opp_low_level>"0") %>% ggplot(aes(gov_opp_low_level)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_low_level)+1), breaks =seq(0,max(cm_data$gov_opp_low_level)+1,10))+
  ylab("Absolute Frequency") + xlab("Gov-Opp Low Level Violence Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave("low_level_freq_reb_gov.png",a)
ggsave("low_level_freq_opp_gov.png",b)
ggsave("low_level_freq_gov_reb.png",c)
ggsave("low_level_freq_gov_opp.png",d)

a<-cm_data%>% filter(gov_opp_accommodations>"0") %>% ggplot(aes(gov_opp_accommodations)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_accommodations)+1), breaks =seq(0,max(cm_data$gov_opp_accommodations)+1,1))+
  ylab("Absolute Frequency") + xlab("Gov-Opp Accommodation Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

b<-cm_data%>% filter(gov_reb_accommodations>"0") %>% ggplot(aes(gov_reb_accommodations)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_reb_accommodations)+1), breaks =seq(0,max(cm_data$gov_reb_accommodations)+1,1))+
  ylab("Absolute Frequency") + xlab("Gov-Reb Accommodation Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

c<-cm_data%>% filter(gov_opp_nonviol_repression>"0") %>% ggplot(aes(gov_opp_nonviol_repression)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_opp_nonviol_repression)+1), breaks =seq(0,max(cm_data$gov_opp_nonviol_repression)+1,1))+
  ylab("Absolute Frequency") + xlab("Gov-Opp Non-Violent Repression Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

d<-cm_data%>% filter(gov_reb_nonviol_repression>"0") %>% ggplot(aes(gov_reb_nonviol_repression)) + geom_histogram(binwidth = 0.5, color="black", fill="white")+
  scale_x_continuous(limits = c(0, max(cm_data$gov_reb_nonviol_repression)+1), breaks =seq(0,max(cm_data$gov_reb_nonviol_repression)+1,1))+
  ylab("Absolute Frequency") + xlab("Gov-Reb Non-Violent Repression Events") + 
  theme(plot.title = element_text(color = "black", size=14, hjust=0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(hjust=0.5, size=16),
        axis.title.y= element_text(hjust=0.5,size=16),
        axis.text = element_text(size=12,colour = "black"))

ggsave("accom_freq_gov_opp.png",a)
ggsave("accom_freq_gov_reb.png",b)
ggsave("repr_freq_gov_opp.png",c)
ggsave("repr_freq_gov_reb.png",d)



#plot possibilities: linear, log, log1p
ggplot(cm_data, aes(log1p(milit_exp))) + geom_histogram()
ggplot(cm_data, aes(ged_best_sb)) + geom_histogram()
ggplot(cm_data, aes(log1p(ged_best_sb))) + geom_histogram()
ggplot(cm_data, aes(log1p(reb_gov_low_level))) + geom_histogram()
ggplot(cm_data, aes(reb_gov_low_level)) + geom_histogram()


ggplot(cm_data, aes(1:nrow(cm_data),log1p(reb_gov_low_level))) + geom_point(size=0.9)
ggplot(cm_data, aes(1:nrow(cm_data),reb_gov_low_level)) + geom_point(size=0.9)

ggplot(cm_data, aes(1:nrow(cm_data),log1p(milit_exp))) + geom_point(size=0.9)
ggplot(cm_data, aes(1:nrow(cm_data),log1p(ged_best_sb))) + geom_point(size=0.9)
ggplot(cm_data, aes(1:nrow(cm_data),log(fvp_population200))) + geom_point(size=0.9)



ggplot(cm_data, aes(1:nrow(cm_data),milit_exp)) + geom_point(size=0.9)
ggplot(cm_data, aes(1:nrow(cm_data),mcw_receiver_rolling)) + geom_point(size=0.9)
ggplot(cm_data, aes(1:nrow(cm_data),mcw_receiver_acute)) + geom_point(size=0.9)
ggplot(cm_data, aes(1:nrow(cm_data),ged_best_sb)) + geom_point(size=0.9)
ggplot(cm_data, aes(1:nrow(cm_data),fvp_population200)) + geom_point(size=0.9)


#Generate lag outcome variables to test relationship:

#ged_dummy_sb
cm_data[, future_target:=c(ged_dummy_sb[-(1:2)],rep(NA, 2)), by=country_id] 
cm_data$ged_dummy_sb = cm_data$future_target

#future_ged_dummy_sb


#future_ged_best_sb


#Stage 1:
#outcome: ged_dummy_sb
model_1<- gam(ged_dummy_sb~ log1p(reb_gov_demands) +
                log1p(opp_gov_demands) +
                log1p(gov_opp_accommodations) +
                log1p(gov_reb_accommodations) +
                log1p(gov_opp_nonviol_repression) +
                log1p(gov_reb_nonviol_repression) +
                log1p(reb_gov_low_level) +
                log1p(opp_gov_low_level) + 
                log1p(gov_reb_low_level) +
                log1p(gov_opp_low_level), cm_data, family = binomial(),
              discrete = F, nthreads = 20,use.chol = T)
summary(model_1)
model_1<- gam(ged_dummy_sb~ log10(reb_gov_demands) +
                log10(opp_gov_demands) +
                log10(gov_opp_accommodations) +
                log10(gov_reb_accommodations) +
                log10(gov_opp_nonviol_repression) +
                log10(gov_reb_nonviol_repression) +
                log10(reb_gov_low_level) +
                log10(opp_gov_low_level) + 
                log10(gov_reb_low_level) +
                log10(gov_opp_low_level), cm_data, family = binomial(),
              discrete = F, nthreads = 20,use.chol = T)
summary(model_1)
model_1<- gam(ged_dummy_sb~ reb_gov_demands +
                opp_gov_demands +
                gov_opp_accommodations +
                gov_reb_accommodations +
                gov_opp_nonviol_repression +
                gov_reb_nonviol_repression +
                reb_gov_low_level +
                opp_gov_low_level + 
                gov_reb_low_level +
                gov_opp_low_level, cm_data, family = binomial(), discrete = F, nthreads = 20,use.chol = T)
summary(model_1)





#Stage 2:
#outcome: future_ged_dummy_sb
#model_2<-


#Stage 3:
#outcome: future_ged_best_sb
#model_3<-
  
#-------------------------------------------------------------------------------------------------------------------------------------#
#################################
# Fill NAs with Time Series
#################################
cm_data_lag<- cm_data
cm_data_lag$gov_opp_accommodations<- cm_data_lag$gov_opp_accommodations %>% replace(is.na(.), 0)
cm_data_lag<- cm_data_lag %>% group_by(country_name) %>% arrange(date) %>% mutate(previous_value=lag(gov_opp_accommodations))
cm_data_lag<- cm_data_lag %>% group_by(country_name) %>% arrange(date) %>% mutate(next_value=lead(gov_opp_accommodations))
cm_data_lag<- cm_data_lag %>% mutate(is_hole = if_else(previous_value > 0 & next_value > 0 & gov_opp_accommodations==0,1,0))






















