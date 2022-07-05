#Library
library(dplyr)
library(data.table)

rm(list=ls())

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Compare data set 2001 to 2015 with Blair and Sambanis (2020)
cm_data_BlairSambanis<- fread("~/ICEWS-Project/Data/cm_data_BlairSambanis.csv")
data_icews_cm<-fread("~/ICEWS-Project/Data/data_icews_cm.csv") 
data_escalation<- fread("~/ICEWS-Project/Data/data_escalation.csv")


#load Blair and Sambanis (2020) and keep relevant variables

cm_data_BlairSambanis<-cm_data_BlairSambanis %>% select(year, month,country_name, gov_opp_accommodations, gov_opp_low_level, gov_opp_nonviol_repression, opp_gov_demands, opp_gov_low_level,gov_reb_accommodations, gov_reb_low_level, gov_reb_nonviol_repression, reb_gov_demands, reb_gov_low_level ) %>%
  filter(country_name %in%  levels(factor(data_icews_cm$Country))) %>%
  mutate(key_cameo= paste(year, month, country_name, sep = "_"))

#filter data set: keep 2001 to 2015
data_escalation_filtered <- data_escalation %>% semi_join(cm_data_BlairSambanis, by="key_cameo")

#compare both data sets
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

#data set with all observations that are in ICEWS but not in Blair and Sambanis (2020)
data_escalation_not_in_BS<- data_escalation_filtered %>% anti_join(cm_data_BlairSambanis, by=escalation_variables)

#data set with all observations that are in ICEWS and in Blair and Sambanis (2020)
data_escalation_in_BS<- data_escalation_filtered %>% semi_join(cm_data_BlairSambanis, by=escalation_variables)

