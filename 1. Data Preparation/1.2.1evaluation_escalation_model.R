#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Compare data set 2001 to 2015 with Blair and Sambanis (2020)


#load Blair and Sambanis (2020) and keep relevant variables

X1mo_data_check<-X1mo_data %>% select(year, month,country_name, gov_opp_accommodations, gov_opp_low_level, gov_opp_nonviol_repression, opp_gov_demands, opp_gov_low_level,gov_reb_accommodations, gov_reb_low_level, gov_reb_nonviol_repression, reb_gov_demands, reb_gov_low_level ) %>%
  filter(country_name %in%  levels(factor(data$Country))) %>%
  mutate(key_cameo= paste(year, month, country_name, sep = "_"))

#filter data set: keep 2001 to 2015
data_sum_filtered <- data_sum %>% semi_join(X1mo_data_check, by="key_cameo")

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
data_sum_not_in_BS<- data_sum_filtered %>% anti_join(X1mo_data_check, by=escalation_variables)

#data set with all observations that are in ICEWS and in Blair and Sambanis (2020)
data_sum_in_BS<- data_sum_filtered %>% semi_join(X1mo_data_check, by=escalation_variables)

