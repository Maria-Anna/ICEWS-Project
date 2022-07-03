#Load packages
library(haven)
library(dplyr)
library(data.table)

rm(list=ls())

######################################
###### Load Data #####################
######################################

data_icews_cm<- fread("~/ICEWS-Project/Data/data_icews_cm.csv")

#Modify Variables
data_icews_cm$year <- as.numeric(data_icews_cm$year)

data_icews_cm<- data_icews_cm %>% mutate(Country = replace(Country, Country == "Congo", "Democratic Republic of Congo"),
                                   Country = replace(Country, Country ==  "Congo, DRC", "Democratic Republic of Congo"),
                                   Country = replace(Country, Country ==   "The Gambia"  ,"Gambia"))



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

######################################
### Variables for escalation model ###
######################################

low_level_violence<- c("145", "1451","1452","1453", "1454", "170", "180", "183", "171", "175", "186", "191","193")

non_violent_repression<-c("123", "1232","1233", "1234", "124","1241", "1321", "1322", "137", "172", "1721")

demand<-c("104","1042","1043","1044")

accommodations<-c("35", "342", "343", "344", "34", "80", "81", "811", "83", "832", "833" , "834", "812")

government<-c("Government",
              "Executive",
              "Executive Office",
              "Cabinet",
              "Foreign Ministry",
              "Interior / Home Ministry",
              "NGO Ministry",
              "Management / Budget / Planning / Organization Ministry",
              "Agriculture / Fishing / Forestry Ministry",
              "Finance / Economy / Commerce / Trade Ministry",
              "Industrial / Textiles / Mining Ministry",
              "Post / Tecoms Ministry",
              "Science / Tech Ministry",
              "State-Owned Enterprises",
              "State-Owned Agricultural",
              "State-Owned Transportation",
              "State-Owned Utilities",
              "State-Owned Heavy Industrial / Chemical",
              "State-Owned Defense / Security",
              "State-Owned Durable Goods",
              "State-Owned Consumer Goods",
              "State-Owned Consumer Services",
              "State-Owned Consulting / Financial Services",
              "State-Owned Science / Tech / Knowledge / Innovation",
              "State-Owned Medical / Health / Pharmaceutical",
              "Women / Children / Social / Welfare / Development / Religion Ministry",
              "Education Ministry",
              "Energy Ministry",
              "Environment Ministry",
              "Transportation Ministry",
              "Food Ministry",
              "Disaster Ministry",
              "Health Ministry",
              "Science / Tech / Knowledge / Innovation Ministry",
              "Human Rights Ministry",
              "Elections Ministry",
              "Housing / Construction Ministry",
              "Justice / Law Ministry",
              "Tourism Ministry",
              "Drugs Ministry",
              "Labor Ministry",
              "Water Ministry",
              "Information / Communication / Transparency Ministry",
              "State Media",
              "Defense / Security Ministry",
              "Government Major Party (In Government)",
              "Government Minor Party (In Government)",
              "Government Provincial Party (In Government)",
              "Government Municipal Party (In Government)",
              "Government Religious",
              "Military",
              "Research and Design Wings",
              "Research and Design Wings Headquarters",
              "Research and Design Wings Education / Training",
              "Research and Design Wings Support",
              "Research and Design Wings Medical",
              "Army",
              "Army Headquarters",
              "Army Special Forces",
              "Army Infantry / Regular",
              "Army Mechanized (Ships, Tanks, Planes)",
              "Army Education / Training",
              "Army Support",
              "Army Medical",
              "Navy",
              "Navy Headquarters",
              "Navy Special Forces",
              "Navy Infantry / Regular",
              "Navy Mechanized (Ships, Tanks, Planes)",
              "Navy Education / Training",
              "Navy Support",
              "Navy Medical",
              "Air Force",
              "Air Force Headquarters",
              "Air Force Special Forces",
              "Air Force Infantry / Regular",
              "Air Force Mechanized (Ships, Tanks, Planes)",
              "Air Force Education / Training",
              "Air Force Support",
              "Air Force Medical",
              "Marines",
              "Marines Headquarters",
              "Marines Special Forces",
              "Marines Infantry / Regular",
              "Marines Mechanized (Ships, Tanks, Planes)",
              "Marines Education / Training",
              "Marines Support",
              "Marines Medical",
              "Coast Guard",
              "Coast Guard Headquarters",
              "Coast Guard Special Forces",
              "Coast Guard Infantry / Regular",
              "Coast Guard Mechanized (Ships, Tanks, Planes)",
              "Coast Guard Education / Training",
              "Coast Guard Support",
              "Coast Guard Medical",
              "Judicial",
              "National / Supreme Court",
              "Provincial Court",
              "Municipal / District Court",
              "Civil Court",
              "Religious Court",
              "Military / Tribunal",
              "Local",
              "Provincial",
              "Municipal",
              "Legal",
              "Parties",
              "(National) Major Party",
              "(National) Minor Party",
              "Provincial Party",
              "Municipal Party")

rebels<-c("Radicals / Extremists / Fundamentalists","Organized Violent","Rebel","Insurgents", "Separatists")

opposition<-c("Dissident","Protestors / Popular Opposition / Mobs","Exiles","Opposition Major Party (Out of Government)","Opposition Minor Party (Out of Government)","Opposition Provincial Party (Out of Government)","Opposition Municipal Party (Out of Government)","Banned Parties")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##############################################
####### Make Data ############################
##############################################

# Data with the above Variables, new cols are binary
data_icews_cm <- data_icews_cm %>% mutate(low_level_violence = case_when(CAMEO.Code %in% low_level_violence ~ 1,           
                                                   !CAMEO.Code %in% low_level_violence ~ 0),
                     
                     non_violent_repression = case_when(CAMEO.Code %in% non_violent_repression ~ 1,           
                                                        !CAMEO.Code %in% non_violent_repression ~ 0),
                     
                     demand = case_when(CAMEO.Code %in% demand ~ 1,           
                                        !CAMEO.Code %in% demand ~ 0),
                    
                     accommodations = case_when(CAMEO.Code %in% accommodations ~ 1,           
                                                !CAMEO.Code %in% accommodations ~ 0),
                     
                     source_government = case_when(grepl(paste(government, collapse="|"), Source.Sectors)~1,
                                           !grepl(paste(government, collapse="|"), Source.Sectors)~0),
                     
                     source_rebels = case_when( grepl(paste(rebels, collapse="|"), Source.Sectors)~1,
                                         !grepl(paste(rebels, collapse="|"), Source.Sectors)~0),
                     
                     source_opposition = case_when( grepl(paste(opposition, collapse="|"), Source.Sectors)~1,
                                            !grepl(paste(opposition, collapse="|"), Source.Sectors)~0),
                     
                     target_government = case_when(grepl(paste(government, collapse="|"), Target.Sectors)~1,
                                                   !grepl(paste(government, collapse="|"), Target.Sectors)~0),
                     
                     target_rebels = case_when( grepl(paste(rebels, collapse="|"), Target.Sectors)~1,
                                                !grepl(paste(rebels, collapse="|"), Target.Sectors)~0),
                     
                     target_opposition = case_when( grepl(paste(opposition, collapse="|"), Target.Sectors)~1,
                                                !grepl(paste(opposition, collapse="|"), Target.Sectors)~0))

#allocation Version 1 R
#SOURCE:
data_icews_cm <- data_icews_cm %>%
  mutate(source_rebels = case_when(source_government != 0 & source_opposition != 0 &  source_rebels != 0 ~ 0,
                                   TRUE ~ source_rebels))
data_icews_cm <- data_icews_cm %>%
  mutate(source_government = case_when( source_government != 0 & source_opposition != 0 & source_rebels==0~ 0,
                                        TRUE ~ source_government))
data_icews_cm <- data_icews_cm %>%
  mutate(source_rebels = case_when(source_opposition != 0 & source_rebels != 0 & source_government==0 ~ 1,
                                   TRUE ~ source_rebels),
         source_opposition = case_when(source_opposition != 0 & source_rebels != 0 & source_government==0 ~ 0,
                                       TRUE ~ source_opposition))
data_icews_cm <- data_icews_cm %>%
  mutate(across(c(source_rebels,source_government,source_opposition), ~case_when(source_opposition == 0 & source_rebels != 0 & source_government!=0~ 0 , TRUE~1*(.))))


#TARGET
data_icews_cm <- data_icews_cm %>%
  mutate(target_rebels = case_when(target_government != 0 & target_opposition != 0 &  target_rebels != 0 ~ 0,
                                   TRUE ~ target_rebels))


data_icews_cm <- data_icews_cm %>%
  mutate(target_government = case_when( target_government != 0 & target_opposition != 0 & target_rebels==0~ 0,
                                        TRUE ~ target_government))

data_icews_cm <- data_icews_cm %>%
  mutate(target_rebels = case_when(target_opposition != 0 & target_rebels != 0 & target_government==0 ~ 1,
                                   TRUE ~ target_rebels),
         target_opposition = case_when(target_opposition != 0 & target_rebels != 0 & target_government==0 ~ 0,
                                       TRUE ~ target_opposition))
data_icews_cm <- data_icews_cm %>%
  mutate(across(c(target_rebels,target_government,target_opposition), ~case_when(target_opposition == 0 & target_rebels != 0 & target_government!=0  ~ 0 , TRUE~1*(.))))



# Data with actors, new variables are binary
data_icews_cm<- data_icews_cm %>% mutate( gov_opp_accommodations     =      case_when(source_government == 1 & target_opposition == 1 &  accommodations == 1 ~ 1,
                                                                    TRUE ~ 0),
                        
                        gov_opp_low_level          =      case_when(source_government == 1 & target_opposition == 1 &  low_level_violence == 1 ~ 1,
                                                                    TRUE ~ 0),
                        
                        gov_opp_nonviol_repression =      case_when(source_government == 1 & target_opposition == 1 &  non_violent_repression == 1 ~ 1,
                                                                    TRUE ~ 0),
                        
                        opp_gov_demands            =      case_when(source_opposition == 1 & target_government == 1 &  demand == 1 ~ 1,
                                                                    TRUE ~ 0),
                        
                        opp_gov_low_level          =      case_when(source_opposition == 1 & target_government == 1 &  low_level_violence == 1 ~ 1,
                                                                    TRUE ~ 0),
                        
                        gov_reb_accommodations     =      case_when(source_government == 1 & target_rebels == 1 &  accommodations == 1 ~ 1,
                                                                    TRUE ~ 0),
                        
                        gov_reb_low_level          =      case_when(source_government == 1 & target_rebels == 1 &  low_level_violence == 1 ~ 1,
                                                                    TRUE ~ 0),
                    
                        gov_reb_nonviol_repression =      case_when(source_government == 1 & target_rebels == 1 &  non_violent_repression == 1 ~ 1,
                                                                    TRUE ~ 0),
                
                        reb_gov_demands            =      case_when(source_rebels == 1 & target_government == 1 &  demand == 1 ~ 1,
                                                                    TRUE ~ 0),
                        
                        reb_gov_low_level          =      case_when(source_rebels == 1 & target_government == 1 &  low_level_violence == 1 ~ 1,
                                                                    TRUE ~ 0),
                        
                        key_cameo                  = paste(year, month, Country, sep = "_"))



#Sum variables by Year-Month
data_escalation <- data_icews_cm %>%
  group_by(key_cameo) %>%
  summarise(gov_opp_accommodations = sum(gov_opp_accommodations),
            gov_opp_low_level  = sum(gov_opp_low_level),
            gov_opp_nonviol_repression = sum(gov_opp_nonviol_repression),
            opp_gov_demands = sum(opp_gov_demands),
            opp_gov_low_level = sum(opp_gov_low_level),
            gov_reb_accommodations = sum(gov_reb_accommodations),
            gov_reb_low_level = sum(gov_reb_low_level),
            gov_reb_nonviol_repression = sum(gov_reb_nonviol_repression),
            reb_gov_demands  = sum(reb_gov_demands),
            reb_gov_low_level = sum(reb_gov_low_level))
  
#Export dataset
#write.csv(data_escalation,"~/ICEWS-Project/Data/data_escalation.csv", row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------------------------------------------
#Merge with data_cm and data_pgm
cm_data = fread("~/ICEWS-Project/Data/cm_data.csv")
load("~/ICEWS-Project/Data/pgm_data.RData")


#change country names
cm_data<- cm_data %>% mutate(country_name = replace(country_name, country_name ==   "The Gambia"  ,"Gambia"),
                             country_name = replace(country_name, country_name ==   "Congo"  ,"Democratic Republic of Congo"),
                             year_month   = format(as.Date(cm_data$date, format="%Y-%m-%d"),"%Y-%m"),
                             key_cameo= paste(year, month, country_name, sep = "_"))


pgm_data<- pgm_data %>% mutate(country_name = replace(country_name, country_name ==   "The Gambia"  ,"Gambia"),
                               country_name = replace(country_name, country_name ==   "Congo"  ,"Democratic Republic of Congo"),
                               year_month   = format(as.Date(pgm_data$date, format="%Y-%m-%d"),"%Y-%m"),
                               key_cameo= paste(year, month, country_name,  sep = "_"))




#Drop all years in Fritz et al. (2021) data set that are not included in our data

#all below 1995-01
cm_data<- cm_data %>% filter(year_month>="1995-01")
pgm_data<- pgm_data %>% filter(year_month>="1995-01")

#all above 2020-04
cm_data<- cm_data %>% filter(year_month<= "2020-04")
pgm_data<- pgm_data %>% filter(year_month<="2020-04")



#keep relevant variables of cm_data
cm_data<-cm_data %>% select(c("date",
                              "key_cameo",      
                              "year_month",
                              "month",
                              "year",
                              "month_id",
                              "key_cy",
                              "key_cm",
                              "country_name",
                              "country_id",
                              "country_iso3",
                              "name_fac",
                              "ged_dummy_sb",
                              "ged_best_sb",
                              "ged_dummy_ns",
                              "ged_best_ns",
                              "ged_dummy_os",
                              "ged_best_os",
                              "time_since_ged_dummy_os",
                              "time_since_ged_dummy_ns",
                              "time_since_ged_dummy_sb",
                              "fvp_population200",
                              "fvp_gdp200",
                              "polity",
                              "milit_exp",
                              "mcw_receiver_rolling",
                              "mcw_receiver_acute",
                              "avr_lon", "avr_lat"))

pgm_data<-pgm_data %>% select(c("key_cameo",
                      "date",
                      "year_month",
                      "month",
                      "year",
                      "pg_id" ,
                      "month_id",
                      "key_pm",
                      "key_py",
                      "key_cm",
                      "key_cy",
                      "country_name",
                      "country_id",
                      "country_iso3",
                      "name_fac",
                      "ged_dummy_sb",
                      "ged_best_sb",
                      "ged_dummy_ns",
                      "ged_best_ns",
                      "ged_dummy_os",
                      "ged_best_os",
                      "pgd_nlights_calib_mean",
                      "pgd_imr_mean",
                      "pgd_capdist",
                      "fvp_population200",
                      "fvp_gdp200",
                      "polity",
                      "milit_exp",
                      "mcw_receiver_rolling",
                      "mcw_receiver_acute",
                      "long",
                      "lat" ,
                      "time_since_ged_dummy_os.x",
                      "time_since_ged_dummy_ns.x",
                      "time_since_ged_dummy_sb.x"))


#Merge two data sets: cm_data (28 columns) and data_sum (11 columns) by key_cameo

cm_icews_data<- left_join(cm_data,data_sum, by="key_cameo")
pgm_icews_data<- left_join(pgm_data,data_sum, by="key_cameo")

#Save data
#write.csv(cm_icews_data, file="~/ICEWS-Project/Data/cm_icews_data.csv", row.names =  FALSE)
#write.csv(pgm_icews_data, file="~/ICEWS-Project/Data/pgm_icews_data.csv", row.names =  FALSE)


