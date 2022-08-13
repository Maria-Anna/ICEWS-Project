################################################################# ESTIMATE MODELS WITH MCW ####################################################################

#Remark: Fritz et al. (2021) estimation model with MCW, with data from 1995-01-01 till 2020-08-01

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

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
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
cm_data = fread("cm_data.csv")
load("pgm_data.RData")

#Filter for years 1995-2020
cm_data<- cm_data %>% filter(year>="1995")
pgm_data<- pgm_data %>% filter(year>="1995")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
############################################
#Variable Modification and Data Preparation
############################################

#Generate new data set: cm_data_included
cm_data_included = cm_data
cm_data_included = cm_data_included[order(month_id,country_id)] #order by month and country ID
cm_data_included$date = as.Date(cm_data_included$date) #set date as date format
cm_data_included$month_rescaled =  cm_data_included$month_id - min(cm_data_included$month_id) #new variable month_rescaled: month ID- min month ID (121), count starts at 0
min_month = min(pgm_data$date) #set min month for above, 1990-01-01
cm_data_included$date_new = ymd(min_month)  + months(cm_data_included$month_rescaled) # new variable date_new: 1990-01-01 plus the month rescaled (from 0 upwards)
cm_data_included$delay_sb = cm_data_included$ged_dummy_sb #new variable: delay_sb: ged_dummy_sb
cm_data_included$name_fac = factor(cm_data_included$name_fac) #variable name_fac as factor variable


#Modify data set pgm_data
pgm_data = pgm_data[order(month_id, pg_id)] #order by month and country ID
min_month = min(pgm_data$date) #set min month, 1990-01-01
pgm_data$month_rescaled =  pgm_data$month_id - min(pgm_data$month_id) #new variable month_rescaled: month ID- min month ID (121), count starts at 0
pgm_data$date_new = ymd(min_month)  + months(pgm_data$month_rescaled)  #new variable date_new: 1990-01-01 plus the month rescaled (from 0 upwards)
pgm_data$date = as.Date(pgm_data$date) #set date as date format


#Verify Country ID and name
tmp_data = cm_data_included[,.(country_name, country_id)] #data set with country and corresponding ID
tmp_data = tmp_data[, .(country_id = names(table(country_id))[which.max(table(country_id))]), by=country_name] #keep only country ID that appear the most often
cm_data_included$country_id = tmp_data$country_id[match(cm_data_included$country_name,tmp_data$country_name)] #keep in cm_data_included only the IDs that appear often
pgm_data$country_id = tmp_data$country_id[match(pgm_data$country_name,tmp_data$country_name)] #keep in pgm_data only the IDs that appear often


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#PART I: PREDICTION

#Create directory for the predictions 
dir.create(path = "Prediction_filtered")

#set time
time_beginning = Sys.time()

#set relevant dates and lags: from 2017-01 till 2019-12, with lags between 2 and 7 months
dates = seq.Date(ymd("2017-01-01"), ymd("2019-12-01"),by = "month")
s_values = 2:7

#Remark:
#t is 2017-01-01
#s is 2

###########################################################################################

#STEP O: Start computation time

for(i in 1:length(dates)){
  for(s in s_values) {
    tmp_date = dates[i]
    rem_used = mem_used()
    # Since the code is in some cases fairly memory-heavy the code prints the current use and model estimating/predicting
    writeLines(paste0("Starting with t = ", as.character(tmp_date), " and s = ",s, ";RAM use ",    
                      round(as.numeric(rem_used)/1000000000,digits = 2)," GB \n"))
    time_now = Sys.time()
    
    
    #STEP 1: Data Preparation- generate training, test and calibration data set
    
    cm_data_tmp = cm_data_included[date_new <=tmp_date] #cm_data_tmp includes only observations where date <= than tmp date (e.g all observations from 1990-01-01 till 2017-01-01, for data 2017-01-01)
    pgm_data_tmp = pgm_data[date_new <= tmp_date] #pgm_data_tmp includes only observations where date <= than tmp date (e.g all observations from 1990-01-01 till 2017-01-01, for data 2017-01-01)
    all_data = data_prep(cm_data = cm_data_tmp, pgm_data = pgm_data_tmp,S = s) #all data includes: cm_data_tmp, pgm_data_tmp and the lag of s months
    #Reminder: cm_data_tmp and pgm_data_tmp contain observations only to a respective year-month of dates (e.g till 2017-01-01)
    
    #STEP 2: Pre Estimation- train models with pre-training data up to t-s-1, 2016-10-01
    
    try_model_1 = bam(ged_dummy_sb~ s(month_id, bs="gp") +  #outcome variable: dummy whether state based conflict in country-month, corresponds to future_target, meaning: for 1990-01-01 the sb value is for 1990-03-01
                        s(log1p(time_since_ged_dummy_os), bs="ps")+
                        s(log1p(time_since_ged_dummy_ns), bs="ps")+
                        s(log1p(time_since_ged_dummy_sb), bs="ps")+
                        factor(month) + #month effect
                        ged_dummy_ns + 
                        ged_dummy_os + 
                        delay_sb + #attention: delay_sb !=ged_dummy_sb, in help function prepare_model_cm the ged_dummy_sb was replaced with future_target, delay_sb is however the state-based dummy for date
                        log1p(ged_best_sb)+
                        log(fvp_population200)+
                        log(fvp_gdp200)  +
                        polity +
                        log1p(milit_exp) + 
                        log1p(mcw_receiver_rolling)+
                        log1p(mcw_receiver_acute) +
                        te(avr_lon, avr_lat) +
                        s(name_fac, bs="re"), #country effect: use of name_fac instead of country_name due to missingness in country_name
                      data  =all_data$train_data_stage_1,family = binomial(), #data set used: date target (upshifted date by lag s) goes from 1993-01-01 till 2016-10-01 (t-s-1, 2017-01-01 -3 = 2016-10-01)
                      discrete = T, nthreads = 20,use.chol = T)
    
    #Remark: predict at country-month level prob. of state-based conflict for e.g for 2016-10-01 with data of 2016-08-01 and so on
    
    
    try_model_2 =bam(future_ged_dummy_sb~ + s(month_id, bs="gp") + #outcome variable: dummy whether state based conflict in prio grid-month, meaning: for 1990-01-01 the sb value is for 1990-03-01
                       factor(month) + 
                       s(log1p(time_since_ged_dummy_os.x), bs="ps")+
                       s(log1p(time_since_ged_dummy_ns.x), bs="ps")+
                       s(log1p(time_since_ged_dummy_sb.x), bs="ps")+
                       ged_dummy_ns +
                       ged_dummy_os +
                       ged_dummy_sb +
                       pgd_nlights_calib_mean + #new included variable
                       pgd_imr_mean + #new included variable
                       log1p(ged_best_sb) +
                       log(fvp_population200)+
                       log(fvp_gdp200)  +
                       polity +
                       log(milit_exp) +
                       pgd_capdist*log1p(mcw_receiver_rolling)+ #new included interaction
                       pgd_capdist*log1p(mcw_receiver_acute) + #new included interaction
                       te(long, lat),
                     data  = all_data$train_data_stage_2 ,family = binomial(), #data set used: date target (upshifted date by lag s) goes from 2003-12-01 till 2016-10-01 (t-s-1, 2017-01-01 -3 = 2016-10-01)
                     discrete = T,nthreads = 20,use.chol = T)
    
    
    #Remark: predict at prio grid-month level (including only prio grid with country-month sb conflicts) prob. of state-based conflict for e.g for 2016-10-01 with data of 2016-08-01 and so on
    
    
    try_model_3 = bam(future_ged_best_sb ~ s(month_id, bs="gp") + #outcome variable: number of state-based conflicts in prio grid-month, meaning: for 1990-01-01 the sb number value is for 1990-03-01
                        factor(month) + 
                        s(time_since_ged_dummy_os.x, bs="ps")+
                        s(time_since_ged_dummy_ns.x, bs="ps")+
                        s(time_since_ged_dummy_sb.x, bs="ps")+
                        pgd_imr_mean +  #attention: nightlights variable missing?
                        s(log1p(ged_best_sb), bs="ps") + 
                        log1p(ged_best_os) +
                        log1p(ged_best_ns) +
                        log(fvp_population200)+
                        log(fvp_gdp200)  +
                        polity +
                        log(milit_exp) +
                        pgd_capdist*log1p(mcw_receiver_rolling)+
                        pgd_capdist* log1p(mcw_receiver_acute) +
                        te(long, lat), data  = all_data$train_data_stage_3 ,family = ztpoisson(),#data set used: date target (upshifted date by lag s) goes from 2003-12-01 till 2016-10-01 (t-s-1, 2017-01-01 -3 = 2016-10-01)
                      discrete = T, nthreads = 20,use.chol = T)
    
    
    class(try_model_1)[1] = "gam"
    class(try_model_2)[1] = "gam"
    
    #Remark: predict at prio grid-month level (including only prio grid with prio grid_months with sb conflicts) the intensity of state-based conflict for e.g for 2016-10-01 with data of 2016-08-01 and so on
    
    
    #STEP 3: Calibration- calibrate the thresholds with data from t-s, 2016-11-01
    
    #Stage 1:
    all_data$cm_calibrate_1$pred_stage_1 = predict.gam(try_model_1,newdata =all_data$cm_calibrate_1, type = "response") #data set used: cm_calibrate_1, only data from 2016-11-01
    #predict using model 1 and cm_calibrate_1
    
    all_data$pgm_calibrate_1$pred_stage_1 = all_data$cm_calibrate_1$pred_stage_1[match(all_data$pgm_calibrate_1$key_cm, #data set used: pgm_calibrate_1, only data from 2016-11-01, observations of prio-grid included with country-month observations with no sb-conflicts 
                                                                                       all_data$cm_calibrate_1$key_cm)] #save in pgm_calibrate_1 the pred_stage_1 at country-month level
    #Stage 2:
    all_data$pgm_calibrate_1$pred_stage_2 = predict.gam(try_model_2,all_data$pgm_calibrate_1, type = "response") #predict using model 2 and pgm_calibrate_1
    
    #Stage 3:
    all_data$pgm_calibrate_1$pred_stage_3 = predict.gam(try_model_3,newdata = all_data$pgm_calibrate_1, type = "response") #predict using model 3 and pgm_calibrate_1
    
    
    set.seed(123)
    try_opt_balance = DEoptim(fn=optim_balance, lower=c(0,0), upper=c(1,1), #global optimization of optim_balance function (defined in helper function) using the pgm_calibrate_1 data set from above
                              control=list(NP=25, itermax=20,trace=F),pred_data = all_data$pgm_calibrate_1)
    
    set.seed(123)
    try_opt_mse = DEoptim(fn=optim_mse, lower=c(0,0), upper=c(1,1), #global optimization of optim_mse function (defined in helper function) using the pgm_calibrate_1 data set from above
                          control=list(NP=25, itermax=20,trace=F),pred_data = all_data$pgm_calibrate_1)
    
    trying_balance = try_opt_balance$member$bestmemit[which(try_opt_balance$member$bestvalit == min(try_opt_balance$member$bestvalit)),] #keep only lambda values of each iteration (max 20) that have the min best value of fn at each iteration
    
    trying_mse = try_opt_mse$member$bestmemit[which(try_opt_mse$member$bestvalit == min(try_opt_mse$member$bestvalit)),] #keep only lambda values of each iteration (max 20) that have the min best value of fn at each iteration
    
    if(length(trying_balance) != 2){    #For balance: if the length of the optimal threshold is higher than 2 (one for each stage) choose the min. lambda for each stage
      optimal_thresholds = trying_balance[which.min(apply(trying_balance, 1, sum)),]
    } else {
      optimal_thresholds = trying_balance
    }
    
    if(length(trying_mse) != 2){       #For MSE: if the length of the optimal threshold is higher than 2 (one for each stage) choose the min. lambda for each stage
      alt_optimal_thresholds = trying_mse[which.min(apply(trying_mse, 1, sum)),]
    } else {
      alt_optimal_thresholds = trying_mse
    }
    
    
    date_change = paste0("Prediction_filtered/final_treshold", gsub(pattern = "-",replacement = "_",x = tmp_date), "_s_",s,".csv" )
    
    writeLines(paste0("Found Tresholds are ", paste(round(alt_optimal_thresholds, digits = 3),collapse = " "), 
                      " and ",paste(round(optimal_thresholds, digits = 3),collapse = " "),"\n"))
    
    
    #STEP 4: Estimation- train models with training data up to t-s, 2016-11-01 (the same as calibration)
    
    all_data$train_data_stage_1 = rbind(all_data$train_data_stage_1, all_data$calibrate_data_stage_1) #before: date_target=2016-10-01, now: date_target=2016-11-01
    all_data$train_data_stage_2 = rbind(all_data$train_data_stage_2, all_data$calibrate_data_stage_2) #before: date_target=2016-10-01, now: date_target=2016-11-01
    all_data$train_data_stage_3 = rbind(all_data$train_data_stage_3, all_data$calibrate_data_stage_3) #before: date_target=2016-10-01, now: date_target=2016-11-01
    
    try_model_1 = bam(ged_dummy_sb~ s(month_id, bs="gp") + #same model as step 2, but with t-s
                        factor(month) + 
                        s(log1p(time_since_ged_dummy_os), bs="ps")+
                        s(log1p(time_since_ged_dummy_ns), bs="ps")+
                        s(log1p(time_since_ged_dummy_sb), bs="ps")+
                        ged_dummy_ns + 
                        ged_dummy_os + 
                        delay_sb +
                        log1p(ged_best_sb)+
                        log(fvp_population200)+
                        log(fvp_gdp200)  +
                        polity +
                        log1p(milit_exp) + 
                        log1p(mcw_receiver_rolling)+
                        log1p(mcw_receiver_acute) +
                        te(avr_lon, avr_lat) +
                        s(name_fac, bs="re"),data  =all_data$train_data_stage_1,family = binomial(),
                      discrete = T, nthreads = 20,use.chol = T)
    
    
    try_model_2 =bam(future_ged_dummy_sb~ -1 + s(month_id, bs="gp") + #same model as step 2, but with t-s
                       factor(month) + 
                       s(log1p(time_since_ged_dummy_os.x), bs="ps")+
                       s(log1p(time_since_ged_dummy_ns.x), bs="ps")+
                       s(log1p(time_since_ged_dummy_sb.x), bs="ps")+
                       ged_dummy_ns +
                       ged_dummy_os +
                       ged_dummy_sb +
                       pgd_nlights_calib_mean +
                       pgd_imr_mean + 
                       log1p(ged_best_sb) +
                       log(fvp_population200)+
                       log(fvp_gdp200)  +
                       polity +
                       log(milit_exp) +
                       pgd_capdist*log1p(mcw_receiver_rolling)+
                       pgd_capdist*log1p(mcw_receiver_acute) +
                       te(long, lat),
                     data  = all_data$train_data_stage_2 ,family = binomial(),
                     discrete = T,nthreads = 20,use.chol = T)
    
    try_model_3 = bam(future_ged_best_sb ~ s(month_id, bs="gp") + #same model as step 2, but with t-s
                        factor(month) + 
                        s(time_since_ged_dummy_os.x, bs="ps")+
                        s(time_since_ged_dummy_ns.x, bs="ps")+
                        s(time_since_ged_dummy_sb.x, bs="ps")+
                        pgd_imr_mean + 
                        s(log1p(ged_best_sb), bs="ps") + 
                        log1p(ged_best_os) +
                        log1p(ged_best_ns) +
                        log(fvp_population200)+
                        log(fvp_gdp200)  +
                        polity +
                        log(milit_exp) +
                        pgd_capdist*log1p(mcw_receiver_rolling)+
                        pgd_capdist* log1p(mcw_receiver_acute) +
                        te(long, lat), data  = all_data$train_data_stage_3 ,family = ztpoisson(),
                      discrete = T, nthreads = 20,use.chol = T)
    
    class(try_model_1)[1] = "gam"
    class(try_model_2)[1] = "gam"
    class(try_model_3)[1] = "gam"
    
    
    
    #STEP 5: Prediction- generate and save forecasts for t, 2017-01-01
    
    
    # Remark: 3 different forecasts from the same model
    # Number 1: pred_final (threshold found by minimizing loss defined in Formula (7) of the article)
    # Number 2: pred_final_alt (threshold found by minimizing the MSE of the predictions on the calibration data)
    # Number 3: pred_final_untuned (threshold set to 0.5)
    
    gc(full = T) # garbage collection 
    
    #STEP 5.1: Save predictions of each stage in cm_data_comp and pgm_data_comp (the latter data set include the new generated variables of functions but are not "cleaned", meaning in pgm data set still prio grid included with country-months without state-based conflicts) 
    
    all_data$cm_data_comp$pred_stage_1 = predict.gam(try_model_1,newdata =all_data$cm_data_comp, type = "response") #save in cm_data_comp with future date 2017-01-01: predictions of model 1
    all_data$pgm_data_comp$pred_stage_1 = all_data$cm_data_comp$pred_stage_1[match(all_data$pgm_data_comp$key_cm, #save in pgm_data_comp with future date 2017-01-01: predictions of model 1 for country-month of cm_data_comp
                                                                                   all_data$cm_data_comp$key_cm)]
    
    all_data$pgm_data_comp$pred_stage_2 = predict.gam(try_model_2,newdata = all_data$pgm_data_comp, type = "response") #save in pgm_data_comp with future date 2017-01-01: predictions of model 2
    all_data$pgm_data_comp$pred_stage_3 = predict(try_model_3,newdata = all_data$pgm_data_comp, type = "response") #save in pgm_data_comp with future date 2017-01-01: predictions of model 3
    
    #STEP 5.2: save predictions of stage 1 and 2 if higher than optimal thereholds 
    
    all_data$pgm_data_comp$higher_than_stage_1 = all_data$pgm_data_comp$pred_stage_1>optimal_thresholds[1] #for optimal balance threshold: predictions that are higher than opt. threshold of stage 1
    all_data$pgm_data_comp$higher_than_stage_2 = (all_data$pgm_data_comp$pred_stage_2>optimal_thresholds[2]) & all_data$pgm_data_comp$higher_than_stage_1 #for optimal balance threshold: predictions that are higher than opt. threshold of stage 2, cond. on higher than stage 1 opt. threshold
    
    all_data$pgm_data_comp$higher_than_stage_1_alt = all_data$pgm_data_comp$pred_stage_1>alt_optimal_thresholds[1] #for optimal MSE threshold: predictions that are higher than opt. threshold of stage 1
    all_data$pgm_data_comp$higher_than_stage_2_alt = (all_data$pgm_data_comp$pred_stage_2>alt_optimal_thresholds[2]) & #for optimal MSE threshold: predictions that are higher than opt. threshold of stage 2, cond. on higher than stage 1 opt. threshold
      all_data$pgm_data_comp$higher_than_stage_1_alt
    
    all_data$pgm_data_comp$higher_than_stage_1_untuned = all_data$pgm_data_comp$pred_stage_1>0.5     #for 0.5 threshold: predictions that are higher than opt. threshold of stage 1
    all_data$pgm_data_comp$higher_than_stage_2_untuned = (all_data$pgm_data_comp$pred_stage_2>0.5) & #for 0.5 threshold: predictions that are higher than opt. threshold of stage 2, cond. on higher than stage 1 opt. threshold
      all_data$pgm_data_comp$higher_than_stage_1_untuned
    
    #STEP 5.3: generate final predictions for t-s, 2017-01-01 
    
    all_data$pgm_data_comp$pred_final = NA #generate variable pred_final: all NA
    all_data$pgm_data_comp$pred_final[!all_data$pgm_data_comp$higher_than_stage_1] = 0 #for variable pred_final: replace with 0 if not higher that stage 1 threshold
    all_data$pgm_data_comp$pred_final[!all_data$pgm_data_comp$higher_than_stage_2] = 0 #for variable pred_final: replace with 0 if not higher that stage 2 threshold
    all_data$pgm_data_comp$pred_final[is.na(all_data$pgm_data_comp$pred_final)] =  #for variable pred_final: replace missings with pred_stage 2 for observations that have predictions above both threholds
      all_data$pgm_data_comp$pred_stage_3[is.na(all_data$pgm_data_comp$pred_final)] 
    
    all_data$pgm_data_comp$pred_final_alt = NA
    all_data$pgm_data_comp$pred_final_alt[!all_data$pgm_data_comp$higher_than_stage_1_alt] = 0
    all_data$pgm_data_comp$pred_final_alt[!all_data$pgm_data_comp$higher_than_stage_2_alt] = 0
    all_data$pgm_data_comp$pred_final_alt[is.na(all_data$pgm_data_comp$pred_final_alt)] = 
      all_data$pgm_data_comp$pred_stage_3[is.na(all_data$pgm_data_comp$pred_final_alt)] 
    
    all_data$pgm_data_comp$pred_final_untuned = NA
    all_data$pgm_data_comp$pred_final_untuned[!all_data$pgm_data_comp$higher_than_stage_1_untuned] = 0
    all_data$pgm_data_comp$pred_final_untuned[!all_data$pgm_data_comp$higher_than_stage_2_untuned] = 0
    all_data$pgm_data_comp$pred_final_untuned[is.na(all_data$pgm_data_comp$pred_final_untuned)] = 
      all_data$pgm_data_comp$pred_stage_3[is.na(all_data$pgm_data_comp$pred_final_untuned)] 
    
    
    date_change = paste0("Prediction_filtered/with_mcw_result_t_",gsub(pattern = "-",replacement = "_",x = tmp_date), "_s_",s,".csv" )
    
    #STEP 5.4: save results
    
    result = data.table(date = tmp_date, 
                        s = s, 
                        prediction = all_data$pgm_data_comp$pred_final, 
                        prediction_alt = all_data$pgm_data_comp$pred_final_alt, 
                        prediction_untuned = all_data$pgm_data_comp$pred_final_untuned, 
                        observation = all_data$pgm_data_comp$future_ged_best_sb,
                        error = abs(all_data$pgm_data_comp$pred_final - all_data$pgm_data_comp$future_ged_best_sb),
                        error_alt = abs(all_data$pgm_data_comp$pred_final_alt - all_data$pgm_data_comp$future_ged_best_sb),
                        error_untuned  = abs(all_data$pgm_data_comp$pred_final_untuned  - all_data$pgm_data_comp$future_ged_best_sb),
                        pred_1 = all_data$pgm_data_comp$pred_stage_1, 
                        pred_2 = all_data$pgm_data_comp$pred_stage_2, 
                        pred_3 = all_data$pgm_data_comp$pred_stage_3, 
                        observation_s = all_data$pgm_data_comp$ged_best_sb,
                        pg_id = all_data$pgm_data_comp$pg_id, 
                        predicted_log_change = log1p(all_data$pgm_data_comp$pred_final) - log1p(all_data$pgm_data_comp$ged_best_sb), 
                        predicted_log_change_alt = log1p(all_data$pgm_data_comp$pred_final_alt) - log1p(all_data$pgm_data_comp$ged_best_sb), 
                        predicted_log_change_untuned = log1p(all_data$pgm_data_comp$pred_final_untuned) - log1p(all_data$pgm_data_comp$ged_best_sb), 
                        observation_log_change = log1p(all_data$pgm_data_comp$future_ged_best_sb) - log1p(all_data$pgm_data_comp$ged_best_sb))
    
    fwrite(result, file = date_change)
    
    #STEP 5.5: intermediate results and TADDA scores
    
    #time measuring
    
    writeLines(paste(" 1. Finished for t =", tmp_date, "and s =", s, "\n"))
    diff = Sys.time() - time_now
    writeLines(paste(" 2. Time needed for this run: ", round(diff, digits = 3),units(diff), "\n"))
    diff = Sys.time() - time_beginning
    writeLines(paste(" 3. Time since beginning: ", round(diff, digits = 3),units(diff), "\n"))
    
    #TADDA scores (needed to compare to MSE)
    
    tada =     mean(abs(result$predicted_log_change-result$observation_log_change) + #TADDA score for tuned threshold (optimal balance threshold)
                      abs(result$predicted_log_change)*
                      (sign(result$predicted_log_change) != sign(result$observation_log_change))*
                      (abs(result$predicted_log_change-result$observation_log_change)>1))
    writeLines(paste(" 4. TADA (tuned TH)= " , round(tada,digits = 3), "\n"))
    
    tada =     mean(abs(result$predicted_log_change_untuned-result$observation_log_change) + #TADDA score for untuned threshold (0.5 threshold)
                      abs(result$predicted_log_change_untuned)*
                      (sign(result$predicted_log_change_untuned) != sign(result$observation_log_change))*
                      (abs(result$predicted_log_change_untuned-result$observation_log_change)>1))
    writeLines(paste(" 5. TADA (not tuned TH)= " , round(tada,digits = 3), "\n"))
    
    tada =     mean(abs(result$predicted_log_change_alt-result$observation_log_change) + #TADDA score for alternative threshold (optimal MSE threshold)
                      abs(result$predicted_log_change_alt)*
                      (sign(result$predicted_log_change_alt) != sign(result$observation_log_change))*
                      (abs(result$predicted_log_change_alt-result$observation_log_change)>1))
    writeLines(paste(" 6. TADA (alt tuned TH)= " , round(tada,digits = 3), "\n"))
    
    #MSE scores (needed to compare to TADDA)
    
    writeLines(paste(" 7. MSE (tuned TH)= " , round(mean((result$predicted_log_change-    #MSE score for tuned threshold (optimal balance threshold)
                                                            result$observation_log_change)^2),digits = 3), "\n"))
    
    writeLines(paste(" 8. MSE (not tuned TH)= " , round(mean((result$predicted_log_change_untuned- #MSE score for untuned threshold (0.5 threshold)
                                                                result$observation_log_change)^2),digits = 3), "\n"))
    
    writeLines(paste(" 9. MSE (only 0) = " , round(mean(log1p(result$observation)^2),digits = 3), "\n")) #MSE score for observed state-based conflicts
    
    writeLines(paste(" 10. MSE (alt-tuned TH)= " , round(mean((result$predicted_log_change_alt- #MSE score for tuned threshold (optimal MSE threshold)
                                                                 result$observation_log_change)^2),digits = 3), "\n"))
    
    
    rm(try_model_1, try_model_2, try_model_3,result,all_data,pgm_data_tmp,cm_data_tmp)
    gc(full = T)
    
  }
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#PART I: REAL FORECASTING

#set time
time_beginning = Sys.time()

#set relevant lags (dates do not need to be set anymore): lags between 2 and 7 months
s_values = 2:7

###############################################################################################


#STEP 0: Start computing time

for(s in s_values) {
  tmp_date = max(cm_data_included$date) + months(s)
  rem_used = mem_used()
  writeLines(paste0("Starting with t = ", as.character(tmp_date), " and s = ",s, ";RAM use ",    
                    round(as.numeric(rem_used)/1000000000,digits = 2)," GB \n"))
  time_now = Sys.time()
  
  
  #STEP 1: Prepare data set
  
  all_data = data_prep_forecast(cm_data = cm_data_included, pgm_data = pgm_data,S = s) #included data sets are: cm_data_included (cm_data with new variables) and pgm_data (normal pgm_data)
  
  #STEP 2: Pre-Estimation
  
  try_model_1 = bam(ged_dummy_sb~ s(month_id, bs="gp") + 
                      s(log1p(time_since_ged_dummy_os), bs="ps")+
                      s(log1p(time_since_ged_dummy_ns), bs="ps")+
                      s(log1p(time_since_ged_dummy_sb), bs="ps")+
                      factor(month) + 
                      ged_dummy_ns + 
                      ged_dummy_os + 
                      delay_sb +
                      log1p(ged_best_sb)+
                      log(fvp_population200)+
                      log(fvp_gdp200)  +
                      polity +
                      log1p(milit_exp) + 
                      log1p(mcw_receiver_rolling)+
                      log1p(mcw_receiver_acute) +
                      te(avr_lon, avr_lat) +
                      s(name_fac, bs="re"),data  =all_data$train_data_stage_1, #the data set used includes observations till date target 2020-07-01 (date 2020-05-01)
                    family = binomial(),
                    discrete = T, nthreads = 20,use.chol = T)
  
  
  try_model_2 =bam(future_ged_dummy_sb~ s(month_id, bs="gp") +
                     factor(month) + 
                     s(log1p(time_since_ged_dummy_os.x), bs="ps")+
                     s(log1p(time_since_ged_dummy_ns.x), bs="ps")+
                     s(log1p(time_since_ged_dummy_sb.x), bs="ps")+
                     ged_dummy_ns +
                     ged_dummy_os +
                     ged_dummy_sb +
                     pgd_nlights_calib_mean +
                     pgd_imr_mean + 
                     log1p(ged_best_sb) +
                     log(fvp_population200)+
                     log(fvp_gdp200)  +
                     polity +
                     log(milit_exp) +
                     pgd_capdist*log1p(mcw_receiver_rolling)+
                     pgd_capdist*log1p(mcw_receiver_acute) +
                     te(long, lat),
                   data  = all_data$train_data_stage_2 ,family = binomial(), #the data set used includes observations till date target 2020-07-01 (date 2020-05-01)
                   discrete = T,nthreads = 20,use.chol = T)
  
  
  try_model_3 = bam(future_ged_best_sb ~ s(month_id, bs="gp") + 
                      factor(month) + 
                      s(time_since_ged_dummy_os.x, bs="ps")+
                      s(time_since_ged_dummy_ns.x, bs="ps")+
                      s(time_since_ged_dummy_sb.x, bs="ps")+
                      pgd_imr_mean + 
                      s(log1p(ged_best_sb), bs="ps") + 
                      log1p(ged_best_os) +
                      log1p(ged_best_ns) +
                      log(fvp_population200)+
                      log(fvp_gdp200)  +
                      polity +
                      log(milit_exp) +
                      pgd_capdist*log1p(mcw_receiver_rolling)+
                      pgd_capdist* log1p(mcw_receiver_acute) +
                      te(long, lat), data  = all_data$train_data_stage_3 ,family = ztpoisson(), #the data set used includes observations till date target 2020-07-01 (date 2020-05-01)
                    discrete = T, nthreads = 20,use.chol = T)
  
  class(try_model_1)[1] = "gam"
  class(try_model_2)[1] = "gam"
  class(try_model_3)[1] = "gam"
  
  
  #STEP 3: Calibration- find and save predictions for each stage and calibrate the optimal threshold
  
  #Stage 1:
  all_data$cm_calibrate_1$pred_stage_1 = predict.gam(try_model_1,newdata =all_data$cm_calibrate_1, type = "response")
  all_data$pgm_calibrate_1$pred_stage_1 = all_data$cm_calibrate_1$pred_stage_1[match(all_data$pgm_calibrate_1$key_cm,
                                                                                     all_data$cm_calibrate_1$key_cm)]
  #Stage 2:
  all_data$pgm_calibrate_1$pred_stage_2 = predict.gam(try_model_2,all_data$pgm_calibrate_1, type = "response")
  
  #Stage 3:
  all_data$pgm_calibrate_1$pred_stage_3 = predict.gam(try_model_3,newdata = all_data$pgm_calibrate_1, type = "response")
  
  
  set.seed(123)
  try_opt_balance = DEoptim(fn=optim_balance, lower=c(0,0), upper=c(1,1), #optimization of optim_balance function
                            control=list(NP=25, itermax=20,trace=F),pred_data = all_data$pgm_calibrate_1)
  
  set.seed(123)
  try_opt_mse = DEoptim(fn=optim_mse, lower=c(0,0), upper=c(1,1), #optimization of optim_mse function
                        control=list(NP=25, itermax=20,trace=F),pred_data = all_data$pgm_calibrate_1)
  
  trying_balance = try_opt_balance$member$bestmemit[which(try_opt_balance$member$bestvalit == min(try_opt_balance$member$bestvalit)),] #find optimal threshold for each stage and iteration
  
  trying_mse = try_opt_mse$member$bestmemit[which(try_opt_mse$member$bestvalit == min(try_opt_mse$member$bestvalit)),] #find optimal threshold for each stage and iteration
  
  if(length(trying_balance) != 2){ #find optimal threshold of balance function with l1 norm
    optimal_thresholds = trying_balance[which.min(apply(trying_balance, 1, sum)),]
  } else {
    optimal_thresholds = trying_balance
  }
  
  if(length(trying_mse) != 2){ #find optimal threshold of mse function with l1 norm
    alt_optimal_thresholds = trying_mse[which.min(apply(trying_mse, 1, sum)),]
  } else {
    alt_optimal_thresholds = trying_mse
  }
  
  writeLines(paste0("Found Tresholds are ", paste(round(alt_optimal_thresholds, digits = 3),collapse = " "), 
                    " and ",paste(round(optimal_thresholds, digits = 3),collapse = " "),"\n"))
  
  
  #STEP 4: Estimation with date target 2020-08-01 (estimation including calibration date)
  
  all_data$train_data_stage_1 = rbind(all_data$train_data_stage_1, all_data$calibrate_data_stage_1) #include in training data set the calibration data set
  all_data$train_data_stage_2 = rbind(all_data$train_data_stage_2, all_data$calibrate_data_stage_2) #include in training data set the calibration data set
  all_data$train_data_stage_3 = rbind(all_data$train_data_stage_3, all_data$calibrate_data_stage_3) #include in training data set the calibration data set
  
  try_model_1 = bam(ged_dummy_sb~ s(month_id, bs="gp") + 
                      factor(month) + 
                      s(log1p(time_since_ged_dummy_os), bs="ps")+
                      s(log1p(time_since_ged_dummy_ns), bs="ps")+
                      s(log1p(time_since_ged_dummy_sb), bs="ps")+
                      ged_dummy_ns + 
                      ged_dummy_os + 
                      delay_sb +
                      log1p(ged_best_sb)+
                      log(fvp_population200)+
                      log(fvp_gdp200)  +
                      polity +
                      log1p(milit_exp) + 
                      log1p(mcw_receiver_rolling)+
                      log1p(mcw_receiver_acute) +
                      te(avr_lon, avr_lat) +
                      s(name_fac, bs="re"),data  =all_data$train_data_stage_1,family = binomial(),
                    discrete = T, nthreads = 20,use.chol = T)
  
  try_model_2 =bam(future_ged_dummy_sb~  s(month_id, bs="gp") +
                     factor(month) + 
                     s(log1p(time_since_ged_dummy_os.x), bs="ps")+
                     s(log1p(time_since_ged_dummy_ns.x), bs="ps")+
                     s(log1p(time_since_ged_dummy_sb.x), bs="ps")+
                     ged_dummy_ns +
                     ged_dummy_os +
                     ged_dummy_sb +
                     pgd_nlights_calib_mean +
                     pgd_imr_mean + 
                     log1p(ged_best_sb) +
                     log(fvp_population200)+
                     log(fvp_gdp200)  +
                     polity +
                     log(milit_exp) +
                     pgd_capdist*log1p(mcw_receiver_rolling)+
                     pgd_capdist*log1p(mcw_receiver_acute) +
                     te(long, lat),
                   data  = all_data$train_data_stage_2 ,family = binomial(),
                   discrete = T,nthreads = 20,use.chol = T)
  
  
  try_model_3 = bam(future_ged_best_sb ~ s(month_id, bs="gp") + 
                      factor(month) + 
                      s(time_since_ged_dummy_os.x, bs="ps")+
                      s(time_since_ged_dummy_ns.x, bs="ps")+
                      s(time_since_ged_dummy_sb.x, bs="ps")+
                      pgd_imr_mean + 
                      s(log1p(ged_best_sb), bs="ps") + 
                      log1p(ged_best_os) +
                      log1p(ged_best_ns) +
                      log(fvp_population200)+
                      log(fvp_gdp200)  +
                      polity +
                      log(milit_exp) +
                      pgd_capdist*log1p(mcw_receiver_rolling)+
                      pgd_capdist* log1p(mcw_receiver_acute) +
                      te(long, lat), data  = all_data$train_data_stage_3 ,family = ztpoisson(),
                    discrete = T, nthreads = 20,use.chol = T)
  
  
  
  
  class(try_model_1)[1] = "gam"
  class(try_model_2)[1] = "gam"
  class(try_model_3)[1] = "gam"
  
  
  
  #STEP 5: Prediction- with and without tuned thresholds and for each stage
  
  gc(full = T)
  
  #Stage 1:
  all_data$cm_data_comp$pred_stage_1 = predict.gam(try_model_1,newdata =all_data$cm_data_comp, type = "response")
  all_data$pgm_data_comp$pred_stage_1 = all_data$cm_data_comp$pred_stage_1[match(all_data$pgm_data_comp$key_cm,
                                                                                 all_data$cm_data_comp$key_cm)]
  
  #Stage 2:
  all_data$pgm_data_comp$pred_stage_2 = predict.gam(try_model_2,newdata = all_data$pgm_data_comp, type = "response")
  
  #Stage 3:
  all_data$pgm_data_comp$pred_stage_3 = predict.gam(try_model_3,newdata = all_data$pgm_data_comp, type = "response")
  
  #For optimal balance, MSE and 0.5 threshold save predictions for stage 1 and 2:
  all_data$pgm_data_comp$higher_than_stage_1 = all_data$pgm_data_comp$pred_stage_1>optimal_thresholds[1]
  all_data$pgm_data_comp$higher_than_stage_2 = (all_data$pgm_data_comp$pred_stage_2>optimal_thresholds[2]) & all_data$pgm_data_comp$higher_than_stage_1
  
  all_data$pgm_data_comp$higher_than_stage_1_alt = all_data$pgm_data_comp$pred_stage_1>alt_optimal_thresholds[1]
  all_data$pgm_data_comp$higher_than_stage_2_alt = (all_data$pgm_data_comp$pred_stage_2>alt_optimal_thresholds[2]) &
    all_data$pgm_data_comp$higher_than_stage_1_alt
  
  all_data$pgm_data_comp$higher_than_stage_1_untuned = all_data$pgm_data_comp$pred_stage_1>0.5
  all_data$pgm_data_comp$higher_than_stage_2_untuned = (all_data$pgm_data_comp$pred_stage_2>0.5) &
    all_data$pgm_data_comp$higher_than_stage_1_untuned
  
  #For optimal balance, MSE and 0.5 threshold save predictions for stage 3:
  all_data$pgm_data_comp$pred_final = NA
  all_data$pgm_data_comp$pred_final[!all_data$pgm_data_comp$higher_than_stage_1] = 0
  all_data$pgm_data_comp$pred_final[!all_data$pgm_data_comp$higher_than_stage_2] = 0
  all_data$pgm_data_comp$pred_final[is.na(all_data$pgm_data_comp$pred_final)] = 
    all_data$pgm_data_comp$pred_stage_3[is.na(all_data$pgm_data_comp$pred_final)] 
  
  all_data$pgm_data_comp$pred_final_alt = NA
  all_data$pgm_data_comp$pred_final_alt[!all_data$pgm_data_comp$higher_than_stage_1_alt] = 0
  all_data$pgm_data_comp$pred_final_alt[!all_data$pgm_data_comp$higher_than_stage_2_alt] = 0
  all_data$pgm_data_comp$pred_final_alt[is.na(all_data$pgm_data_comp$pred_final_alt)] = 
    all_data$pgm_data_comp$pred_stage_3[is.na(all_data$pgm_data_comp$pred_final_alt)] 
  
  all_data$pgm_data_comp$pred_final_untuned = NA
  all_data$pgm_data_comp$pred_final_untuned[!all_data$pgm_data_comp$higher_than_stage_1_untuned] = 0
  all_data$pgm_data_comp$pred_final_untuned[!all_data$pgm_data_comp$higher_than_stage_2_untuned] = 0
  all_data$pgm_data_comp$pred_final_untuned[is.na(all_data$pgm_data_comp$pred_final_untuned)] = 
    all_data$pgm_data_comp$pred_stage_3[is.na(all_data$pgm_data_comp$pred_final_untuned)] 
  
  
  date_change = paste0("Prediction_filtered/real_mcw_forecast_t_",gsub(pattern = "-",replacement = "_",x = tmp_date), "_s_",s,".csv" )
  
  #save predictions
  result = data.table(date = tmp_date, 
                      s = s, 
                      prediction = all_data$pgm_data_comp$pred_final, 
                      prediction_alt = all_data$pgm_data_comp$pred_final_alt, 
                      prediction_untuned = all_data$pgm_data_comp$pred_final_untuned, 
                      pred_1 = all_data$pgm_data_comp$pred_stage_1, 
                      pred_2 = all_data$pgm_data_comp$pred_stage_2, 
                      pred_3 = all_data$pgm_data_comp$pred_stage_3, 
                      observation_s = all_data$pgm_data_comp$ged_best_sb,
                      pg_id = all_data$pgm_data_comp$pg_id, 
                      predicted_log_change = log1p(all_data$pgm_data_comp$pred_final) - log1p(all_data$pgm_data_comp$ged_best_sb), 
                      predicted_log_change_alt = log1p(all_data$pgm_data_comp$pred_final_alt) - log1p(all_data$pgm_data_comp$ged_best_sb), 
                      predicted_log_change_untuned = log1p(all_data$pgm_data_comp$pred_final_untuned) - log1p(all_data$pgm_data_comp$ged_best_sb))
  
  fwrite(result, file = date_change)
  
  writeLines(paste(" 1. Finished for t =", tmp_date, "and s =", s, "\n"))
  diff = Sys.time() - time_now
  writeLines(paste(" 2. Time needed for this run: ", round(diff, digits = 3),units(diff), "\n"))
  diff = Sys.time() - time_beginning
  writeLines(paste(" 3. Time since beginning: ", round(diff, digits = 3),units(diff), "\n"))
  
  
  gc(full = T)
  # save the models for s = 2 
  if(s == 2){
    save(try_model_1,file =  "Prediction_filtered/models/try_model_1.RData")
    save(try_model_2,file =  "Prediction_filtered/models/try_model_2.RData")
    save(try_model_3,file =  "Prediction_filtered/models/try_model_3.RData")
    data_pg = all_data$pgm_data_comp
    save(data_pg, file = "Prediction_filtered/models/data_pg.RData")
    data_c = all_data$cm_data_comp
    save(data_c, file = "Prediction_filtered/models/data_c.RData")
  }
  
  rm(try_model_1, try_model_2, try_model_3,result,all_data)
  
}

