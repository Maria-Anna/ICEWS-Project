# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ------                          Cornelius Fritz                       ------ #
# ------              Estimation Model Without MCW Covariates           ------ #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# As the code of the model is (mostly) equivalent to the script '1_estimate_models.R' comments are omitted  
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
rm(list=ls())

source('helper_functions.R')

library(mgcv)
library(MASS)
library(grid)
library(data.table)
library(countreg)
library(lubridate)
library(pryr)
library(DEoptim)
# 1. Load Data ----

cm_data = fread("Data/cm_data.csv")
load("Data/pgm_data.RData")

# pgm_data = fread("Data/pgm_data.csv")

cm_data_included = cm_data
cm_data_included = cm_data_included[order(month_id,country_id)]
pgm_data = pgm_data[order(month_id, pg_id)]

# Include date variable (new version)
cm_data_included$date = as.Date(cm_data_included$date)
min_month = min(pgm_data$date)
cm_data_included$month_rescaled =  cm_data_included$month_id - min(cm_data_included$month_id)
cm_data_included$date_new = ymd(min_month)  + months(cm_data_included$month_rescaled)
cm_data_included$delay_sb = cm_data_included$ged_dummy_sb 
cm_data_included$name_fac = factor(cm_data_included$name_fac)
min_month = min(pgm_data$date)
pgm_data$month_rescaled =  pgm_data$month_id - min(pgm_data$month_id)
pgm_data$date_new = ymd(min_month)  + months(pgm_data$month_rescaled)
pgm_data$date = as.Date(pgm_data$date)
# Check the country_ids and country_names 
tmp_data = cm_data_included[,.(country_name, country_id)]
# check for each country_id/country_name tuple which id is used most often and use this
tmp_data = tmp_data[, .(country_id = names(table(country_id))[which.max(table(country_id))]), by=country_name]
cm_data_included$country_id = tmp_data$country_id[match(cm_data_included$country_name,tmp_data$country_name)]
pgm_data$country_id = tmp_data$country_id[match(pgm_data$country_name,tmp_data$country_name)]


time_beginning = Sys.time()

# 2. Evaluation Forecast ----
# Between January 2014 and December 2019
# Given that one model takes about 2.6 min the complete prediction should take (2019 - 2014 +1)*12*4*(1.2 to 1.6)/60 = (5.76 to 7.68)  Hours 
dates = seq.Date(ymd("2017-01-01"), ymd("2019-12-01"),by = "month")
s_values = 2:7

for(i in 1:length(dates)){
  for(s in s_values) {
    tmp_date = dates[i]
    rem_used = mem_used()
    
    writeLines(paste0("Starting with t = ", as.character(tmp_date), " and s = ",s, ";RAM use ",    
                      round(as.numeric(rem_used)/1000000000,digits = 2)," GB \n"))
    time_now = Sys.time()

    # Preparation ----
    
    cm_data_tmp = cm_data_included[date_new <=tmp_date]
    cm_data_tmp = cm_data_tmp[date_new > (tmp_date - months(36))]
    pgm_data_tmp = pgm_data[date_new <= tmp_date]
    pgm_data_tmp = pgm_data_tmp[date_new > (tmp_date - months(36))]

    all_data = data_prep(cm_data = cm_data_tmp, pgm_data = pgm_data_tmp,S = s)
  
    # Pre -Estimation ----
    
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
                        # log1p(milit_exp) + 
                        # log1p(mcw_receiver_rolling)+
                        # log1p(mcw_receiver_acute) +
                        te(avr_lon, avr_lat) +
                        s(name_fac, bs="re"),
                      data  =all_data$train_data_stage_1,family = binomial(),
                      discrete = T, nthreads = 20,use.chol = T)
   
    try_model_2 =bam(future_ged_dummy_sb~ + s(month_id, bs="gp") +
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
                       polity +pgd_capdist+
                       # log(milit_exp) +
                       # pgd_capdist*log1p(mcw_receiver_rolling)+
                       # pgd_capdist*log1p(mcw_receiver_acute) +
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
                        polity + pgd_capdist+
                        # log(milit_exp) +
                        # pgd_capdist*log1p(mcw_receiver_rolling)+
                        # pgd_capdist* log1p(mcw_receiver_acute) +
                        te(long, lat), data  = all_data$train_data_stage_3 ,family = ztpoisson(),
                      discrete = T, nthreads = 20,use.chol = T)

    
    class(try_model_1)[1] = "gam"
    class(try_model_2)[1] = "gam"

    
    # Calibration ----

    all_data$cm_calibrate_1$pred_stage_1 = predict.gam(try_model_1,newdata =all_data$cm_calibrate_1, type = "response")
    all_data$pgm_calibrate_1$pred_stage_1 = all_data$cm_calibrate_1$pred_stage_1[match(all_data$pgm_calibrate_1$key_cm,
                                                                                       all_data$cm_calibrate_1$key_cm)]
    all_data$pgm_calibrate_1$pred_stage_2 = predict.gam(try_model_2,all_data$pgm_calibrate_1, type = "response")
    all_data$pgm_calibrate_1$pred_stage_3 = predict.gam(try_model_3,newdata = all_data$pgm_calibrate_1, type = "response")
    
  
    set.seed(123)
    try_opt_balance = DEoptim(fn=optim_balance, lower=c(0,0), upper=c(1,1),
                              control=list(NP=25, itermax=20,trace=F),pred_data = all_data$pgm_calibrate_1)
    
    set.seed(123)
    try_opt_mse = DEoptim(fn=optim_mse, lower=c(0,0), upper=c(1,1),
                          control=list(NP=25, itermax=20,trace=F),pred_data = all_data$pgm_calibrate_1)
    
    trying_balance = try_opt_balance$member$bestmemit[which(try_opt_balance$member$bestvalit == min(try_opt_balance$member$bestvalit)),]
    trying_mse = try_opt_mse$member$bestmemit[which(try_opt_mse$member$bestvalit == min(try_opt_mse$member$bestvalit)),]
    
    if(length(trying_balance) != 2){
      optimal_thresholds = trying_balance[which.min(apply(trying_balance, 1, sum)),]
    } else {
      optimal_thresholds = trying_balance
    }
    
    if(length(trying_mse) != 2){
      alt_optimal_thresholds = trying_mse[which.min(apply(trying_mse, 1, sum)),]
    } else {
      alt_optimal_thresholds = trying_mse
    }
  
    date_change = paste0("Prediction_new/final_treshold",gsub(pattern = "-",replacement = "_",x = tmp_date), "_s_",s,".csv" )

    writeLines(paste0("Found Tresholds are ", paste(round(alt_optimal_thresholds, digits = 3),collapse = " "), 
                      " and ",paste(round(optimal_thresholds, digits = 3),collapse = " "),"\n"))
    
    # fwrite(data.table(alt_optimal_thresholds,optimal_thresholds),file = date_change)
    
    
    # Estimation (Full Modell) ----
    
    all_data$train_data_stage_1 = rbind(all_data$train_data_stage_1, all_data$calibrate_data_stage_1)
    all_data$train_data_stage_2 = rbind(all_data$train_data_stage_2, all_data$calibrate_data_stage_2)
    all_data$train_data_stage_3 = rbind(all_data$train_data_stage_3, all_data$calibrate_data_stage_3)
    
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
                        # log1p(milit_exp) + 
                        # log1p(mcw_receiver_rolling)+
                        # log1p(mcw_receiver_acute) +
                        te(avr_lon, avr_lat) +
                        s(name_fac, bs="re"),data  =all_data$train_data_stage_1,family = binomial(),
                      discrete = T, nthreads = 20,use.chol = T)
    
    
    try_model_2 =bam(future_ged_dummy_sb~ -1 + s(month_id, bs="gp") +
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
                       polity + pgd_capdist +
                       # log(milit_exp) +
                       # pgd_capdist*log1p(mcw_receiver_rolling)+
                       # pgd_capdist*log1p(mcw_receiver_acute) +
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
                        polity + pgd_capdist +
                        # log(milit_exp) +
                        # pgd_capdist*log1p(mcw_receiver_rolling)+
                        # pgd_capdist* log1p(mcw_receiver_acute) +
                        te(long, lat), data  = all_data$train_data_stage_3 ,family = ztpoisson(),
                      discrete = T, nthreads = 20,use.chol = T)
 
    
    class(try_model_1)[1] = "gam"
    class(try_model_2)[1] = "gam"
    class(try_model_3)[1] = "gam"
    
    
    
    # Prediction ----
    
    gc(full = T)
    
    
    all_data$cm_data_comp$pred_stage_1 = predict.gam(try_model_1,newdata =all_data$cm_data_comp, type = "response")
    
    all_data$pgm_data_comp$pred_stage_1 = all_data$cm_data_comp$pred_stage_1[match(all_data$pgm_data_comp$key_cm,
                                                                                   all_data$cm_data_comp$key_cm)]
    all_data$pgm_data_comp$pred_stage_2 = predict.gam(try_model_2,newdata = all_data$pgm_data_comp, type = "response")
    all_data$pgm_data_comp$pred_stage_3 = predict(try_model_3,newdata = all_data$pgm_data_comp, type = "response")
    
    all_data$pgm_data_comp$higher_than_stage_1 = all_data$pgm_data_comp$pred_stage_1>optimal_thresholds[1]
    all_data$pgm_data_comp$higher_than_stage_2 = (all_data$pgm_data_comp$pred_stage_2>optimal_thresholds[2]) & all_data$pgm_data_comp$higher_than_stage_1
    
    all_data$pgm_data_comp$higher_than_stage_1_alt = all_data$pgm_data_comp$pred_stage_1>alt_optimal_thresholds[1]
    all_data$pgm_data_comp$higher_than_stage_2_alt = (all_data$pgm_data_comp$pred_stage_2>alt_optimal_thresholds[2]) &
      all_data$pgm_data_comp$higher_than_stage_1_alt
    
    all_data$pgm_data_comp$higher_than_stage_1_untuned = all_data$pgm_data_comp$pred_stage_1>0.5
    all_data$pgm_data_comp$higher_than_stage_2_untuned = (all_data$pgm_data_comp$pred_stage_2>0.5) &
      all_data$pgm_data_comp$higher_than_stage_1_untuned
    
    
    
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
    
   
    
    date_change = paste0("Prediction/no_mcw_result_t_",gsub(pattern = "-",replacement = "_",x = tmp_date), "_s_",s,".csv" )
    
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
    
    writeLines(paste(" 1. Finished for t =", tmp_date, "and s =", s, "\n"))
    diff = Sys.time() - time_now
    writeLines(paste(" 2. Time needed for this run: ", round(diff, digits = 3),units(diff), "\n"))
    diff = Sys.time() - time_beginning
    writeLines(paste(" 3. Time since beginning: ", round(diff, digits = 3),units(diff), "\n"))
    
    tada =     mean(abs(result$predicted_log_change-result$observation_log_change) +
                      abs(result$predicted_log_change)*
                      (sign(result$predicted_log_change) != sign(result$observation_log_change))*
                      (abs(result$predicted_log_change-result$observation_log_change)>1))
    writeLines(paste(" 4. TADA (tuned TH)= " , round(tada,digits = 3), "\n"))
    
    tada =     mean(abs(result$predicted_log_change_untuned-result$observation_log_change) +
                      abs(result$predicted_log_change_untuned)*
                      (sign(result$predicted_log_change_untuned) != sign(result$observation_log_change))*
                      (abs(result$predicted_log_change_untuned-result$observation_log_change)>1))
    writeLines(paste(" 5. TADA (not tuned TH)= " , round(tada,digits = 3), "\n"))
    tada =     mean(abs(result$predicted_log_change_alt-result$observation_log_change) +
                      abs(result$predicted_log_change_alt)*
                      (sign(result$predicted_log_change_alt) != sign(result$observation_log_change))*
                      (abs(result$predicted_log_change_alt-result$observation_log_change)>1))
    writeLines(paste(" 6. TADA (alt tuned TH)= " , round(tada,digits = 3), "\n"))
    
    writeLines(paste(" 7. MSE (tuned TH)= " , round(mean((result$predicted_log_change-
                                                            result$observation_log_change)^2),digits = 3), "\n"))
    
    writeLines(paste(" 8. MSE (not tuned TH)= " , round(mean((result$predicted_log_change_untuned-
                                                                result$observation_log_change)^2),digits = 3), "\n"))
    
    writeLines(paste(" 9. MSE (only 0) = " , round(mean(log1p(result$observation)^2),digits = 3), "\n"))
    
    writeLines(paste(" 10. MSE (alt-tuned TH)= " , round(mean((result$predicted_log_change_alt-
                                                                 result$observation_log_change)^2),digits = 3), "\n"))
    
    
    rm(try_model_1, try_model_2, try_model_3,result,all_data,pgm_data_tmp,cm_data_tmp)
    gc(full = T)
    
  }
}


# 3. Real Forecasting -----

time_beginning = Sys.time()
s_values = 2:7

for(s in s_values) {
  tmp_date = max(cm_data_included$date) + months(s)
  rem_used = mem_used()
  
  writeLines(paste0("Starting with t = ", as.character(tmp_date), " and s = ",s, ";RAM use ",    
                    round(as.numeric(rem_used)/1000000000,digits = 2)," GB \n"))
  time_now = Sys.time()

  # Preparation ----
  all_data = data_prep_forecast(cm_data = cm_data_included, pgm_data = pgm_data,S = s)

  # Pre -Estimation ----
  
  
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
                      # log1p(milit_exp) + 
                      # log1p(mcw_receiver_rolling)+
                      # log1p(mcw_receiver_acute) +
                      te(avr_lon, avr_lat) +
                      s(name_fac, bs="re"),data  =all_data$train_data_stage_1,
                    family = binomial(),
                    discrete = T, nthreads = 20,use.chol = T)
  
  
  try_model_2 =bam(future_ged_dummy_sb~ + s(month_id, bs="gp") +
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
                     polity +pgd_capdist + 
                     # log(milit_exp) +
                     # pgd_capdist*log1p(mcw_receiver_rolling)+
                     # pgd_capdist*log1p(mcw_receiver_acute) +
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
                      polity + pgd_capdist + 
                      # log(milit_exp) +
                      # pgd_capdist*log1p(mcw_receiver_rolling)+
                      # pgd_capdist* log1p(mcw_receiver_acute) +
                      te(long, lat), data  = all_data$train_data_stage_3 ,family = ztpoisson(),
                    discrete = T, nthreads = 20,use.chol = T)
  
  class(try_model_1)[1] = "gam"
  class(try_model_2)[1] = "gam"
  class(try_model_3)[1] = "gam"
  
  
  # Calibration ----
  # Get the predictions 
  all_data$cm_calibrate_1$pred_stage_1 = predict.gam(try_model_1,newdata =all_data$cm_calibrate_1, type = "response")
  all_data$pgm_calibrate_1$pred_stage_1 = all_data$cm_calibrate_1$pred_stage_1[match(all_data$pgm_calibrate_1$key_cm,
                                                                                     all_data$cm_calibrate_1$key_cm)]
  all_data$pgm_calibrate_1$pred_stage_2 = predict.gam(try_model_2,all_data$pgm_calibrate_1, type = "response")
  all_data$pgm_calibrate_1$pred_stage_3 = predict.gam(try_model_3,newdata = all_data$pgm_calibrate_1, type = "response")
  
  # Set seed for reproducability
  set.seed(123)
  try_opt_balance = DEoptim(fn=optim_balance, lower=c(0,0), upper=c(1,1),
                            control=list(NP=25, itermax=20,trace=F),pred_data = all_data$pgm_calibrate_1)
  
  set.seed(123)
  try_opt_mse = DEoptim(fn=optim_mse, lower=c(0,0), upper=c(1,1),
                        control=list(NP=25, itermax=20,trace=F),pred_data = all_data$pgm_calibrate_1)
  
  trying_balance = try_opt_balance$member$bestmemit[which(try_opt_balance$member$bestvalit == min(try_opt_balance$member$bestvalit)),]
  trying_mse = try_opt_mse$member$bestmemit[which(try_opt_mse$member$bestvalit == min(try_opt_mse$member$bestvalit)),]
  
  # Use the found thresholds with lowest l1 norm 
  if(length(trying_balance) != 2){
    optimal_thresholds = trying_balance[which.min(apply(trying_balance, 1, sum)),]
  } else {
    optimal_thresholds = trying_balance
  }
  
  if(length(trying_mse) != 2){
    alt_optimal_thresholds = trying_mse[which.min(apply(trying_mse, 1, sum)),]
  } else {
    alt_optimal_thresholds = trying_mse
  }
  

  date_change = paste0("Prediction/real_forecast_threshold",gsub(pattern = "-",replacement = "_",x = tmp_date), "_s_",s,".csv" )
  
  writeLines(paste0("Found Tresholds are ", paste(round(alt_optimal_thresholds, digits = 3),collapse = " "), 
                    " and ",paste(round(optimal_thresholds, digits = 3),collapse = " "),"\n"))
  
  # fwrite(data.table(alt_optimal_thresholds,optimal_thresholds),file = date_change)
  
  
  # Estimation (Full Modell) ----
  
  all_data$train_data_stage_1 = rbind(all_data$train_data_stage_1, all_data$calibrate_data_stage_1)
  all_data$train_data_stage_2 = rbind(all_data$train_data_stage_2, all_data$calibrate_data_stage_2)
  all_data$train_data_stage_3 = rbind(all_data$train_data_stage_3, all_data$calibrate_data_stage_3)
  
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
                      # log1p(milit_exp) + 
                      # log1p(mcw_receiver_rolling)+
                      # log1p(mcw_receiver_acute) +
                      te(avr_lon, avr_lat) +
                      s(name_fac, bs="re"),data  =all_data$train_data_stage_1,family = binomial(),
                    discrete = T, nthreads = 20,use.chol = T)
  
  try_model_2 =bam(future_ged_dummy_sb~ -1 + s(month_id, bs="gp") +
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
                     polity + pgd_capdist + 
                     # log(milit_exp) +
                     # pgd_capdist*log1p(mcw_receiver_rolling)+
                     # pgd_capdist*log1p(mcw_receiver_acute) +
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
                      polity + pgd_capdist + 
                      # log(milit_exp) +
                      # pgd_capdist*log1p(mcw_receiver_rolling)+
                      # pgd_capdist* log1p(mcw_receiver_acute) +
                      te(long, lat), data  = all_data$train_data_stage_3 ,family = ztpoisson(),
                    discrete = T, nthreads = 20,use.chol = T)
  
  
  
  
  class(try_model_1)[1] = "gam"
  class(try_model_2)[1] = "gam"
  class(try_model_3)[1] = "gam"
  
  
  
  # Prediction ----
  
  gc(full = T)
  
  
  all_data$cm_data_comp$pred_stage_1 = predict.gam(try_model_1,newdata =all_data$cm_data_comp, type = "response")
  
  all_data$pgm_data_comp$pred_stage_1 = all_data$cm_data_comp$pred_stage_1[match(all_data$pgm_data_comp$key_cm,
                                                                                 all_data$cm_data_comp$key_cm)]
  all_data$pgm_data_comp$pred_stage_2 = predict.gam(try_model_2,newdata = all_data$pgm_data_comp, type = "response")
  all_data$pgm_data_comp$pred_stage_3 = predict.gam(try_model_3,newdata = all_data$pgm_data_comp, type = "response")
  
  all_data$pgm_data_comp$higher_than_stage_1 = all_data$pgm_data_comp$pred_stage_1>optimal_thresholds[1]
  all_data$pgm_data_comp$higher_than_stage_2 = (all_data$pgm_data_comp$pred_stage_2>optimal_thresholds[2]) & all_data$pgm_data_comp$higher_than_stage_1
  
  all_data$pgm_data_comp$higher_than_stage_1_alt = all_data$pgm_data_comp$pred_stage_1>alt_optimal_thresholds[1]
  all_data$pgm_data_comp$higher_than_stage_2_alt = (all_data$pgm_data_comp$pred_stage_2>alt_optimal_thresholds[2]) &
    all_data$pgm_data_comp$higher_than_stage_1_alt
  
  all_data$pgm_data_comp$higher_than_stage_1_untuned = all_data$pgm_data_comp$pred_stage_1>0.5
  all_data$pgm_data_comp$higher_than_stage_2_untuned = (all_data$pgm_data_comp$pred_stage_2>0.5) &
    all_data$pgm_data_comp$higher_than_stage_1_untuned
  
  
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
  
  
  date_change = paste0("Prediction/real_no_mcw_forecast_t_",gsub(pattern = "-",replacement = "_",x = tmp_date), "_s_",s,".csv" )
  
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

  
  rm(try_model_1, try_model_2, try_model_3,result,all_data)
  gc(full = T)
  
}



