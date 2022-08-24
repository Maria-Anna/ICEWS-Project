########################################################### MSE vs. TADDA #######################################################

#Load necessary packages:
library(xtable)
library(data.table)

rm(list=ls())


#Set path
path_prediction<- "~/ICEWS-Project/3. Model/Predictions/"

#------------------------------------------------------------------------------------------------------------------------------------------------#
#Fritz. et al. (2021) filtered:

#Generate data file directory
data_files = list.files(paste0(path_prediction, "Prediction_filtered/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_filtered/", data_files_evaluation_forecasts), fread))

#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}


results_mse_sb = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                         tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]

#save as latex
#print(xtable(results_mse_sb, type="latex", digits = 8),file="mse_sb.tex")


#------------------------------------------------------------------------------------------------------------------------------------------------#
#ESC Model
#For all ICEWS Variables
#Predictions in Prediction_ICEWS
#All logarithmic variables are included linear in the model, in all stages without any interactions. 

#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}


#Table with MSE and TADDA
results_mse_icews = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                            tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]


#Display Table
results_mse_icews

#save as latex
#print(xtable(results_mse_icews, type="latex", digits = 8),file="mse_icews.tex")


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Low Level Violence Only
#only_low Model
#Only logarithmic low-level-violence Variables are included in the model without interactions.
#Prediction in : Prediction_ICEWS_Low

#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS_Low/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS_Low/", data_files_evaluation_forecasts), fread))

#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#Table with MSE and TADDA
results_mse_icews_low_level = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                    tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]


#Display Table
results_mse_icews_low_level

#save as latex
#print(xtable(results_mse_icews_low_level, type="latex", digits = 8),file="mse_icews_low_level.tex")



#------------------------------------------------------------------------------------------------------------------------------------------------#
#ICEWS without demands variables
#Model : esc_no_dem Predictions in Prediction_ICEWS_Esc
#Demands Variables are excluded from the model. All other variables are included linear, without interactions

#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS_Esc/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS_Esc/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}


#Table with MSE and TADDA
results_mse_icews_escalation = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                         tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]


#Display Table
results_mse_icews_escalation

#save as latex
#print(xtable(results_mse_icews_escalation, type="latex", digits = 8),file="results_mse_icews_escalation.tex")



#------------------------------------------------------------------------------------------------------------------------------------------------#
#CM and PGM Level (with cap factor interaction):
#Model: cm_pgm_int_capfac; Predictions in Prediction_ICEWS_CM_PGM
#All logarithmic variables are interacted with cap_fac, in the 2. and 3. stage. cap_fac is  a boolian variable for the presents of the capital in the prio grid cell.

#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS_CM_PGM/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS_CM_PGM/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}


#Table for MSE and TADDA
results_mse_icews_cm_pgm = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                          tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]


#Display Table
results_mse_icews_cm_pgm

#save as latex
#print(xtable(results_mse_icews_cm_pgm, type="latex", digits = 8),file="results_mse_icews_cm_pgm.tex")


#------------------------------------------------------------------------------------------------------------------------------------------------#
#Escalation Cov with Cap Dist in Stage 2 and Stage 3
# Model: esc_int_capdist, Prediction in Prediction_ICEWS_cm_cap
#All logarithmic variables are interacted with pgd_capdist, the Distance of a grid cell to the capital,  in the 2. and 3. stage.


#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS_cm_cap/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS_cm_cap/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#Table for MSE and TADDA
results_mse_icews_cm_cap = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                         tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]

#Display Table 
results_mse_icews_cm_cap

#save as latex
#print(xtable(results_mse_icews_cm_cap, type="latex", digits = 8),file="results_mse_icews_cm_cap.tex")


#------------------------------------------------------------------------------------------------------------------------------------------------#
#ICEWS, with CM and PGM with no interaction:
#Model : cm_pgm_no_int , Predictions in Prediction_ICEWS_CM_PGM_NO_INT
#All logarithmic variables are included linear in the model, in all stages without any interactions. 


#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS_CM_PGM_NO_INT/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS_CM_PGM_NO_INT/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#Table with MSE and TADDA
results_mse_cm_pgm_no_int = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                        tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]


#Display Table
results_mse_cm_pgm_no_int

#save as latex
#print(xtable(results_mse_cm_pgm_no_int, type="latex", digits = 8),file="results_mse_cm_pgm_no_int.tex")

#------------------------------------------------------------------------------------------------------------------------------------------------#
#Fritz. et al. (2021) and ICEWS, with CM and PGM and Cap Dist Interaction:
#Model: model_cm_pgm_cap_dist , Predictions in Prediction_ICEWS_CM_PGM_CAP_DIST
#All logarithmic variables are interacted with pgd_capdist, the Distance of a grid cell to the capital,  in the 2. and 3. stage. 

#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS_CM_PGM_CAP_DIST/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS_CM_PGM_CAP_DIST/", data_files_evaluation_forecasts), fread))

#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#Table for MSE and TADDA
results_mse_cm_pgm_cap_dist = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                       tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]


#Display Table: 
results_mse_cm_pgm_cap_dist

#save as latex
#print(xtable(results_mse_cm_pgm_cap_dist, type="latex", digits = 8),file="results_mse_cm_pgm_cap_dist.tex")


#------------------------------------------------------------------------------------------------------------------------------------------------#
#ICEWS, Aggregation over 2 Months Window:
#Model: aggreg_2 , Prediction in : Prediction_ICEWS_Aggr_2_months
#Both data sets are aggregated over 2 months. The idea is that the effect of the escalation variables
#is visible after 2 months + current lag. 


#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS_Aggr_2_months/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS_Aggr_2_months/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#Table for MSE and TADDA
results_mse_aggreg_2_months = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                         tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]

#Display Table
results_mse_aggreg_2_months

#save as latex
#print(xtable(results_mse_aggreg_2_months , type="latex", digits = 8),file="results_mse_aggreg_2_months .tex")

#------------------------------------------------------------------------------------------------------------------------------------------------#
#Fritz. et al. (2021) Filtered for two months window:


#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_filtered_window_2/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_filtered_window_2/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#Table for MSE and TADDA
results_filtered_window_2 = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                         tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]


#Display Table
results_filtered_window_2

#save as latex
#print(xtable(results_filtered_window_2, type="latex", digits = 8),file="results_filtered_window_2.tex")

#------------------------------------------------------------------------------------------------------------------------------------------------#
#ICEWS, aggregated events over 3 months:
#Model: aggreg3, Prediction in Prediction_ICEWS_Aggr
#Both data sets are aggregated over 3 months. The idea is that the effect of the escalation variables
#is visible after 3 months + current lag. 

#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_ICEWS_Aggr/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_ICEWS_Aggr/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#Table for MSE and TADDA
results_mse_icews_aggregate = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                         tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]


#Display Table
results_mse_icews_aggregate

#save as latex
#print(xtable(results_mse_icews_aggregate, type="latex", digits = 8),file="results_mse_icews_aggregate.tex")

#------------------------------------------------------------------------------------------------------------------------------------------------#
#Fritz. et al. (2021) filtered for three months window:


#List al Files and make a dataset out of them
data_files = list.files(paste0(path_prediction, "Prediction_filtered_window/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(paste0(path_prediction,"Prediction_filtered_window/", data_files_evaluation_forecasts), fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#Table for MSE and TADDA
results_mse_filtered_three = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                        tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]

#Display Table
results_mse_filtered_three

#save as latex
#print(xtable(results_mse_filtered_three, type="latex", digits = 8),file="results_mse_filtered_three.tex")











