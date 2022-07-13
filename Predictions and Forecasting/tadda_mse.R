####################################################### MSE vs. TADDA #######################################################


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Fritz. et al. (2021) filtered


#Generate data file directory
data_files = paste0("Prediction_filtered/",list.files("Prediction_filtered/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(data_files_evaluation_forecasts, fread))


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
print(xtable(results_mse_sb, type="latex", digits = 8),file="mse_sb.tex")



#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Fritz. et al. (2021) and ICEWS



data_files = paste0("Prediction_ICEWS/",list.files("Prediction_ICEWS/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]

data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(data_files_evaluation_forecasts, fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}


results_mse_icews = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                            tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]

#save as latex
print(xtable(results_mse_icews, type="latex", digits = 8),file="mse_icews.tex")



#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Fritz. et al. (2021) and ICEWS, Try2



data_files = paste0("Prediction_ICEWS_try2/",list.files("Prediction_ICEWS_try2/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]

data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(data_files_evaluation_forecasts, fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}


results_mse_icews_try2 = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                               tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]

#save as latex
print(xtable(results_mse_icews, type="latex", digits = 8),file="mse_icews_try2.tex")





#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Fritz. et al. (2021) and ICEWS, Only Low Level



data_files = paste0("Prediction_ICEWS_Low/",list.files("Prediction_ICEWS_Low/"))
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]

data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]
results = rbindlist(lapply(data_files_evaluation_forecasts, fread))


#MSE and TADDA
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}


results_mse_icews_low_level = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                                    tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48)), by = s]

#save as latex
print(xtable(results_mse_icews, type="latex", digits = 8),file="mse_icews_low_level.tex")






