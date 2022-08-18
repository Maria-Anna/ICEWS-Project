########################################################################### PLOTS AND TABLES##############################################################################

#Remark: Fritz et al. (2021) plots R script

#Load necessary packages
rm(list=ls())

source('helper_functions.R')
library(mgcv)
library(MASS)
library(grid)
library(data.table)
library(countreg)
library(lubridate)
library(pryr)
library(cshapes)
library(sf)
library(viridis)
library(ggpubr)
library(cowplot)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##################
#Data Preparation
##################

#Load data sets and country list
cm_data= fread("Data/cm_data.csv")
load("Data/pgm_data.RData")
country_list = fread( "Data/country_list.csv")
load("Data/pgm_data.RData")

#Modify data sets

#cm_data:
cm_data_included = cm_data
cm_data_included = cm_data_included[order(month_id,country_id)]
cm_data_included$month_rescaled
cm_data_included$date = as.Date(cm_data_included$date)
min_month = min(pgm_data$date)
cm_data_included$month_rescaled =  cm_data_included$month_id - min(cm_data_included$month_id)
cm_data_included$date_new = ymd(min_month)  + months(cm_data_included$month_rescaled)
cm_data_included$delay_sb = cm_data_included$ged_dummy_sb 

#pgm_data
pgm_data = pgm_data[order(month_id, pg_id)]
min_month = min(pgm_data$date)
pgm_data$month_rescaled =  pgm_data$month_id - min(pgm_data$month_id)
pgm_data$date_new = ymd(min_month)  + months(pgm_data$month_rescaled)

#tmp data
tmp_data = cm_data_included[,.(country_name, country_id)]
tmp_data = tmp_data[, .(country_id = names(table(country_id))[which.max(table(country_id))]), by=country_name]
cm_data_included$country_id = tmp_data$country_id[match(cm_data_included$country_name,tmp_data$country_name)]
pgm_data$country_id = tmp_data$country_id[match(pgm_data$country_name,tmp_data$country_name)]
pgm_data_split = split(pgm_data, factor(pgm_data$pg_id))

#tmp_result
tmp_result = lapply(pgm_data_split, function(x){
  tmp_table_id = table(x$country_id)
  tmp_table_name = table(x$country_name)
  return(data.table(pg_id = x$pg_id[1], 
                    country_name = names(tmp_table_name)[which.max(tmp_table_name)], 
                    country_id = names(tmp_table_id)[which.max(tmp_table_id)]))
  
})


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##################
#Extras
##################

#Change all South Sudan Prio grid IDs (the problem is that they are overwritten since they are Sudan for more years than South Sudan)
tmp_result = rbindlist(tmp_result)
ids_south_sudan = unique(pgm_data[country_name == "South Sudan", (pg_id)])
tmp_result$country_name[tmp_result$pg_id %in% ids_south_sudan] = "South Sudan"
tmp_result$country_id[tmp_result$pg_id %in% ids_south_sudan] = 246


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##################
#Load Forecasts
##################

#Search for the corresponding files: differentiate between result (2017-01 till 2019-12) and forecast (2020-06 and 2020-12)
data_files = paste0("Prediction_ICEWS/",list.files("Prediction_ICEWS/"))
data_files_evaluation_forecasts_no_mcw = data_files[grep(pattern = "no_mcw_result_t", data_files)]
data_files_real_forecasts_no_mcw = data_files[grep(pattern = "no_mcw_forecast", data_files)]
data_files_real_forecasts = data_files[grep(pattern = "real_mcw_forecast_t_", data_files)]
data_files_evaluation_forecasts = data_files[grep(pattern = "with_mcw_result_t_", data_files)]

#Start with: Evaluation Forecasts (forecasts with MCW) - gen results
results = rbindlist(lapply(data_files_evaluation_forecasts, fread))
results$country_id = tmp_result$country_id[match(results$pg_id,  tmp_result$pg_id)]
results$country_name = tmp_result$country_name[match(results$pg_id,  tmp_result$pg_id)]
results$country_lon = cm_data$avr_lon[match(results$country_id,cm_data$country_id)]
results$country_lat = cm_data$avr_lat[match(results$country_id,cm_data$country_id)]
results$pg_lon =  pgm_data$long[match(results$pg_id,pgm_data$pg_id)]
results$pg_lat =  pgm_data$lat[match(results$pg_id,pgm_data$pg_id)]
results$error_ln = log1p(results$observation) - log1p(results$prediction)
results$country_iso3 = cm_data$country_iso3[match(results$country_id,cm_data$country_id)]
results$country_gwo = pgm_data$country_gwo[match(results$country_id,pgm_data$country_id)]

#Correct for missing gwo numbers of Sudan
results[is.na(country_gwo), "country_gwo"] = 625

#Correct for iso3 codes
results$country_name_gwstates = states::gwstates$country_name[match(results$country_name,states::gwstates$country_name)]
results$country_name_gwstates[results$country_name == "Zimbabwe" ] = "Zimbabwe (Rhodesia)"
results$country_name_gwstates[results$country_name == "Congo, DRC" ] = "Congo, Democratic Republic of (Zaire)"
results$country_name_gwstates[results$country_name == "Tanzania" ] = "Tanzania/Tanganyika"
results$country_name_gwstates[results$country_name == "Cote d'Ivoire" ] = "Cote D'Ivoire"
results$country_name_gwstates[results$country_name == "Burkina Faso" ] = "Burkina Faso (Upper Volta)"
results$country_name_gwstates[results$country_name == "The Gambia" ] = "Gambia"
results$country_iso3_corr = states::gwstates$gwc[match(results$country_name_gwstates,states::gwstates$country_name)]
results$country_gwo_corr = states::gwstates$gwcode[match(results$country_name_gwstates,states::gwstates$country_name)]
results$id = paste(results$date, results$s, results$pg_id,sep = "_")


#Continue with: Evaluation Forecasts (forecasts with NO MCW) - gen results_no_mcw
results_no_mcw = rbindlist(lapply(data_files_evaluation_forecasts_no_mcw, fread))
results_no_mcw$id = paste(results_no_mcw$date, results_no_mcw$s, results_no_mcw$pg_id,sep = "_")
#for results save new variable
results$prediction_no_mcw= results_no_mcw$prediction

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##################
#MSE and TADDA
##################

#get log change observed and predicted from results
obs_delta = results$observation_log_change
pred_delta = results$predicted_log_change

#generate function to estimate TADDA
tadda = function(obs_delta, pred_delta, epsilon = 0.048){
  mean((abs(obs_delta- pred_delta) + abs(pred_delta)*(sign(obs_delta) == sign(pred_delta))* 
          ((abs(obs_delta-pred_delta)> epsilon))))
}

#for MSE and TADDA
results$predicted_log_change_no_mcw = log1p(results$prediction_no_mcw) - log1p(results$observation_s) #save under results the predicted log change with no mcw

#Save MSE and TADDA for MCW and no MCW (the benchmark results indicated in the paper are not available)

results_mse = results[,.(mse_mcw = mean((log1p(observation) - log1p(prediction))^2),
                         mse_no_mcw = mean((log1p(observation) - log1p(prediction_no_mcw))^2),
                         tadda_mcw = tadda(observation_log_change,predicted_log_change, 0.48), 
                         tadda_no_mcw = tadda(observation_log_change,predicted_log_change_no_mcw, 0.48)
                         ),
                         by = s]

#generate LATEX table to save the results
library(kableExtra)
kable(results_mse,format = "latex",digits = 3)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#####################
#Load Real Forecasts
#####################

#Start with: Real Forecasts (forecasts with MCW) - gen_results_forecasts

results_real_forecasts = rbindlist(lapply(data_files_real_forecasts, fread))
results_real_forecasts$country_id = tmp_result$country_id[match(results_real_forecasts$pg_id,  tmp_result$pg_id)]
results_real_forecasts$country_name = tmp_result$country_name[match(results_real_forecasts$pg_id,  tmp_result$pg_id)]
results_real_forecasts$country_lon = cm_data$avr_lon[match(results_real_forecasts$country_id,cm_data$country_id)]
results_real_forecasts$country_lat = cm_data$avr_lat[match(results_real_forecasts$country_id,cm_data$country_id)]
results_real_forecasts$pg_lon =  pgm_data$long[match(results_real_forecasts$pg_id,pgm_data$pg_id)]
results_real_forecasts$pg_lat =  pgm_data$lat[match(results_real_forecasts$pg_id,pgm_data$pg_id)]
results_real_forecasts$error_ln = log1p(results_real_forecasts$observation) - log1p(results_real_forecasts$prediction)
results_real_forecasts$country_iso3 = cm_data$country_iso3[match(results_real_forecasts$country_id,cm_data$country_id)]
results_real_forecasts$country_gwo = pgm_data$country_gwo[match(results_real_forecasts$country_id,pgm_data$country_id)]


#Correct for missing gwo numbers of Sudan
results_real_forecasts[is.na(country_gwo), "country_gwo"] = 625


#Correct for iso3 codes
results_real_forecasts$country_name_gwstates = states::gwstates$country_name[match(results_real_forecasts$country_name,states::gwstates$country_name)]
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Zimbabwe" ] = "Zimbabwe (Rhodesia)"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Congo, DRC" ] = "Congo, Democratic Republic of (Zaire)"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Tanzania" ] = "Tanzania/Tanganyika"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Cote d'Ivoire" ] = "Cote D'Ivoire"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Burkina Faso" ] = "Burkina Faso (Upper Volta)"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "The Gambia" ] = "Gambia"
results_real_forecasts$country_iso3_corr = states::gwstates$iso3c[match(results_real_forecasts$country_name_gwstates,states::gwstates$country_name)]
results_real_forecasts$country_gwo_corr = states::gwstates$gwcode[match(results_real_forecasts$country_name_gwstates,states::gwstates$country_name)]

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##############################
#Prepare Results for each Task
##############################


# 1.: True forecasts for Oct. 2020 – March 2021

results_real_forecasts_hand_in = results_real_forecasts[,.(date, prediction, observation_s, pg_id, predicted_log_change)]
fwrite(results_real_forecasts_hand_in, file = "Raw/ViEWSpred_competition_cornelius_fritz_task_1.csv") #save as csv

# 2. : Forecasts for Jan. 2017 to Dec. 2019.

results_evaluation_forecasts_hand_in = results[,.(date, s,pg_id, prediction,prediction_alt,observation, observation_s, predicted_log_change,observation_log_change)]
results_evaluation_forecasts_hand_in_task_2 = results_evaluation_forecasts_hand_in[date %within% interval(ymd("2017-01-01"),ymd("2019-12-01"))]
results_evaluation_forecasts_hand_in_task_2[,.(mse_final = mean((log1p(observation) - log1p(prediction))^2)), by = s]
fwrite(results_evaluation_forecasts_hand_in_task_2, file = "Raw/ViEWSpred_competition_cornelius_fritz_task_2.csv") #save as csv

# 3. : Forecasts for Jan. 2014 through Dec. 2016.

results_evaluation_forecasts_hand_in_task_3 = results_evaluation_forecasts_hand_in[date %within% interval(ymd("2014-01-01"),ymd("2016-12-01"))]
fwrite(results_evaluation_forecasts_hand_in_task_3, file = "Raw/ViEWSpred_competition_cornelius_fritz_task_3.csv") #save as csv



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##############################
#Plots
##############################

#FIRST: Plots of Evaluation Forecasting


#Create directory for the predictions
if(!"Plots" %in% dir()) {
  dir.create(path = "Plots")
}


#Generate map
world_data <- cshp(date=as.Date("2012-1-01"), useGW=TRUE) #create world map
world_data = st_as_sf(world_data)
summary_results = results[,.(mean_err = mean(error), 
                             mean_obs = mean(observation), 
                             mean_pred = mean(prediction), 
                             country_gwo = country_gwo_corr[1], 
                             country_id = country_id[1], 
                             country_name = country_name[1], 
                             country_iso3 = country_iso3_corr[1], 
                             country_lat = country_lat[1], 
                             country_lon = country_lon[1], 
                             pg_lat = pg_lat[1], 
                             pg_lon = pg_lon[1]), by = .(pg_id,s)]


#subset world data to keep only prio grid IDs of data set
map_data_1 = world_data[world_data$gwcode %in% results$country_gwo_corr,]
dates = seq.Date(ymd("2017-01-01"), ymd("2019-12-01"),by = "month")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Figure 1 of Fritz et al. (2021)

for(tmp in 1:length(dates)){
  tmp_date = dates[tmp]
  sub_results = results[date == tmp_date]
  country_data = sub_results[,.(observation = sum(observation),
                                prediction = sum(prediction),
                                prediction_stage_1 = mean(pred_1)), by = .(country_name,country_gwo_corr,country_iso3_corr)]
  map_data_1$observation_bin = country_data$observation[match(map_data_1$gwcode,country_data$country_gwo_corr)]>0
  map_data_1$prediction_bin = country_data$prediction_stage_1[match(map_data_1$gwcode,country_data$country_gwo_corr)]
  map_data_1$observation = country_data$observation[match(map_data_1$gwcode,country_data$country_gwo_corr)]
  map_data_1$prediction = country_data$prediction[match(map_data_1$gwcode,country_data$country_gwo_corr)]>0
  
  
  sub_countries = c(531,520,501,500,626,530)
  sub_map_data_1 = map_data_1[map_data_1$gwcode %in% sub_countries,]
  micro_results = sub_results[(sub_results$country_gwo_corr %in% sub_countries )& (s == 2),]
  
  #STAGE 1
  a = ggplot() + 
    theme_pubr() +
    ggtitle("Probabity Prediction") +
    geom_sf(data = sub_map_data_1, aes(fill = prediction_bin),col = "black") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) + 
    theme(legend.position="bottom") +
    scale_fill_viridis(option = "D","",discrete = F)
  
  b = ggplot() + 
    theme_pubr() +
    ggtitle("Thresholded Prediction") +
    geom_sf(data = sub_map_data_1, aes(fill = prediction),col = "black") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) + 
    theme(legend.position="bottom") +
    scale_fill_viridis(option = "D","",discrete = T)
  
  title <- ggdraw() +
    draw_label(
      "Stage 1",
      fontface = 'bold',
      x = 0,
      hjust = -8.1
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  # plot_grid(
  #   title, plot_grid(a,b,ncol = 2),
  #   ncol = 1,
  #   # rel_heights values control vertical title margins
  #   rel_heights = c(0.03, 1)
  # )
  # 
  
  
  ggsave2(filename =  paste0("Plots/stage_1_",dates[tmp],".pdf"),
          plot_grid(
            title, plot_grid(a,b,ncol = 2),
            ncol = 1,
            # rel_heights values control vertical title margins
            rel_heights = c(0.03, 1)
          ),width = 12,height = 5
  )
  
  #STAGE 2
  
  a = ggplot() + 
    theme_pubr() +
    ggtitle(" ") +
    geom_tile(data =micro_results, aes(x = pg_lon, y = pg_lat, fill = pred_2))+
    geom_sf(data = sub_map_data_1, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) + 
    scale_fill_viridis(option = "D","",discrete = F)+
    theme(legend.position="bottom") 
  
  b = ggplot() + 
    theme_pubr() +
    ggtitle("") +
    geom_tile(data =micro_results, aes(x = pg_lon, y = pg_lat, fill = prediction>0))+
    geom_sf(data = sub_map_data_1, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) + 
    scale_fill_viridis(option = "D","",discrete = T)+
    theme(legend.position="bottom")  
  
  title <- ggdraw() +
    draw_label(
      "Stage 2",
      fontface = 'bold',
      x = 0,
      hjust = -8.1
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  # plot_grid(
  #   title, plot_grid(a,b,ncol = 2),
  #   ncol = 1,
  #   # rel_heights values control vertical title margins
  #   rel_heights = c(0.03, 1)
  # )
  
  ggsave2(filename =  paste0("Plots/stage_2_",dates[tmp],".pdf"),
          plot_grid(
            title,  plot_grid(a,b,ncol = 2),
            ncol = 1,
            # rel_heights values control vertical title margins
            rel_heights = c(0.03, 1)
          ),width = 12,height = 5
  )
  
  #STAGE 3/ PREDICTION
  
  a = ggplot() + 
    theme_pubr() +
    ggtitle("Predicted Change") +
    geom_tile(data =micro_results, aes(x = pg_lon, y = pg_lat, fill = predicted_log_change))+
    geom_sf(data = sub_map_data_1, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) + 
    scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000", limits = c(-3.5,4.5))+
    theme(legend.position="bottom") 
  
  c = get_legend(a)
  
  a = ggplot() + 
    theme_pubr() +
    ggtitle("Predicted Change") +
    geom_tile(data =micro_results, aes(x = pg_lon, y = pg_lat, fill = predicted_log_change))+
    geom_sf(data = sub_map_data_1, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) + 
    scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000", limits = c(-3.5,4.5))+
    guides(fill = F)
  
  b = ggplot() + 
    theme_pubr() +
    ggtitle("Observed Change") +
    geom_tile(data =micro_results, aes(x = pg_lon, y = pg_lat, fill = observation_log_change))+
    geom_sf(data = sub_map_data_1, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank())+ 
    scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000", limits = c(-3.5,4.5))+
    guides(fill = F)
  
  
  title <- ggdraw() +
    draw_label(
      "Prediction",
      fontface = 'bold',
      x = 0,
      hjust = -5.9
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  
  
  ggsave2(filename =  paste0("Plots/prediction",dates[tmp],".pdf"),
          plot_grid(
            title, plot_grid(b,a,ncol = 2), c,
            ncol = 1,
            # rel_heights values control vertical title margins
            rel_heights = c(0.03, 1.,0.1)
          ),width = 12,height = 5
  )
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#SECOND: Plots of Effects


if(!"Smooth" %in% dir(path = "Plots")) {
  dir.create(path = "Plots/Smooth")
}



plot_smooths = function(info = info_1,font = 20, numbers = 20,min_date = min(cm_data$date),
                        min_id = min(cm_data$month_id),font_main = 20,font_sub = 20, 
                        coef_names= c("Month", "Time Since OS",
                                      "Time Since NS", "Time Since SB",
                                      "loc", "rand") ){
  plots = list()
  title = paste0(letters[1:length(coef_names)], ")")
  y_axis = paste0("f(",coef_names, ")")
  x_axis = coef_names
  for(i in 1:length(info)){
    tmp_y_axis =y_axis[i]
    tmp_x_axis =x_axis[i]
    
    plot_data = data.frame("coef" = info[[i]]$fit, "time" = info[[i]]$x,
                           "se_lower" = info[[i]]$fit - qnorm(p  = 0.975)*info[[i]]$se, 
                           "se_upper" = info[[i]]$fit + qnorm(p  = 0.975)*info[[i]]$se)
    if(tmp_x_axis == "Month") {
      plot_data$time_scale = plot_data$time - min_id
      plot_data$date = ymd(min_date) + months(plot_data$time_scale)
      
      plot =   ggplot(data = plot_data,aes(x = date,y = coef))+
        geom_line(lwd = 0.8) +
        theme_pubr(base_size = font) +
        geom_ribbon(aes(ymin = se_lower,ymax = se_upper),alpha= 0.2) +
        theme(plot.title = element_text(hjust = 0.5,size = font_main)) +
        theme(plot.subtitle = element_text(hjust = 0.5, size = font_sub)) +
        guides(color = FALSE, shape = FALSE) +
        ylab(tmp_y_axis) +
        xlab(tmp_x_axis) +
        ggtitle(title[i]) +
        geom_hline(yintercept = 0,lty = 2) 
    } else {
      plot =   ggplot(data = plot_data,aes(x = time,y = coef))+
        geom_line(lwd = 0.8) +
        theme_pubr(base_size = font) +
        geom_ribbon(aes(ymin = se_lower,ymax = se_upper),alpha= 0.2) +
        theme(plot.title = element_text(hjust = 0.5,size = font_main)) +
        theme(plot.subtitle = element_text(hjust = 0.5, size = font_sub)) +
        guides(color = FALSE, shape = FALSE) +
        ylab(tmp_y_axis) +
        xlab(tmp_x_axis) +
        ggtitle(title[i]) +
        geom_hline(yintercept = 0,lty = 2) 
    }
    
    plots[[i]] = plot
  }
  
  return(plots)
}



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#THIRD: Smooth plots (of non linear effects)

#load package
library(knitr)

#load try model 1
load("Prediction/models/try_model_1.RData")
summary_1 = summary(try_model_1)

kable(round(summary_1$p.table, digits = 4), format = "latex")



info_1 = plot(try_model_1, select = 0, n = 366)
plots_1 = plot_smooths(info_1)

for(i in 1:(length(plots_1)-2)) {
  ggsave2(plot = plots_1[[i]],filename = paste0("Plots/Smooth/Stage_1_",i,".pdf" ),width = 5,height = 5)
}

#load try model 2
load("Prediction/models/try_model_2.RData")
summary_2 = summary(try_model_2)

kable(round(summary_2$p.table, digits = 4), format = "latex")

info_2 = plot(try_model_2, select = 0, n = 366)
plots_2 = plot_smooths(info_2,coef_names = c("Month", "Time Since OS",
                                             "Time Since NS", "Time Since SB",
                                             "loc"))
for(i in 1:(length(plots_2)-1)) {
  ggsave2(plot = plots_2[[i]],filename = paste0("Plots/Smooth/Stage_2_",i,".pdf" ),width = 5,height = 5)
  
}

#load try model 3
load("Prediction/models/try_model_3.RData")
summary_3 = summary(try_model_3)

kable(round(summary_3$p.table, digits = 4), format = "latex")

info_3 = plot(try_model_3, select = 0,n = 366)
plots_3 =plot_smooths(info_3,coef_names = c("Month", "Time Since OS",
                                            "Time Since NS", "Time Since SB", "Intensity SB",
                                            "loc"))
for(i in 1:(length(plots_3)-1)) {
  ggsave2(plot = plots_3[[i]],filename = paste0("Plots/Smooth/Stage_3_",i,".pdf" ),width = 5,height = 5)
  
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#FORTH: Month estimate plots

font = 20
font_main = 20
font_sub = 20

if(!"Month" %in% dir(path = "Plots")) {
  dir.create(path = "Plots/Month")
}

info_1 = summary(try_model_1) #for model 1
plot_data = data.table(month = 2:12, 
                       coef = info_1$p.coeff[2:(2+10)], 
                       stderr = info_1$se[2:(2+10)], 
                       upper = info_1$p.coeff[2:(2+10)]+ qnorm(p  = 0.975)*info_1$se[2:(2+10)],
                       lower = info_1$p.coeff[2:(2+10)]- qnorm(p  = 0.975)*info_1$se[2:(2+10)])

pdf("Plots/Month/monthly_stage_1.pdf", width = 5,height = 5) #save as pdf

ggplot(data = plot_data,aes(x = factor(month),y = coef, ymin=upper, ymax = lower))+
  geom_pointrange(lwd = 0.8) +
  theme_pubr(base_size = font) +
  theme(plot.title = element_text(hjust = 0.5,size = font_main)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = font_sub)) +
  guides(color = FALSE, shape = FALSE) +
  xlab("Month") +
  ylab("Effect") +
  ggtitle("Stage 1") +
  geom_hline(yintercept = 0,lty = 2) 

dev.off()

info_1 = summary(try_model_2) #for model 2
plot_data = data.table(month = 2:12, 
                       coef = info_1$p.coeff[2:(2+10)], 
                       stderr = info_1$se[2:(2+10)], 
                       upper = info_1$p.coeff[2:(2+10)]+ qnorm(p  = 0.975)*info_1$se[2:(2+10)],
                       lower = info_1$p.coeff[2:(2+10)]- qnorm(p  = 0.975)*info_1$se[2:(2+10)])

pdf("Plots/Month/monthly_stage_2.pdf", width = 5,height = 5) #save as pdf

ggplot(data = plot_data,aes(x = factor(month),y = coef, ymin=upper, ymax = lower))+
  geom_pointrange(lwd = 0.8) +
  theme_pubr(base_size = font) +
  theme(plot.title = element_text(hjust = 0.5,size = font_main)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = font_sub)) +
  guides(color = FALSE, shape = FALSE) +
  xlab("Month") +
  ylab("Effect") +
  ggtitle("Stage 2") +
  geom_hline(yintercept = 0,lty = 2) 
dev.off()

info_1 = summary(try_model_3) #for model 3
plot_data = data.table(month = 2:12, 
                       coef = info_1$p.coeff[2:(2+10)], 
                       stderr = info_1$se[2:(2+10)], 
                       upper = info_1$p.coeff[2:(2+10)]+ qnorm(p  = 0.975)*info_1$se[2:(2+10)],
                       lower = info_1$p.coeff[2:(2+10)]- qnorm(p  = 0.975)*info_1$se[2:(2+10)])

pdf("Plots/Month/monthly_stage_3.pdf", width = 5,height = 5) #save as pdf

ggplot(data = plot_data,aes(x = factor(month),y = coef, ymin=upper, ymax = lower))+
  geom_pointrange(lwd = 0.8) +
  theme_pubr(base_size = font) +
  theme(plot.title = element_text(hjust = 0.5,size = font_main)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = font_sub)) +
  guides(color = FALSE, shape = FALSE) +
  xlab("Month") +
  ylab("Effect") +
  ggtitle("Stage 3") +
  geom_hline(yintercept = 0,lty = 2) 
dev.off()


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#FIFTH: Spatial and random effect Plots 

#load world data 
world_data <- cshp(date=as.Date("2012-1-01"), useGW=TRUE)
world_data = st_as_sf(world_data)

#load data_c and data_pg
load("Prediction/models/data_c.RData")
load("Prediction/models/data_pg.RData")

#modify in order to plot random and spatial effects
country_results = results[,.(country_gwo = country_gwo_corr[1], 
                             country_id = country_id[1], 
                             country_name = country_name[1], 
                             country_iso3 = country_iso3_corr[1], 
                             country_lat = country_lat[1], 
                             country_lon = country_lon[1]), by = .(country_name)]
sub_results = results[date == "2017-01-01"]
country_data = sub_results[,.(observation = sum(observation),
                              prediction = sum(prediction)), 
                           by = .(country_name,country_gwo_corr,country_iso3_corr)]
terms_model_1 = data.table(predict(try_model_1, type = "terms", newdata = data_c))
terms_model_2 = data.table(predict(try_model_2, type = "terms", newdata = data_pg))
data_pg$spatial_stage_2 = terms_model_2$`te(long,lat)`
country_deaths = pgm_data[, .(sum(ged_best_sb)), by = country_name]
data_pg$spatial_stage_2[!data_pg$country_name %in% country_deaths$country_name[country_deaths$V1> 25]] = NA
terms_model_3 = data.table(predict(try_model_3, type = "terms", newdata = data_pg))
data_pg$spatial_stage_3 = terms_model_3$`te(long,lat)`
data_pg$spatial_stage_3[!data_pg$country_name %in% country_deaths$country_name[country_deaths$V1> 25]] = NA
country_data$random = terms_model_1$`s(name_fac)`[match(country_data$country_name, data_c$country_name)]
country_data$fixed = terms_model_1$`te(avr_lon,avr_lat)`[match(country_data$country_name, data_c$country_name)]
map_data_1$random = country_data$random[match(map_data_1$gwcode,country_data$country_gwo_corr)]
map_data_1$fixed = country_data$fixed[match(map_data_1$gwcode,country_data$country_gwo_corr)]

lon_lat_dt = sub_results[, .(max_lat = max(pg_lat), 
                             min_lat = min(pg_lat),
                             max_lon = max(pg_lon), 
                             min_lon = min(pg_lon)),by = country_name]
lim_lat = c(-35, 
            lon_lat_dt$max_lat[lon_lat_dt$country_name == "Tunisia"])
lim_lon = c(lon_lat_dt$min_lon[lon_lat_dt$country_name == "Senegal"], 
            lon_lat_dt$max_lon[lon_lat_dt$country_name == "Somalia"])


pdf("Plots/Smooth/random_spatial_effects.pdf") #save as pdf random spatial effects

ggplot() + 
  theme_pubr() +
  ggtitle("Random Effect Stage 1") + #random effect stage 1
  geom_sf(data = map_data_1, aes(fill = random),col = "black") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) + 
  theme(legend.position="bottom") +
  coord_sf(xlim = lim_lon, ylim = lim_lat)+ 
  scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000")


ggplot() + 
  theme_pubr() +
  ggtitle("Spatial Effect Stage 1") + #spatial effect stage 1
  geom_sf(data = map_data_1, aes(fill = fixed),col = "black") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) + 
  theme(legend.position="bottom") +
  coord_sf(xlim = lim_lon, ylim = lim_lat)+ 
  scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000")


ggplot() + 
  theme_pubr() +
  ggtitle("Spatial Effect Stage 2") + #spatial effect stage 2
  geom_tile(data =data_pg, aes(x = long, y = lat, fill = spatial_stage_2))+
  geom_sf(data = map_data_1, col = "black", alpha = 0.00001) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) + 
  coord_sf(xlim = lim_lon, ylim = lim_lat)+ 
  scale_fill_viridis(option = "D","",discrete = F)+
  theme(legend.position="bottom") 

ggplot() + 
  theme_pubr() +
  ggtitle("Spatial Effect Stage 3") + #spatial effect stage 3
  geom_tile(data =data_pg, aes(x = long, y = lat, fill = spatial_stage_3))+
  geom_sf(data = map_data_1, col = "black", alpha = 0.00001) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) + 
  coord_sf(xlim = lim_lon, ylim = lim_lat)+ 
  scale_fill_viridis(option = "D","",discrete = F,guide = guide_colourbar( barwidth = 9))+
  theme(legend.position="bottom") 

dev.off()


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#SIXTH: Real Forecasts

dates = sort(unique(results_real_forecasts$date))

for(tmp in 1:length(dates)){
  tmp_date = dates[tmp]
  sub_results = results_real_forecasts[date == tmp_date]
  
  sub_countries = c(436,475,434,432)
  sub_map_data_1 = map_data_1[map_data_1$GWCODE %in% sub_countries,]
  micro_results = sub_results[(sub_results$country_gwo_corr %in% sub_countries ),]
  union_map_data_1 = st_union(sub_map_data_1)
  union_map_data_1 = st_difference(sub_map_data_1)
  
  ggplot() +
    theme_pubr() +
    ggtitle(paste(substr(micro_results$date[1],0,7))) +
    geom_tile(data =micro_results, aes(x = pg_lon, y = pg_lat, fill = predicted_log_change))+
    geom_sf(data = sub_map_data_1, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) +
    scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000")+
    theme(legend.position="bottom")
  
  
  a =  ggplot() +
    theme_pubr() +
    ggtitle(paste(substr(micro_results$date[1],0,7))) +
    geom_tile(data =micro_results, aes(x = pg_lon, y = pg_lat, fill = predicted_log_change))+
    geom_sf(data = sub_map_data_1, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) +
    scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000")+
    theme(legend.position="bottom")
  
  ggsave2(filename =  paste0("Plots/forecast_",gsub(pattern = "-", "_",tmp_date),".pdf"),a,
          width = 5.5,height = 5.5)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#SEVENTH: Zoom Forecast Plot

#load package
library(cowplot)

#Start with: Real Forecasts (forecasts with MCW) - gen results_real_forecasts

results_real_forecasts = rbindlist(lapply(data_files_real_forecasts, fread))
results_real_forecasts$country_id = tmp_result$country_id[match(results_real_forecasts$pg_id,  tmp_result$pg_id)]
results_real_forecasts$country_name = tmp_result$country_name[match(results_real_forecasts$pg_id,  tmp_result$pg_id)]
results_real_forecasts$country_lon = cm_data$avr_lon[match(results_real_forecasts$country_id,cm_data$country_id)]
results_real_forecasts$country_lat = cm_data$avr_lat[match(results_real_forecasts$country_id,cm_data$country_id)]
results_real_forecasts$pg_lon =  pgm_data$long[match(results_real_forecasts$pg_id,pgm_data$pg_id)]
results_real_forecasts$pg_lat =  pgm_data$lat[match(results_real_forecasts$pg_id,pgm_data$pg_id)]
results_real_forecasts$error_ln = log1p(results_real_forecasts$observation) - log1p(results_real_forecasts$prediction)
results_real_forecasts$country_iso3 = cm_data$country_iso3[match(results_real_forecasts$country_id,cm_data$country_id)]
results_real_forecasts$country_gwo = pgm_data$country_gwo[match(results_real_forecasts$country_id,pgm_data$country_id)]

#Correct for gwo of Sudan
results_real_forecasts[is.na(country_gwo), "country_gwo"] = 625

#Correct for iso3 code 
results_real_forecasts$country_name_gwstates = states::gwstates$country_name[match(results_real_forecasts$country_name,states::gwstates$country_name)]
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Zimbabwe" ] = "Zimbabwe (Rhodesia)"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Congo, DRC" ] = "Congo, Democratic Republic of (Zaire)"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Tanzania" ] = "Tanzania/Tanganyika"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Cote d'Ivoire" ] = "Cote D'Ivoire"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "Burkina Faso" ] = "Burkina Faso (Upper Volta)"
results_real_forecasts$country_name_gwstates[results_real_forecasts$country_name == "The Gambia" ] = "Gambia"
results_real_forecasts$country_iso3_corr = states::gwstates$gwc[match(results_real_forecasts$country_name_gwstates,states::gwstates$country_name)]
results_real_forecasts$country_gwo_corr = states::gwstates$gwcode[match(results_real_forecasts$country_name_gwstates,states::gwstates$country_name)]

dates = sort(unique(results_real_forecasts$date))

#gen PG ID 1
pg_ids_1 = c(149426,149427,149428,149429,149430, 148706,148707,148708,148709,148710, 147986,
             147987,147988,147989,147990, 147266,147267,147268,147269,147270, 146546,146547,146548,
             146549,146550)

#gen PG ID 2
pg_ids_2 = c(114918,114919,114920,114921,114922, 114198,114199,114200,114201,114202, 113478,
             113479,113480,113481,113482, 112758,112759,112760,112761,112762, 112038,112039,112040,
             112041,112042)

#Zoom the map: Get the map results only for mainland Africa (only lat between Senegal and Somalia 
#and lon not lower than South Africa)
lon_lat_dt = sub_results[, .(max_lat = max(pg_lat), 
                             min_lat = min(pg_lat),
                             max_lon = max(pg_lon), 
                             min_lon = min(pg_lon)),by = country_name]
lim_lat = c(-35, 
            lon_lat_dt$max_lat[lon_lat_dt$country_name == "Tunisia"])
lim_lon = c(lon_lat_dt$min_lon[lon_lat_dt$country_name == "Senegal"], 
            lon_lat_dt$max_lon[lon_lat_dt$country_name == "Somalia"])
if(!"Zoom" %in% dir(path = "Plots")) {
  dir.create(path = "Plots/Zoom")
}

tmp = 1

for(tmp in 1:length(dates)){
  tmp_date = dates[tmp]
  sub_results = results_real_forecasts[(date == tmp_date) &
                                         (pg_lat>=lim_lat[1]) &  (pg_lat<=lim_lat[2]) & 
                                         (pg_lon>=lim_lon[1]) &  (pg_lon<=lim_lon[2])]
  
  
  tmp_data = sub_results[pg_id %in% pg_ids_1, .(pg_lon, pg_lat)]
  centroid = unlist(apply(tmp_data, 2, mean))
  # 
  # 
  # sub_map_data_1 = map_data_1[map_data_1$GWCODE %in% sub_countries,]
  # micro_results = sub_results[(sub_results$country_gwo_corr %in% sub_countries ),]
  
  my_map = ggplot() +
    theme_pubr() +
    geom_tile(data =sub_results, aes(x = pg_lon, y = pg_lat, fill = predicted_log_change))+
    geom_sf(data = map_data_1, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) +
    coord_sf(xlim = lim_lon, ylim = lim_lat)+ 
    scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000")
  
  # points = rbindlist(lapply(sub_map_data_1$geometry, function(x){setDT(data.frame(x[[1]][[1]]))}))
  
  # xlim <- range(points$X1)
  # ylim <- range(points$X2)
  
  xlim <- centroid[1]
  ylim <- centroid[2]
  
  location_bbox <- c(xlim, ylim)
  # bbox_radius <- max((location_bbox[2] - location_bbox[1])/2, (location_bbox[4] - location_bbox[3])/2)*1.1
  bbox_radius = 4
  
  # bbox_centroid<- data.frame(x = (location_bbox[1]+location_bbox[2])/2, y = (location_bbox[3]+location_bbox[4])/2) %>%
  #   st_as_sf(coords = c('x','y'), crs = 27700)
  bbox_centroid<- data.frame(x = xlim, y = ylim) %>%
    st_as_sf(coords = c('x','y'), crs = 27700)
  
  buffer <- st_buffer(bbox_centroid, bbox_radius)
  buffer = st_set_crs(buffer,value = "EPSG:4326")
  map_data
  # get data from input map
  map_data <- map_data_1
  zoom_dat <- map_data %>% mutate(colid = factor(row_number())) %>% st_intersection(buffer)
  
  sub_results$pg_lat2 = sub_results$pg_lat
  sub_results$pg_lon2 = sub_results$pg_lon
  sub_results_sf = st_as_sf(sub_results, coords = c("pg_lon2", "pg_lat2"))
  sub_results_sf = st_set_crs(sub_results_sf,value = "EPSG:4326")
  sub_results_sf_tile <- sub_results_sf %>% mutate(colid = factor(row_number())) %>% st_intersection(buffer)
  
  
  map = ggplot() +
    theme_pubr() +
    geom_tile(data =sub_results, aes(x = pg_lon, y = pg_lat, fill = predicted_log_change))+
    geom_sf(data = map_data_1, col = "black", alpha = 0.00001) +
    geom_sf(data = buffer, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(), 
          legend.position="right") +
    coord_sf(xlim = lim_lon, ylim = lim_lat)+ 
    scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000", 
                         limits = range(sub_results$predicted_log_change))
  
  zoomed_map <- ggplot() +
    theme_pubr() +
    geom_tile(data =sub_results_sf_tile, aes(x = pg_lon, y = pg_lat, fill = predicted_log_change))+
    geom_sf(data = zoom_dat, col = "black", alpha = 0.00001) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank()) +
    scale_fill_gradient2("",low = "#005fcc",mid = "grey80",high = "#ff0000", 
                         limits = range(sub_results$predicted_log_change))+
    guides(fill = F)
  
  
  
  legend <- get_legend(map)
  map <- map + theme(legend.position="none")
  title =   ggdraw() +
    draw_label(
      tmp_date,
      fontface = 'bold',
      x = 0,
      hjust = -1.7,size = 20
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  final_plot =   plot_grid(title,plot_grid(legend,map, zoomed_map, nrow = 1,rel_widths = c(0.1,0.8,0.35)), nrow = 2,  rel_heights = c(0.1, 1))
  
  ggsave2(paste0("Plots/Zoom/",gsub(pattern = "-",replacement = "_",x = tmp_date), ".pdf"),final_plot,width = 7)
  
}
