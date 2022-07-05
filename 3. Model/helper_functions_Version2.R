################################################################# Helper Functions: VERSION 2 ####################################################################

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
############################
#Functions for Calibration:
############################

#MSE:
#Function calculating the MSE between the predicted intensities and the observed intensities used in the calibration of the threshold 

optim_mse = function(lambda, pred_data){ #function requires lambda and data containing the predictions
  pred_data$higher_than_stage_1 = pred_data$pred_stage_1>lambda[1] #variable containing predictions from stage 1 that are higher than lambda
  pred_data$higher_than_stage_2 = (pred_data$pred_stage_2>lambda[2]) &pred_data$higher_than_stage_1 #variable containing predictions from stage 2 that are higher than lambda,
                                                                                                    #given that the predictions are higher than lambda in stage 1
  
  pred_data$pred_final = NA #generate new variable with NA
  pred_data$pred_final[!pred_data$higher_than_stage_1] = 0 #replace NA with 0 if predictions of stage 1 !>lambda
  pred_data$pred_final[!pred_data$higher_than_stage_2] = 0 #replace NA with 0 if predictions of stage 2 !>lambda, given that they were >lambda in stage 1
  pred_data$pred_final[is.na(pred_data$pred_final)] = 
    pred_data$pred_stage_3[is.na(pred_data$pred_final)] #replace NA with predictions from stage 3 if still missing
  return(mean((log1p( pred_data$future_ged_best_sb) - log1p(pred_data$pred_final))^2)) #calculate MSE: Average of (log1p(Number of future state-based conflicts)-log1p(Predicted number))^2
}

#Balance Statistic:
#Function calculating the Balance Statistic between the predicted intensities and the observed intensities used in the calibration of the threshold 

optim_balance = function(lambda, pred_data){ #function requires lambda and data containing the predictions
  pred_data$higher_than_stage_1 = pred_data$pred_stage_1>lambda[1] #variable containing predictions from stage 1 that are higher than lambda
  pred_data$higher_than_stage_2 = (pred_data$pred_stage_2>lambda[2]) & pred_data$higher_than_stage_1 #variable containing predictions from stage 2 that are higher than lambda,
                                                                                                    #given that the predictions are higher than lambda in stage 1
  
  
  pred_data$pred_final = NA #generate new variable with NA
  pred_data$pred_final[!pred_data$higher_than_stage_1] = 0 # replace NA with 0 if predictions of stage 1 !>lambda
  pred_data$pred_final[!pred_data$higher_than_stage_2] = 0 #replace NA with 0 if predictions of stage 2 !>lambda, given that they were >lambda in stage 1
  pred_data$pred_final[is.na(pred_data$pred_final)] = pred_data$pred_stage_3[is.na(pred_data$pred_final)] #replace NA with predictions from stage 3 if still missing
  return(abs((sum(log1p( pred_data$future_ged_best_sb)) - sum(log1p(pred_data$pred_final))))) #calculate BS: absolute difference of the sum of log1p(number of future state based conflicts) and log1p(number of predicted conflicts) 
}


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Model Preparation Function using the CM-data set
#Relevant for: Predictions


#Function lagging the cm (country-month) data by S months between the target variable and all covariates 

prepare_model_cm = function(cm_data,S){ # function requires the cm_data set and the lag (a vector from 2:7)
  
  cm_data = cm_data[order(country_id,month_id)] #order cm_data by country ID and month ID 
  
  month_max =  max(cm_data$month_id) #set the max month ID (488)
  
  #NEW VARIABLES:
  cm_data[, future_target:=c(ged_dummy_sb[-(1:S)],rep(NA, S)), by=country_id] #new variable future_target: by country ID, 
                                                                              #the last S observations of state-based dummy are shifted up by S and replaced by NA
  
  cm_data[, date_target:=c(date[-(1:S)],rep(NA, S)), by=country_id] #new variable date_target: by country ID, 
                                                                    #the last S observations of date  are shifted up by S and replaced by NA
  
  #NEW DATA SET:
  cm_data_lag = cm_data[!is.na(future_target)] #cm_data_lag does not include the observations with NA in the above generated variables
  
  #NEW VARIABLE
  cm_data_lag$ged_dummy_sb = cm_data_lag$future_target #replace ged_dummy_sb of cm_data_lag with future_target
  
  return(cm_data_lag)
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Model Preparation Function PGM-data set:
#Relevant for: Predictions

#Function lagging the pgm (prio grid-month) data by S months between the target variable and all covariates 

prepare_model_pgm = function(pgm_data,S){ #function requires the pgm_data set and the lag (a vector from 2:7)
  
  pgm_data = pgm_data[order(pg_id,month_id)] #order pgm_data by country ID and month ID 

  month_max =  max(pgm_data$month_id) #set the max month ID (488)
  
  #NEW VARIABLES:
  pgm_data[, future_ged_dummy_sb:=c(ged_dummy_sb[-(1:S)],rep(NA, S)), by=pg_id] #new variable future_ged_dummy_sb: by Prio ID, 
                                                                                #the last S observations of state-based dummy are shifted up by S and replaced by NA
  
  pgm_data[, future_ged_best_sb:=c(ged_best_sb[-(1:S)],rep(NA, S)), by=pg_id] #new variable future_ged_best_sb: by Prio ID, 
                                                                              #the last S observations of number of state-based conflicts are shifted up by S and replaced by NA
  
  pgm_data[, date_target:=c(date[-(1:S)],rep(NA, S)), by=pg_id] #new variable date_target: by Prio ID, 
                                                                #the last S observations of date are shifted up by S and replaced by NA
  
  #NEW DATA SET:
  pgm_data_lag = pgm_data[!is.na(future_ged_dummy_sb)] #pgm_data_lag does not include the observations with NA in the above generated variables
  
  
  return(pgm_data_lag)
  
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Data Preparation Function:
#Relevant for prediction, using the above generated functions

#Function uses the above generated functions and creates 23 data sets used for training, test and calibration when predicting

data_prep = function(cm_data,pgm_data, S) { #function requires cm_data and pgm_data

  ################################################################################################
  #Prepare data sets for training, test and calibration
  
  #STAGE 1:
  cm_data_1 = prepare_model_cm(cm_data =cm_data,S = S) #cm_data_1 includes lag and the two generate variables future_target and date_target
  data_stage_1 = cm_data_1 #date_stage1 equals the cm_data_1
  tmp_data = cm_data_1[ged_dummy_sb == 1, c("country_id", "key_cm")] #tmp_data includes only key_cm (countries and months) with state-based conflicts
  pgm_data_1 = prepare_model_pgm(pgm_data,S = S) #pgm_data_1 includes lag and the three generate variables future_ged_dummy_sb, date_target and future_ged_best_sb 
  
  #STAGE 2:
  data_stage_2 = pgm_data_1[key_cm %in% tmp_data$key_cm] #data_stage_2 includes only observations (prio grids) where a country and month experienced a state-based conflict
  
  #STAGE 3:
  data_stage_3 = data_stage_2[future_ged_dummy_sb>0] #data_stage_3 includes only observations (prio grids) where a prio grid and month experienced a state-based conflict
  
  
  ################################################################################################
  #Generate data sets for training, test and calibration
  
  #STAGE 1:
  test_data_stage_1 = data_stage_1[date_target == max(date_target)] #Test Data Stage 1: includes only observations of max date_target (e.g only 2020-08-01)
  calibrate_data_stage_1 = data_stage_1[date_target == max(date_target) - months(S)] #Calibration Data Stage 1: includes only observations of max date_target minus lag months (e.g 2020-06-01 for s=2)
  train_data_stage_1 = data_stage_1[date_target < (max(date_target) - months(S))] #Training Data Stage 1: includes only observations of date target lower than in calibration data (e.g 2020-05-01 till 1990-01-01 for s=2)
  do_nothing_data_1 = data_stage_1[date_target %within% interval(max(date_target) - months(S) + months(1), #Irrelevant Data Stage 1:includes observations in interval between calibration and test data date (e.g 2020-07-01 for s=2)
                                                                 max(date_target) - months(1))]
  
  #STAGE 2:
  test_data_stage_2 = data_stage_2[date == max(date)]
  calibrate_data_stage_2 = data_stage_2[date_target == (max(date_target) - months(S))]
  train_data_stage_2 = data_stage_2[date_target < (max(date_target) - months(S))]
  do_nothing_data_2 = data_stage_2[date_target %within% interval(max(date_target) - months(S) + months(1),
                                                                 max(date_target) - months(1))]
  
  #STAGE 3:
  test_data_stage_3 = data_stage_3[date == max(date)]
  calibrate_data_stage_3 = data_stage_3[date_target == (max(date_target) - months(S))]
  train_data_stage_3 = data_stage_3[date_target < (max(date_target) - months(S))]
  do_nothing_data_3 = data_stage_3[date_target %within% interval(max(date_target) - months(S) + months(1),
                                                                 max(date_target) - months(1))]
  
  ################################################################################################
  #Parallel new and modified data sets
  
  #GENERATE SECOND STAGE DATA SETS WHERE PRIO GRIDS WITH COUNTRY-MONTH OBSERVATIONS INCLUDED WITH NO SB-CONFLICT
  
  test_two_stage = pgm_data_1[date == max(date)] #pgm_data_1 (with three new included variables of prepare_model_pgm) with only max date observations (e.g 2020-08-01)
  calibrate_two_stage = pgm_data_1[date_target == (max(date_target) - months(S))] #pgm_data_1 (with three new included variables of prepare_model_pgm) with only max date observations minus lag months (e.g 2020-06-01 for s=2)
  train_two_stage = pgm_data_1[date_target < (max(date_target) - months(S))] #pgm_data_1 (with three new included variables of prepare_model_pgm) with only observations with date lower than calibration date (e.g 2020-05-01 till 1990-01-01 for s=2)
  

  all_pgm_data_train = pgm_data_1[date_target <= (max(date_target) - months(S))] #pgm_data_1 data set with observations where date target smaller equal calibration date (e.g 2020-06-01 till 1990-01-01 for s=2)

  pgm_calibrate_1 = pgm_data_1[date_target  == (max(date_target) - months(S))] #equals calibrate_two_stage (prio grid of country-months with no sb included)
  cm_calibrate_1 =cm_data_1[date_target  == (max(date_target) - months(S))] #cm_data_1 (with two new included variables of prepare_model_cm) with only max date observations minus lag months (e.g 2020-06-01 for s=2)

  pgm_data_1 = pgm_data_1[date == max(date)] #equals test_two_stage
  cm_data_1 =cm_data_1[date == max(date)] #cm_data_1 (with two new included variables of prepare_model_cm) with only max date observations (e.g 2020-08-01)
  
  return(list(stage_1 = data_stage_1,stage_2 = data_stage_2,stage_3 = data_stage_3, 
              test_data_stage_1 = test_data_stage_1, 
              test_data_stage_2 = test_data_stage_2,
              test_data_stage_3 = test_data_stage_3, 
              train_data_stage_1 = train_data_stage_1, 
              train_data_stage_2 = train_data_stage_2, 
              train_data_stage_3 = train_data_stage_3, 
              test_two_stage = test_two_stage, 
              train_two_stage = train_two_stage, 
              calibrate_two_stage = calibrate_two_stage, 
              calibrate_data_stage_1 = calibrate_data_stage_1,
              calibrate_data_stage_2 = calibrate_data_stage_2, 
              calibrate_data_stage_3 = calibrate_data_stage_3, 
              cm_data_comp = cm_data_1, 
              pgm_data_comp = pgm_data_1,
              all_pgm_data_train = all_pgm_data_train, 
              pgm_calibrate_1 = pgm_calibrate_1, cm_calibrate_1 = cm_calibrate_1, 
              do_nothing_data_1= do_nothing_data_1, 
              do_nothing_data_2= do_nothing_data_2, 
              do_nothing_data_3 = do_nothing_data_3))
}




#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Data Preparation Forecasting for CM-data set:
#Relevant for forecasting data

#Function lagging the cm (country-month) data by S months between the target variable and all covariates 

prepare_model_cm_forecast = function(cm_data,S, forecast_date){ #functions requires the cm_data, the lag S (a vector from 2:7) and the forecast date
  
  cm_data = cm_data[order(country_id,month_id)] #order cm data by country and month ID 
  
  month_max =  max(cm_data$month_id) #find max month ID (488)
  
  #NEW VARIABLES
  cm_data[, future_target:=c(ged_dummy_sb[-(1:S)],rep(NA, S)), by=country_id] #new variable future_target: by country ID, 
                                                                              #the last S observations of state-based conflicts are shifted up by S and replaced by NA
  
  cm_data[, date_target:=c(date[-(1:S)],rep(NA, S)), by=country_id] #new variable date_target: by country ID, 
                                                                    #the last S observations of date are shifted up by S and replaced by NA
  
  cm_data$date_future = cm_data$date + months(S) #new variable date_future: by country ID, 
                                                 #the date observations + lag months (e.g for date 2020-08-01, date future is 2020-10-01 for s=2)
  
  #NEW DATA SET and VARIABLES
  cm_data_additional = cm_data[date_future == forecast_date] #cm data set with date future being the forecasting date (e.g 2020-10-01 for s=2), forecast_data defined below
  cm_data_additional$ged_dummy_sb = cm_data_additional$future_target # replace state based dummy with future state based dummy
  
  cm_data_lag = cm_data[!is.na(future_target)] #cm_data_lag does not include the observations with NA in the above generated variables
  cm_data_lag$ged_dummy_sb = cm_data_lag$future_target # replace state based dummy with future state based dummy
 
  return(rbind(cm_data_lag, cm_data_additional))

}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Data Preparation Forecasting for PGM-data set:
#Relevant for forecasting data

#Function lagging the pgm (prio grid-month) data by S months between the target variable and all covariates 

prepare_model_pgm_forecast = function(pgm_data,S, forecast_date){ #functions requires the pgm_data, the lag S (a vector from 2:7) and the forecast date
  
  pgm_data = pgm_data[order(pg_id,month_id)]  #order pgm data by country and month ID

  month_max =  max(pgm_data$month_id) #find max month ID (488)

  #NEW VARIABLES
  pgm_data[, future_ged_dummy_sb:=c(ged_dummy_sb[-(1:S)],rep(NA, S)), by=pg_id] #new variable future_ged_dummy_sb: by Prio ID, 
                                                                                #the last S observations of state-based dummy are shifted up by S and replaced by NA
  
  pgm_data[, future_ged_best_sb:=c(ged_best_sb[-(1:S)],rep(NA, S)), by=pg_id] #new variable future_ged_best_sb: by Prio ID, 
                                                                              #the last S observations of number of state-based conflicts are shifted up by S and replaced by NA
  
  pgm_data[, date_target:=c(date[-(1:S)],rep(NA, S)), by=pg_id] #new variable date target: by Prio ID, 
                                                                #the last S observations of date target are shifted up by S and replaced by NA
  
  pgm_data$date_future = pgm_data$date + months(S) #new variable date future: date plus number of lag months (e.g if date 2020-06-01, than 2020-08-01 if s=2)
  
  #NEW DATA SET and VARIABLES
  pgm_data_lag = pgm_data[!is.na(future_ged_dummy_sb)] #pgm_data_lag does not include the observations with NA in the above generated variables 
  pgm_data_additional = pgm_data[date_future == forecast_date] #pgm data set with date future being the forecasting date (e.g 2020-10-01 for s=2), forecast_data defined below
  
  return(rbind(pgm_data_lag,pgm_data_additional))

}



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Data Preparation Forecasting:
#Relevant for forecasting, using the above generated functions

#Function uses the above generated functions and creates 16 data sets used for training, test and calibration when forecasting


data_prep_forecast = function(cm_data,pgm_data, S) { #function requires cm data, pgm data and lag s (vector from 2:7)

  ################################################################################################
  #Prepare data sets for training, test and calibration
  
  #FORECAST DATE
  forecast_date = max(cm_data$date) + months(S)  #Forecasting date is the maximal date plus the lag months (e.g max 2020-08-01, forecast date is 2020-10-01 if s=2)
  
  #STAGE 1
  cm_data_1 = prepare_model_cm_forecast(cm_data =cm_data,S = S, forecast_date = forecast_date) # cm_data_1 includes three new generate variables from above
  data_stage_1 = cm_data_1 #data_stage_1 equals cm_data_1
  tmp_data = cm_data_1[ged_dummy_sb == 1, c("country_id", "key_cm")] #keep only observations where country and month have a state-based conflict
  pgm_data_1 = prepare_model_pgm_forecast(pgm_data,S = S, forecast_date = forecast_date) # pgm_data_1 includes the four new generate variables from above
  
  #STAGE 2
  data_stage_2 = pgm_data_1[key_cm %in% tmp_data$key_cm] #keep only prio grid observations where the country-month had a state based conflict
  
  #STAGE 3
  data_stage_3 = data_stage_2[future_ged_dummy_sb>0] #keep only prio grids of stage 2 where the number of state based conflicts is higher than zero
  
  
  ################################################################################################
  #Generate data sets for training, test and calibration
  
  #STAGE 1
  test_data_stage_1 = data_stage_1[date_future == max(date_future)] #Test Data Stage 1: includes only observations of max date_future (e.g only 2020-10-01 for s=2)
  day_before_test = sort(unique(data_stage_1$date_future),decreasing = T)[2]
  calibrate_data_stage_1 = data_stage_1[date_target == day_before_test] #Calibrate Data Stage 1: includes only observations of two month before max date future (if 2020-10-01 than 2020-08-01)
  train_data_stage_1 = data_stage_1[date_target <= (day_before_test - months(1))] #Training Data Stage 1: includes only observations of date target lower than date of calibration (e.g 2020-07-01 till 1990-01-01 for s=2)
  
  #STAGE 2
  test_data_stage_2 = data_stage_2[date_future == max(date_future)]
  calibrate_data_stage_2 = data_stage_2[date_target == day_before_test]
  train_data_stage_2 = data_stage_2[date_target <= (day_before_test - months(1))]
  
  #STAGE 3
  test_data_stage_3 = data_stage_3[date_future == max(date_future)]
  calibrate_data_stage_3 = data_stage_3[date_target == day_before_test]
  train_data_stage_3 = data_stage_3[date_target <= (day_before_test - months(1))]
  
  ################################################################################################
  #Parallel new and modified data sets
  
  #GENERATE CALIBRATE DATA SETS WHERE PRIO GRIDS WITH COUNTRY-MONTH OBSERVATIONS INCLUDED WITH NO SB-CONFLICT
  
  pgm_calibrate_1 = pgm_data_1[date_target == day_before_test] # #Calibrate Data Stage 1: includes only prio grid-month observations of one month before max date future (if 2020-10-01 than 2020-09-01)
  cm_calibrate_1 =cm_data_1[date_target == day_before_test] #Calibrate Data Stage 1: includes only country-month observations of one month before max date future (if 2020-10-01 than 2020-09-01)
 
  pgm_data_1 = pgm_data_1[date_future == max(date_future)] #Test Data Stage 1: includes only prio grid-month observations of max date_future (e.g only 2020-10-01 for s=2)
  cm_data_1 =cm_data_1[date_future == max(date_future)] #Test Data Stage 1: includes only country-month observations of max date_future (e.g only 2020-10-01 for s=2)
  
  return(list(stage_1 = data_stage_1,stage_2 = data_stage_2,stage_3 = data_stage_3, 
              test_data_stage_1 = test_data_stage_1, 
              test_data_stage_2 = test_data_stage_2,
              test_data_stage_3 = test_data_stage_3, 
              train_data_stage_1 = train_data_stage_1, 
              train_data_stage_2 = train_data_stage_2, 
              train_data_stage_3 = train_data_stage_3, 
              calibrate_data_stage_1 = calibrate_data_stage_1,
              calibrate_data_stage_2 = calibrate_data_stage_2, 
              calibrate_data_stage_3 = calibrate_data_stage_3, 
              cm_data_comp = cm_data_1, 
              pgm_data_comp = pgm_data_1,
              pgm_calibrate_1 = pgm_calibrate_1, cm_calibrate_1 = cm_calibrate_1))
}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords_to_continent = function(points)
{  
  #countriesSP <- getMap(resolution='low')
  countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  list("continent1" = indices$continent ,
       "continent2" = indices$REGION, 
       "country" = indices$ADMIN, 
       "iso3" = indices$ISO3 )
}


coords_to_continent_missing = function(data_missing,countries)
{  
  #countriesSP <- getMap(resolution='low')
  countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  countriesSP =  countriesSP[countriesSP$NAME %in% countries,] 
  centroids <- getSpPPolygonsLabptSlots(countriesSP)
  points = data.table(long = data_missing$longitude, 
                      lat = data_missing$latitude)
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  centroids= SpatialPoints(centroids, proj4string=CRS(proj4string(countriesSP)))  
  
  res = spDists(pointsSP, centroids)
  res = apply(res, 1,which.min)
  
  list("continent1" = countriesSP[[37]][res] ,
       "continent2" = countriesSP[[38]][res], 
       "country" = countriesSP[[6]][res], 
       "iso3" = countriesSP[[1]][res] )
}

