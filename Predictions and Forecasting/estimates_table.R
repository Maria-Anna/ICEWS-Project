####################################################### Estimates Table#######################################################

#Generate Estimates Table for various lags and for training data from 1990-01 to 2020-08 

#------------------------------------------------------------------------------------------------------------------------------------------------#
#For all ICEWS Variables and s=2
#load package
library(knitr)

#load try model 1
load("Prediction_ICEWS/models/try_model_1.RData")
summary_1 = summary(try_model_1)
summary_1
print(xtable(round(summary_1$p.table, digits = 4), type = "latex"), file = "Stage_1_estimates_2.tex")



#load try model 2
load("Prediction_ICEWS/models/try_model_2.RData")
summary_2 = summary(try_model_2)
summary_2
print(xtable(round(summary_2$p.table, digits = 4), type = "latex"), file = "Stage_2_estimates_2.tex")


#load try model 3
load("Prediction_ICEWS/models/try_model_3.RData")
summary_3 = summary(try_model_3)
summary_3
print(xtable(round(summary_3$p.table, digits = 4), type = "latex"), file = "Stage_3_estimates_2.tex")


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For all ICEWS Variables and s=7
#load package
library(knitr)

#load try model 1
load("Prediction_ICEWS/models_7/try_model_1.RData")
summary_1 = summary(try_model_1)
summary_1
print(xtable(round(summary_1$p.table, digits = 4), type = "latex"), file = "Stage_1_estimates_7.tex")


#load try model 2
load("Prediction_ICEWS/models_7/try_model_2.RData")
summary_2 = summary(try_model_2)
summary_2
print(xtable(round(summary_2$p.table, digits = 4), type = "latex"), file = "Stage_2_estimates_7.tex")

#load try model 3
load("Prediction_ICEWS/models_7/try_model_3.RData")
summary_3 = summary(try_model_3)
summary_3
print(xtable(round(summary_3$p.table, digits = 4), type = "latex"), file = "Stage_3_estimates_7.tex")


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For only low level violence ICEWS Variables and s=2
kable(round(summary_1$p.table, digits = 4), format = "latex")







