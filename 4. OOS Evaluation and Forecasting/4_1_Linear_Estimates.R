########################################################## Estimation Tables #######################################################

#Load necessary packages
library(gtools)
library(knitr)
library(xtable)

rm(list=ls())

#Set working directory
path_predictions<- "~/ICEWS-Project/3. Model/Predictions/"


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For all ICEWS Variables
#ESC Model
#All logarithmic variables are included linear in the model, in all stages without any interactions. 

#Lag: s=2

#load Models
load(paste0(path_predictions, "Prediction_ICEWS/models/try_model_1_s2.RData"))
load(paste0(path_predictions, "Prediction_ICEWS/models/try_model_2_s2.RData"))
load(paste0(path_predictions, "Prediction_ICEWSmodels/try_model_3_s2.RData"))

#Estimates for all three stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3


#LateX Table
#print(xtable(round(summary_1$p.table, digits = 4), type = "latex"), file = "Stage_1_estimates_2.tex")
#print(xtable(round(summary_2$p.table, digits = 4), type = "latex"), file = "Stage_2_estimates_2.tex")
#print(xtable(round(summary_3$p.table, digits = 4), type = "latex"), file = "Stage_3_estimates_2.tex")


#Lag: s=7

#load Models
load(paste0(path_predictions,"Prediction_ICEWS/models/try_model_1_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS/models/try_model_2_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS/models/try_model_3_s7.RData"))


#Estimates for all tree stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3


#LateX Tables
#print(xtable(round(summary_1$p.table, digits = 4), type = "latex"), file = "Stage_1_estimates_7.tex")
#print(xtable(round(summary_2$p.table, digits = 4), type = "latex"), file = "Stage_2_estimates_7.tex")
#print(xtable(round(summary_3$p.table, digits = 4), type = "latex"), file = "Stage_3_estimates_7.tex")


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Escalation Cov (no demands)
#Lag: s=2

load(paste0(path_predictions,"Prediction_ICEWS_Esc/models/try_model_1_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Esc/models/try_model_2_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Esc/models/try_model_3_s2.RData"))

#Estimates for all three Stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance 
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#LateX Table
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)


#Lag: s=7
load(paste0(path_predictions,"Prediction_ICEWS_Esc/models/try_model_1_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Esc/models/try_model_2_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Esc/models/try_model_3_s7.RData"))

#Estimates for all tree stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#LateX Table
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Escalation Cov with Cap Distance Interaction
#Lag: s=2

#Load Datasets
load(paste0(path_predictions,"Prediction_ICEWS_cm_cap/models/try_model_1_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_cm_cap/models/try_model_2_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_cm_cap/models/try_model_3_s2.RData"))

#Estimates for all three Stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#Latex Table
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)


#Lag: s=7

load(paste0(path_predictions,"Prediction_ICEWS_cm_cap/models/try_model_1_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_cm_cap/models/try_model_2_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_cm_cap/models/try_model_3_s7.RData"))

#Estimates for all tree stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#Latex Table
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)

#------------------------------------------------------------------------------------------------------------------------------------------------#
#For CM and PGM

#s=2
load(paste0("Prediction_ICEWS_CM_PGM/models/try_model_1_s2.RData"))
load(paste0("Prediction_ICEWS_CM_PGM/models/try_model_2_s2.RData"))
load(paste0("Prediction_ICEWS_CM_PGM/models/try_model_3_s2.RData"))

#Estimates for all three stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#LateX tables
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)

#Lag: s=7

load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM/models/try_model_1_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM/models/try_model_2_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM/models/try_model_3_s7.RData"))

#Estimates for all tree stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#Latex Table
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)

#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Low Level Violence Only

#s=2
load(paste0(path_predictions,"Prediction_ICEWS_Low/models/try_model_1_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Low/models/try_model_2_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Low/models/try_model_3_s2.RData"))

#Estimates for all tree estimates
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#Latex Table
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)


#s=7
load(paste0(path_predictions,"Prediction_ICEWS_Low/models/try_model_1_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Low/models/try_model_2_s7.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Low/models/try_model_3_s7.RData"))

#Estimates for all tree estimates
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#Latex Table
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)

#------------------------------------------------------------------------------------------------------------------------------------------------#
#For CM and PGM without Cap Factor

#s=2
load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM_NO_INT/models/try_model_1_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM_NO_INT/models/try_model_2_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM_NO_INT/models/try_model_3_s2.RData"))

#Linear Estimates for all three stages     
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#LateX Tables
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For CM and PGM with PGM Cap Dist

#s=2
load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM_CAP_DIST/models/try_model_1_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM_CAP_DIST/models/try_model_2_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_CM_PGM_CAP_DIST/models/try_model_3_s2.RData"))

#Linear estimates for all tree stages
summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#LateX Tables
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For 3 Months Aggregation

#Lag: s=2
load(paste0(path_predictions,"Prediction_ICEWS_Aggr/models/try_model_1_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Aggr/models/try_model_2_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Aggr/models/try_model_3_s2.RData"))

#Extract Linear Estimates for all tree stages
summary_1 = summary(try_model_1)
summary_1
summary_2 = summary(try_model_2)
summary_2
summary_3 = summary(try_model_3)
summary_3

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#LateX Tables
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)

#------------------------------------------------------------------------------------------------------------------------------------------------#
#For 2 Months Aggregation
#s=2
load(paste0(path_predictions,"Prediction_ICEWS_Aggr_2_months/models/try_model_1_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Aggr_2_months/models/try_model_2_s2.RData"))
load(paste0(path_predictions,"Prediction_ICEWS_Aggr_2_months/models/try_model_3_s2.RData"))

#Linear Estimates for all tree stages
summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

#Extract significance
summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

#LateX tables
#xtable(summary_1, digits = 4)
#xtable(summary_2, digits = 4)
#xtable(summary_3, digits = 4)

