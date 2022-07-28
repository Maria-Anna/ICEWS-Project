####################################################### Estimation Tables#######################################################

#Estimate Tables for lag s=2 and s=7
#Estimate Tables for various models
#Estimate Tables with training data from 1990-01 to 2020-08 

#------------------------------------------------------------------------------------------------------------------------------------------------#
#For all ICEWS Variables
#s=2
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


#s=7

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
#For Escalation Cov
#s=2

load("Prediction_ICEWS_Esc/models/try_model_1_s2.RData")
load("Prediction_ICEWS_Esc/models/try_model_2_s2.RData")
load("Prediction_ICEWS_Esc/models/try_model_3_s2.RData")

summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

xtable(summary_1, digits = 4)
xtable(summary_2, digits = 4)
xtable(summary_3, digits = 4)


#s=7
load("Prediction_ICEWS_Esc/models/try_model_1_s7.RData")
load("Prediction_ICEWS_Esc/models/try_model_2_s7.RData")
load("Prediction_ICEWS_Esc/models/try_model_3_s7.RData")

summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

xtable(summary_1, digits = 4)
xtable(summary_2, digits = 4)
xtable(summary_3, digits = 4)


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Escalation Cov with Cap Distance Interaction
#s=2

load("Prediction_ICEWS_cm_cap/models/try_model_1_s2.RData")
load("Prediction_ICEWS_cm_cap/models/try_model_2_s2.RData")
load("Prediction_ICEWS_cm_cap/models/try_model_3_s2.RData")

summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

xtable(summary_1, digits = 4)
xtable(summary_2, digits = 4)
xtable(summary_3, digits = 4)


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For CM and PGM
#s=2
load("Prediction_ICEWS_CM_PGM/models/try_model_1_s2.RData")
load("Prediction_ICEWS_CM_PGM/models/try_model_2_s2.RData")
load("Prediction_ICEWS_CM_PGM/models/try_model_3_s2.RData")

summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

xtable(summary_1, digits = 4)
xtable(summary_2, digits = 4)
xtable(summary_3, digits = 4)


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For Low Level Violence Only
#s=2
load("Prediction_ICEWS_Low/models/try_model_1_s2.RData")
load("Prediction_ICEWS_Low/models/try_model_2_s2.RData")
load("Prediction_ICEWS_Low/models/try_model_3_s2.RData")

summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

xtable(summary_1, digits = 4)
xtable(summary_2, digits = 4)
xtable(summary_3, digits = 4)

#------------------------------------------------------------------------------------------------------------------------------------------------#
#For CM and PGM without Cap Factor
#s=2
load("Prediction_ICEWS_CM_PGM_NO_INT/models/try_model_1_s2.RData")
load("Prediction_ICEWS_CM_PGM_NO_INT/models/try_model_2_s2.RData")
load("Prediction_ICEWS_CM_PGM_NO_INT/models/try_model_3_s2.RData")

summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

xtable(summary_1, digits = 4)
xtable(summary_2, digits = 4)
xtable(summary_3, digits = 4)


#------------------------------------------------------------------------------------------------------------------------------------------------#
#For CM and PGM with PGM Cap
#s=2
load("Prediction_ICEWS_CM_PGM_CAP_DIST/models/try_model_1_s2.RData")
load("Prediction_ICEWS_CM_PGM_CAP_DIST/models/try_model_2_s2.RData")
load("Prediction_ICEWS_CM_PGM_CAP_DIST/models/try_model_3_s2.RData")

summary_1 = summary(try_model_1)
summary_2 = summary(try_model_2)
summary_3 = summary(try_model_3)

summary_1<-as.data.frame(summary_1$p.table)
summary_1$Significance<-stars.pval(summary_1[,4])
summary_2<-as.data.frame(summary_2$p.table)
summary_2$Significance<-stars.pval(summary_2[,4])
summary_3<-as.data.frame(summary_3$p.table)
summary_3$Significance<-stars.pval(summary_3[,4])

xtable(summary_1, digits = 4)
xtable(summary_2, digits = 4)
xtable(summary_3, digits = 4)






