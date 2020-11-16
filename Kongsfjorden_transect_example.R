#R script kinetic Model Kongsfjorden transect 2017
#As an example only one station is shown this is JE (KF1) 0-1 cm sediment depth

####Loading required packages####
library(ggplot2)
library(openxlsx)
library(nlme)
library(nls2)
library(broom)
library(ggthemes)
library(grid)

####Load data####
dataKF<-read.xlsx("KFoneSheetMtFed.xlsx",1)
#View(dataBags)
#names(dataBags)

####Calculate model with nls#### Use the best starting parameters determined with nlsList####
# At first create a data frame for each sample
JE0to1<-dataKF[dataKF$Sample=="JE0to1",c("Mt","Time","Fediss")]
    JE0to1<-cbind(JE0to1, "Time_min"=JE0to1[,"Time"]/60)
    JE0to1<-cbind(JE0to1, "Time_h"=JE0to1[,"Time_min"]/60)

####define M(0), which is the maximum at the end of the extracation#### 
      M0_JE0to1<-max(JE0to1[,"Fediss"])

####calculate a and v with the nls function####
nlsJE0to1<-nls(Mt~M0_JE0to1*(a/(a+Time))^v, data=JE0to1, start=list(a=1000, v=0.5))
      coefficients(nlsJE0to1)
   
              
####Put results from the model into a data frame####
parametersJE0to1<-tidy(nlsJE0to1) 

####Put all into one data frame#### from here on shown as an example for all samples from all depth of KF1 (=JE)
dfparametersKF<-data.frame(Sample="JE0to1", M0=M0_JE0to1, a=parametersJE0to1[1,"estimate"], v=parametersJE0to1[2, "estimate"])
dfparametersKF<-rbind(dfparametersKF, data.frame(Sample="JE1to2",M0= M0_JE1to2, a= parametersJE1to2[1,"estimate"], v=parametersJE1to2[2, "estimate"]))
dfparametersKF<-rbind(dfparametersKF, data.frame(Sample="JE3to4",M0= M0_JE3to4, a= parametersJE3to4[1,"estimate"], v=parametersJE3to4[2, "estimate"]))
dfparametersKF<-rbind(dfparametersKF, data.frame(Sample="JE6to8",M0= M0_JE6to8, a= parametersJE6to8[1,"estimate"], v=parametersJE6to8[2, "estimate"]))
dfparametersKF<-rbind(dfparametersKF, data.frame(Sample="JE10to13",M0= M0_JE10to13, a= parametersJE10to13[1,"estimate"], v=parametersJE10to13[2, "estimate"]))


####Calculate Rsquared and put into dfparametersKF####
R2nlsJE0to1<-1-(deviance(nlsJE0to1)/sum((JE0to1[,"Mt"]-mean(JE0to1[,"Mt"]))^2))

dfparametersKF<-cbind(dfparametersKF, "R2"=c(R2nlsJE0to1, R2nlsJE1to2, R2nlsJE3to4, R2nlsJE6to8, R2nlsJE10to13))

####calculate a/v und 1+1/v and add that to the data frame####
dfparametersKF<-transform(dfparametersKF, "v/a" = estimate.1/estimate) 
dfparametersKF<-transform(dfparametersKF, "1+1/v" = 1+1/estimate.1)
dfparametersKF

####calculate the modelled Fediss over time for plot and add that to a data frame####
PredKF<-data.frame(Time=seq(0,120000,by=120))#creates a new data frame with more time (in seconds), than the ones that I measured the Fediss at
PredKF<-cbind(PredKF, "Time_min"=PredKF[,"Time"]/60)#adds a column with time in minutes (maybe better to use for the graphs)
PredKF<-cbind(PredKF, "Time_h"=PredKF[,"Time_min"]/60)

PredKF$PredMt<-predict(nlsJE0to1,PredKF) #adds a variable to the data frame called PredMt, that is calculated for each Time saved in the PredTime data frame
    JE0to1_pred<-PredKF
PredKF$PredMt<-predict(nlsJE1to2,PredKF)
    JE1to2_pred<-PredKF
PredKF$PredMt<-predict(nlsJE3to4,PredKF)
    JE3to4_pred<-PredKF
PredKF$PredMt<-predict(nlsJE6to8,PredKF)
    JE6to8_pred<-PredKF
PredKF$PredMt<-predict(nlsJE10to13,PredKF)
    JE10to13_pred<-PredKF

    
                 
####Calculate Fediss from Mt and the parameters of the model& add that to the predictions data frame####
JE0to1_pred<-cbind(JE0to1_pred, "Fediss"=dfparametersKF[dfparametersKF$Sample=="JE0to1","M0"]-JE0to1_pred[,"PredMt"])
JE1to2_pred<-cbind(JE1to2_pred, "Fediss"=dfparametersKF[dfparametersKF$Sample=="JE1to2","M0"]-JE1to2_pred[,"PredMt"])
JE3to4_pred<-cbind(JE3to4_pred, "Fediss"=dfparametersKF[dfparametersKF$Sample=="JE3to4","M0"]-JE3to4_pred[,"PredMt"]) 
JE6to8_pred<-cbind(JE6to8_pred, "Fediss"=dfparametersKF[dfparametersKF$Sample=="JE6to8","M0"]-JE6to8_pred[,"PredMt"])
JE10to13_pred<-cbind(JE10to13_pred, "Fediss"=dfparametersKF[dfparametersKF$Sample=="JE10to13","M0"]-JE10to13_pred[,"PredMt"])



####Create one data frame that contains all predicted data for plots, this was used for the model line in the graphs####
JE0to1_pred<-cbind(JE0to1_pred,"Sample"="JE0to1")
JE1to2_pred<-cbind(JE1to2_pred,"Sample"="JE1to2")
JE3to4_pred<-cbind(JE3to4_pred,"Sample"="JE3to4")
JE6to8_pred<-cbind(JE6to8_pred,"Sample"="JE6to8")
JE10to13_pred<-cbind(JE10to13_pred,"Sample"="JE10to13")


dfKF<-rbind(JE0to1_pred, JE1to2_pred, JE3to4_pred, JE6to8_pred, JE10to13_pred)


####Add initial rate parameters to the parameters data frame####
dfparametersKF<-cbind(dfparametersKF, "initial_rates"=dfparametersKF[,"v.a"]*dfparametersKF[,"M0"]) 

##############save data frame to be opened in excel or any other programm#############
write.table(dfparametersKF, file="dfparametersKF", sep=",")
                                      

