library(dplyr)


data_path = 'C:/Users/spacella/OneDrive - Environmental Protection Agency (EPA)/NEP OA standards analysis/QC Scripts'
#data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/Finalized Data from NEPs/Continuous'
setwd(data_path)
load('NEP_data.Rdata')       # to load data



################################################################################
########################pH QC for Barnaget Bay##################################
################################################################################

#Flags:
#1 = Pass
#2 = Suspect
#3 = Fail
#5 = Not Evaluated


#Add flag column for each relevant variable; start flags as 5 (not evaluated)
data_list$Barnegat$na.test <- rep(5,)
data_list$Barnegat$Flatline.test <- rep(5,)
data_list$Barnegat$GrossRange.test <- rep(5,)
data_list$Barnegat$RateChange.test <- rep(5,)
data_list$Barnegat$Spike.test <- rep(5,)
data_list$Barnegat$AttenuatedSig.test <- rep(5,)

###############################################################################
######### Define QC thresholds for pH #########################################
###############################################################################

#Flatline test: flag strings of repeated data points
##REP_CNT_FAIL = Number of repeated observations for fail flag
##REP_CNT_SUSPECT = Number of repeated observations for suspect flag
REP_CNT_FAIL <- 4*12 # number of repeated values
REP_CNT_SUSPECT <- 4*6 # number of repeated values

#Gross Range test: flag out of range data for pH
##pH_SENSOR_MAX = Maximum value the sensor can output
##pH_SENSOR_MIN = Minimum value the sensor can output
pH_SENSOR_MAX <- 14
pH_SENSOR_MIN <- 0

##pH_USER_MAX = Maximum pH value based on local pH range
##pH_USER_MIN = Minimum pH value based on local pH range
pH_USER_MAX <- 9
pH_USER_MIN <- 6

#Rate of Change test: flag adjacent data points >X standard deviations apart over set period of time
##N_DEV_PH = Number of standard deviations adjacent data points can differ
N_DEV_PH <- 4 #number of standard deviations

##TIM_DEV_PH = Period of time standard deviations are calculated
TIM_DEV_PH <- 12 #hours

##HOURLY_READS = Number of readings sensor puts out per hour
HOURLY_READS <- 4 #per hour

#Spike test: flag spikes between adjacent pH data points
##THRSHLD_LOW = Threshold for absolute value between adjacent pH data for suspect flag
##THRSHLD_HIGH = Threshold for absolute value between adjacent pH data for fail flag
THRSHLD_LOW <- 1.0
THRSHLD_HIGH <- 2.0

#Attenuated Signal test: flag data points subject to biofouling
##MIN_VAR = Minimum variance between a string of data points
MIN_VAR <- 0.01

##TST_TIM_SUSPECT = Number of hours data did not exceed minimum variance for suspect flag
##TST_TIM_FAIL = Number of hours data did not exceed minimum variance for fail flag
TST_TIM_SUSPECT <- 12 #hours
TST_TIM_FAIL <- 24 #hours


###############################################################################
##########Find NANs and flag#################
###############################################################################
d = 1:length(data_list$Barnegat$na.test)

for (i in d) {
  if(is.na(data_list$Barnegat$ph[i]) ==TRUE){
    data_list$Barnegat$na.test[i] <- 3
  }else{
    data_list$Barnegat$na.test[i] <- 1
  }
}

###############################################################################
##########Flatline test: flag strings of repeated data points#################
###############################################################################
  
  b = 1:(length(data_list$Barnegat$ph) - REP_CNT_SUSPECT)
  c = 1:(length(data_list$Barnegat$ph) - REP_CNT_FAIL)
  d = 1:length(data_list$Barnegat$Flatline.test)
  
  for (i in b) {
    if(isTRUE(sd(data_list$Barnegat$ph[i:(i+REP_CNT_SUSPECT-1)]) == 0)){
      data_list$Barnegat$Flatline.test[i:(i+REP_CNT_SUSPECT-1)] <- 2
    }else{
      data_list$Barnegat$Flatline.test[i:(i+REP_CNT_SUSPECT-1)] <- data_list$Barnegat$Flatline.test[i:(i+REP_CNT_SUSPECT-1)]
    }
  }
  
  for (i in c) {
    if(isTRUE(sd(data_list$Barnegat$ph[i:(i+REP_CNT_FAIL-1)]) == 0)){
      data_list$Barnegat$Flatline.test[i:(i+REP_CNT_FAIL-1)] <- 3
    }else{
      data_list$Barnegat$Flatline.test[i:(i+REP_CNT_FAIL-1)] <- data_list$Barnegat$Flatline.test[i:(i+REP_CNT_FAIL-1)]
    }
  }
  
  for (i in d) {
    if(data_list$Barnegat$Flatline.test[i] == 5){
      data_list$Barnegat$Flatline.test[i] <- 1
    }else{
      data_list$Barnegat$Flatline.test[i] <- data_list$Barnegat$Flatline.test[i]
    }
  }
  
  
  ###############################################################################
  ##########Gross Range test: flag out of range data for pH#####################
  ###############################################################################
  #SRP edits below to avoid "for" loops and increase efficiency of QC checks. Old code wih loops is commented out.
  
  data_list$Barnegat$GrossRange.test[which(is.na(data_list$Barnegat$ph) == TRUE)] <- 3
  data_list$Barnegat$GrossRange.test[which(data_list$Barnegat$ph < pH_SENSOR_MIN)] <- 3
  data_list$Barnegat$GrossRange.test[which(data_list$Barnegat$ph > pH_SENSOR_MAX)] <- 3
  data_list$Barnegat$GrossRange.test[which(data_list$Barnegat$ph > pH_SENSOR_MIN & data_list$Barnegat$ph < pH_USER_MIN)] <- 2
  data_list$Barnegat$GrossRange.test[which(data_list$Barnegat$ph > pH_USER_MAX & data_list$Barnegat$ph < pH_SENSOR_MAX)] <- 2
  data_list$Barnegat$GrossRange.test[which(data_list$Barnegat$ph > pH_USER_MIN & data_list$Barnegat$ph < pH_USER_MAX)] <- 1
  
  # e = seq_along(data_list$Barnegat$ph)
  #   
  # for (i in e) {
  #   if(is.na(data_list$Barnegat$ph[i])==FALSE) {
  #   if(data_list$Barnegat$ph[i] < pH_SENSOR_MIN) {
  #     data_list$Barnegat$GrossRange.test[i] <- 3
  #   }else{
  #     data_list$Barnegat$GrossRange.test[i]<-data_list$Barnegat$GrossRange.test[i]
  #   }
  #   }else{data_list$Barnegat$GrossRange.test[i]<-3}
  # }
  # 
  # for (i in e) {
  #   if(data_list$Barnegat$ph[i] > pH_SENSOR_MAX) {
  #     data_list$Barnegat$GrossRange.test[i] <- 3
  #   }else{
  #     data_list$Barnegat$GrossRange.test[i]<-data_list$Barnegat$GrossRange.test[i]
  #   }
  # }
  # 
  # for (i in e) {
  #   if(data_list$Barnegat$ph[i] > pH_SENSOR_MIN & data_list$Barnegat$ph[i] < pH_USER_MIN) {
  #     data_list$Barnegat$GrossRange.test[i] <- 2
  #   }else{
  #     data_list$Barnegat$GrossRange.test[i]<-data_list$Barnegat$GrossRange.test[i]
  #   }
  # }
  # 
  # for (i in e) {
  #   if(data_list$Barnegat$ph[i] > pH_USER_MAX & data_list$Barnegat$ph[i] < pH_SENSOR_MAX) {
  #     data_list$Barnegat$GrossRange.test[i] <- 2
  #   }else{
  #     data_list$Barnegat$GrossRange.test[i]<-data_list$Barnegat$GrossRange.test[i]
  #   }
  # }
  # 
  # for (i in e) {
  #   if(data_list$Barnegat$ph[i] > pH_USER_MIN & data_list$Barnegat$ph[i] < pH_USER_MAX) {
  #     data_list$Barnegat$GrossRange.test[i] <- 1
  #   }else{
  #     data_list$Barnegat$GrossRange.test[i]<-data_list$Barnegat$GrossRange.test[i]
  #   }
  # }
  
  
  
  ###############################################################################
  #Rate of Change test: flag adjacent data points >X standard deviations apart over set period of time
  ###############################################################################
  
  ROC_SUSPECT = TIM_DEV_PH * HOURLY_READS
  
  f = 1:(length(data_list$Barnegat$ph)-1)
  
  for (i in f){
    if(isTRUE(abs(data_list$Barnegat$ph[i]-data_list$Barnegat$ph[i+1]) > N_DEV_PH * (sd(data_list$Barnegat$ph[i:(i+ROC_SUSPECT-1)])))){
      data_list$Barnegat$RateChange.test[i] <- 2
    }else{
      data_list$Barnegat$RateChange.test[i] <- 1
    }}
  
  
  ###############################################################################
  ######## #Spike test: flag spikes between adjacent pH data points##############
  ###############################################################################
 
  j = 1:(length(data_list$Barnegat$ph)-1)
  k = 1:length(data_list$Barnegat$Spike.test)
  
  for (i in j) {
    if(isTRUE(abs(data_list$Barnegat$ph[i]-data_list$Barnegat$ph[i+1]) > THRSHLD_LOW)){
      data_list$Barnegat$Spike.test[i] <- 2
    }else{
      data_list$Barnegat$Spike.test[i] <- data_list$Barnegat$Spike.test[i]
    }
  }
  
  for (i in j) {
    if(isTRUE(abs(data_list$Barnegat$ph[i]-data_list$Barnegat$ph[i+1]) > THRSHLD_HIGH)){
      data_list$Barnegat$Spike.test[i] <- 3
    }else{
      data_list$Barnegat$Spike.test[i] <- data_list$Barnegat$Spike.test[i]
    }
  }
  
  for (i in k) {
    if(data_list$Barnegat$Spike.test[i] == 5){
      data_list$Barnegat$Spike.test[i] <- 1
    }else{
      data_list$Barnegat$Spike.test[i] <- data_list$Barnegat$Spike.test[i]
    }
  }
  
  
  ###############################################################################
  ####### #Attenuated Signal test: flag data points subject to biofouling########
  ###############################################################################
 
  ATTEN_SUS = TST_TIM_SUSPECT * HOURLY_READS
  ATTEN_FAIL = TST_TIM_FAIL * HOURLY_READS
  
  l = 1:(length(data_list$Barnegat$ph) - ATTEN_SUS)
  m = 1:(length(data_list$Barnegat$ph) - ATTEN_FAIL)
  n = 1:length(data_list$Barnegat$ph)
  
  
  for (i in l) {
    if((max(data_list$Barnegat$ph[i:(i+ATTEN_SUS-1)], na.rm=TRUE) - min(data_list$Barnegat$ph[i:(i+ATTEN_SUS-1)], na.rm=TRUE)) < MIN_VAR){
      data_list$Barnegat$AttenuatedSig.test[i:(i+ATTEN_SUS-1)] <- 2
    }else{
      data_list$Barnegat$AttenuatedSig.test[i:(i+ATTEN_SUS-1)] <- data_list$Barnegat$AttenuatedSig.test[i:(i+ATTEN_SUS-1)]
    }
  }
  
  
  for (i in m) {
    if((max(data_list$Barnegat$ph[i:(i+ATTEN_FAIL-1)], na.rm=TRUE) - min(data_list$Barnegat$ph[i:(i+ATTEN_FAIL-1)], na.rm=TRUE)) < MIN_VAR){
      data_list$Barnegat$AttenuatedSig.test[i:(i+ATTEN_FAIL-1)] <- 3
    }else{
      data_list$Barnegat$AttenuatedSig.test[i:(i+ATTEN_FAIL-1)] <- data_list$Barnegat$AttenuatedSig.test[i:(i+ATTEN_FAIL-1)]
    }
  }
  
  for (i in n) {
    if(data_list$Barnegat$AttenuatedSig.test[i] == 5){
      data_list$Barnegat$AttenuatedSig.test[i] <- 1
    }else{
      data_list$Barnegat$AttenuatedSig.test[i] <- data_list$Barnegat$AttenuatedSig.test[i]
    }
  }
  
