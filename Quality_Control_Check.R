############# 
# 5-minute Quality Control Checks
# Last updated 1/15/18
############# 
#
# Has yet to be tested on actual files. Current test files can not be compared due to mismatch of times as explained below.
#
### Notes ###
# Naming should be as follows: 
#   Original files  = MOCA_Participant ID_Environment
#   Quality Control = MOCA_Participant ID_Environment_QC
#
# Looking at the outputs, it seems the times listed are relative to when observation coding was performed.
# Will need to re-export observations as described in Rob MOP, then try this code
#
# Things to add: 
#     None come to mind at the moment.... Stay tuned
#
############# 

rm(list = ls())

library(stringr)
library(dplyr)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)

QC.rater.reliability <- function(file.1,file.2)
{
  DO.data.1 <- file.1 #read File
  DO.data.1 <- DO.data.1[DO.data.1$Event_Type=="State start",] # remove all stop times since they're 0 duration
  secs <- round(DO.data.1$Duration_sf*100) # round duration of obs to nearest hundredth of a second (hence * 100)
  
  big.DO.1 <- data.frame(Time_Relative_sf = rep(DO.data.1$Time_Relative_sf,times = secs),
                         Duration_sf = rep(DO.data.1$Duration_sf,times = secs),
                         Behavior=rep(DO.data.1$Behavior,times=secs),
                         METs=rep(DO.data.1$Modifier_1,times=secs),
                         Modifier_2=rep(DO.data.1$Modifier_2,times=secs),
                         Modifier_3 = rep(DO.data.1$Modifier_3,times = secs),
                         Modifier_4 = rep(DO.data.1$Modifier_4,times = secs)) #creates DF that only has Beh, METs, Mod
  big.DO.1$Behavior <- as.character(big.DO.1$Behavior)  # transforms Beh and Mod to character strings
  big.DO.1$Modifier_2 <- as.character(big.DO.1$Modifier_2)
  big.DO.1$Modifier_3 <- as.character(big.DO.1$Modifier_3)
  big.DO.1$Modifier_4 <- as.character(big.DO.1$Modifier_4)
  #big.DO.1$Modifier_5 <- sapply(big.DO.1$Behavior, function(x) ifelse(any(str_detect(x,c('Lying','Sitting'))),'Sedentary','Active'))
  
  DO.data.2 <- file.2
  DO.data.2 <- DO.data.2[DO.data.2$Event_Type=="State start",]
  secs <- round(DO.data.2$Duration_sf*100)
  big.DO.2 <- data.frame(Time_Relative_sf = rep(DO.data.2$Time_Relative_sf,times = secs),
                         Duration_sf = rep(DO.data.2$Duration_sf,times = secs),
                         Behavior =rep(DO.data.2$Behavior,times=secs),
                         METs=rep(DO.data.2$Modifier_1,times=secs),
                         Modifier_2=rep(DO.data.2$Modifier_2,times=secs),
                         Modifier_3 = rep(DO.data.2$Modifier_3,times = secs),
                         Modifier_4 = rep(DO.data.2$Modifier_4,times = secs))
  big.DO.2$Behavior <- as.character(big.DO.2$Behavior)
  big.DO.2$Modifier_2 <- as.character(big.DO.2$Modifier_2)
  big.DO.2$Modifier_3 <- as.character(big.DO.2$Modifier_3)
  big.DO.2$Modifier_4 <- as.character(big.DO.2$Modifier_4)
  #big.DO.2$Modifier_5 <- sapply(big.DO.2$Behavior, function(x) ifelse(any(str_detect(x,c('Lying','Sitting'))),'Sedentary','Active'))
  
  n.1 <- dim(big.DO.1)[1] # examine the length of each data set
  n.2 <- dim(big.DO.2)[1]
  
  min.n <- min(n.1,n.2)   # take the data set with the shorter length
  big.DO.1 <- big.DO.1[1:min.n,]  # make both datasets same length
  big.DO.2 <- big.DO.2[1:min.n,]
  
  # Finds index of transition from first and last obs in QC, which should be file 2
  begin_index = min(which(big.DO.2$Time_Relative_sf > 0))
  end_index = max(which(big.DO.2$Time_Relative_sf < tail(big.DO.2$Time_Relative_sf,n=1)))
  
  # Trim files to include only same point in time
  big.DO.1 = big.DO.1[begin_index:end_index,]
  big.DO.2 = big.DO.2[begin_index:end_index,]
  
  n.1 <- dim(big.DO.1)[1] # examine the length of each data set
  n.2 <- dim(big.DO.2)[1]
  
  min.n <- min(n.1,n.2)   # take the data set with the shorter length
  
  big.DO.1$MET.level <- as.character(cut(big.DO.1$METs,breaks=c(0,1.5,3,6,Inf),right=F,labels=c("Sed","Light","Mod","Vig")))
  big.DO.2$MET.level <- as.character(cut(as.numeric(big.DO.2$METs),breaks=c(0,1.5,3,6,Inf),right=F,labels=c("Sed","Light","Mod","Vig")))
  
  perc.agree.Behavior <- mean(big.DO.1$Behavior==big.DO.2$Behavior) # mean of logical for comparison of character strings
  perc.agree.METs <- mean(big.DO.1$METs==big.DO.2$METs, na.rm=T) # mean of MET agreements
  perc.agree.Activities <- mean(big.DO.1$Modifier_2==big.DO.2$Modifier_2) # mean of logical for comp of char strings again
  perc.agree.Locomotion <- mean(big.DO.1$Modifier_3 == big.DO.2$Modifier_3)
  perc.agree.LimbMovement <- mean(big.DO.1$Modifier_4== big.DO.2$Modifier_4)
  perc.agree.MET.level <- mean(big.DO.1$MET.level== big.DO.2$MET.level, na.rm = T)
  
  METs.mean.diff <- mean(big.DO.1$METs - big.DO.2$METs, na.rm=T) # no difference since all MET values are same
  METs.sd.diff <- sd(big.DO.1$METs - big.DO.2$METs, na.rm=T) 
  METs.rmse <- sqrt(mean((big.DO.1$METs - big.DO.2$METs)^2, na.rm=T))
  
  ### All METs, Modifier 1
  MET.level.agreement.detail <- 
    ddply(data.frame(file=c(rep(1,min.n),rep(2,min.n)),MET.level=c(big.DO.1$MET.level,big.DO.2$MET.level)),
          .(MET.level,file),"nrow")
  names(MET.level.agreement.detail)[3] <- "seconds"
  MET.level.agreement.detail$seconds <- MET.level.agreement.detail$seconds/100
  
  ### All Behavior, Behavior
  Behavior.agreement.detail <- 
    ddply(data.frame(file=c(rep(1,min.n),rep(2,min.n)),Behavior=c(big.DO.1$Behavior,big.DO.2$Behavior)),
          .(Behavior,file),"nrow")
  names(Behavior.agreement.detail)[3] <- "seconds"
  Behavior.agreement.detail$seconds <- Behavior.agreement.detail$seconds/100
  
  ### All Activity, Modifier 2
  Activity.agreement.detail <- 
    ddply(data.frame(file=c(rep(1,min.n),rep(2,min.n)),Activity=c(big.DO.1$Modifier_2,big.DO.2$Modifier_2)),
          .(Activity,file),"nrow")
  names(Activity.agreement.detail)[3] <- "seconds"
  Activity.agreement.detail$seconds <- Activity.agreement.detail$seconds/100
  
  ### All Locomotion, Modifier 3
  Locomotion.agreement.detail <- 
    ddply(data.frame(file=c(rep(1,min.n),rep(2,min.n)),Activity=c(big.DO.1$Modifier_3,big.DO.2$Modifier_3)),
          .(Activity,file),"nrow")
  names(Locomotion.agreement.detail)[3] <- "seconds"
  Locomotion.agreement.detail$seconds <- Locomotion.agreement.detail$seconds/100
  
  ### All Limb movement, Modifier 4
  LimbMovement.agreement.detail <- 
    ddply(data.frame(file=c(rep(1,min.n),rep(2,min.n)),Activity=c(big.DO.1$Modifier_4,big.DO.2$Modifier_4)),
          .(Activity,file),"nrow")
  names(LimbMovement.agreement.detail)[3] <- "seconds"
  LimbMovement.agreement.detail$seconds <- LimbMovement.agreement.detail$seconds/100
  
  return(list(perc.agreements=data.frame(perc.agree.Behavior=perc.agree.Behavior,
                                         perc.agree.METs=perc.agree.METs,
                                         perc.agree.Activities=perc.agree.Activities,
                                         perc.agree.Locomotion=perc.agree.Locomotion,
                                         perc.agree.LimbMovement=perc.agree.LimbMovement,
                                         perc.agree.MET.level = perc.agree.MET.level),
              MET.stats=data.frame(METs.mean.diff=METs.mean.diff,METs.sd.diff=METs.sd.diff,METs.rmse=METs.rmse,
                                   mean.1=mean(big.DO.1$METs, na.rm=T),mean.2=mean(big.DO.2$METs, na.rm=T),
                                   sd.1=sd(big.DO.1$METs, na.rm=T),sd.2=sd(big.DO.2$METs, na.rm=T)),
              MET.level.agreement.detail=MET.level.agreement.detail,
              Behavior.agreement.detail=Behavior.agreement.detail,
              Activity.agreement.detail=Activity.agreement.detail,
              Locomotion.agreement.detail=Locomotion.agreement.detail,
              LimbMovement.agreement.detail=LimbMovement.agreement.detail))
  
}

setwd('/Users/rmarcotte/Desktop/MOCA/Quality Control/files')
wd = getwd()

# File selection
files = Sys.glob(dir())

file_pieces = str_split(files,' - ', simplify = T) # split to find participant identifier in 2nd column
participant_pieces = str_split(file_pieces[,2],'_', simplify = T) # split the participant identifier into MOCA, participant ID, Environment, and/or QC
participant = unique(participant_pieces[,2]) # find the number of participants for analysis

for(i in 1:length(participant)){
  participant_indices = str_detect(files, participant) # find files related to that participant
  
  participant_files = files[participant_indices] # only include those specific participants' files
  
  # Read in data, regardless of file type
  for(j in 1:length(participant_files)){
    if(str_detect(participant_files[j],'_QC')){     # Looks for the _ in case the off chance a Undergrad Researcher's initials are QC. Hence appending of QC at the end
      if(str_detect(participant_files[j],'.csv')){
        QC_data = read.csv(paste(wd, participant_files[j],sep = '/'),header = T)
      } else {
        QC_data = read_xlsx(paste(wd,participant_files[j],sep = '/'),col_names = T)
        QC_data$Modifier_1 = as.numeric(QC_data$Modifier_1)
      }
    } else {
      if(str_detect(participant_files[j],'.csv')){
        OG_data = read.csv(paste(wd, participant_files[j],sep = '/'),header = T)
      } else {
        OG_data = read_xlsx(paste(wd,participant_files[j],sep = '/'),col_names = T)
        OG_data$Modifier_1 = as.numeric(OG_data$Modifier_1)
        
      }
    }
  }
  
  if(exists('results')==0){
    results2 = QC.rater.reliability(OG_data, QC_data) # QC data always has to be second in the input
    results = data.frame(participant_files[1],participant_files[2],results2$perc.agreements,stringsAsFactors = F)
    } else {
    results2 = QC.rater.reliability(OG_data, QC_data)
    results2 = data.frame(participant_files[1],participant_files[2],results2$perc.agreements,stringsAsFactors = F)
    results = rbind(results,results2)
    }
  
}


