## 11/12/17
## Runs reliability analysis on Final Reliability files folder
## Uses MOCA files for criterion comparison, missing some criterion files for Home and School
## Verify if MOCA is dummy files or criterion

rm(list=ls())

# load a library
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

DO.rater.reliability <- function(file.1,file.2)
{
  DO.data.1 <- file.1 #read File
  DO.data.1 <- DO.data.1[DO.data.1$Event_Type=="State start",] # remove all stop times since they're 0 duration
  secs <- round(DO.data.1$Duration_sf*100) # round duration of obs to nearest hundredth of a second (hence * 100)

  big.DO.1 <- data.frame(Behavior=rep(DO.data.1$Behavior,times=secs),
                         METs=as.numeric(rep(DO.data.1$Modifier_1,times=secs)),
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
  big.DO.2 <- data.frame(Behavior =rep(DO.data.2$Behavior,times=secs),
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

  big.DO.1$MET.level <- as.character(cut(as.numeric(big.DO.1$METs),breaks=c(0,1.5,3,6,Inf),right=F,labels=c("Sed","Light","Mod","Vig")))
  big.DO.2$MET.level <- as.character(cut(as.numeric(big.DO.2$METs),breaks=c(0,1.5,3,6,Inf),right=F,labels=c("Sed","Light","Mod","Vig")))

  perc.agree.Behavior <- mean(big.DO.1$Behavior==big.DO.2$Behavior) # mean of logical for comparison of character strings
  perc.agree.METs <- mean(big.DO.1$METs==big.DO.2$METs, na.rm=T) # mean of MET agreements
  perc.agree.Activities <- mean(big.DO.1$Modifier_2==big.DO.2$Modifier_2) # mean of logical for comp of char strings again
  perc.agree.Locomotion <- mean(big.DO.1$Modifier_3 == big.DO.2$Modifier_3)
  perc.agree.LimbMovement <- mean(big.DO.1$Modifier_4== big.DO.2$Modifier_4)
  perc.agree.MET.level <- mean(big.DO.1$MET.level== big.DO.2$MET.level, na.rm = T)

  METs.mean.diff <- mean(as.numeric(big.DO.1$METs) - as.numeric(big.DO.2$METs), na.rm=T) # no difference since all MET values are same
  METs.sd.diff <- sd(as.numeric(big.DO.1$METs) - as.numeric(big.DO.2$METs), na.rm=T)
  METs.rmse <- sqrt(mean((as.numeric(big.DO.1$METs) - as.numeric(big.DO.2$METs))^2, na.rm=T))

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

#this is where the files are, needs to be chnaged for different systems if box sync isn't on computer
setwd("~/Box/Sirard Lab/MOCA - R01/Coding/Pending Certification")

files<- Sys.glob(dir())

wd = getwd() # establish variable wd as the directory


## This section handles CSV files ----
files_csv = files[grepl(".csv",files)]
files_csv = data.frame(as.character(files_csv), stringsAsFactors = F) # create data frame of all files in directory

splitr = function(x){strsplit(x,'CV')}
files2 = apply(files_csv, 1,splitr)

files2 = t(data.frame(files2))

rownames(files2) = 1:nrow(files2)
colnames(files2) = c('Coder','Environment')

file_sub = data.frame(files2[,2])
splitr2 = function(x){strsplit(x,'[.]')}
files3 = apply(file_sub, 1, splitr2)
files3 = t(data.frame(files3))

files2[,2] = files3[,1]

files2 = data.frame(files2)
files2$Coder = as.character(files2$Coder)
files2$Environment = as.character(files2$Environment)

### Handle csv names that are abnormal, is this necessary since vids are DOVE videos?----



### This section handles .xlsx file extensions ----
# xlsx files are messy in file names, regroup with Lanna for reminding students on proper naming schematic.....
files_xlsx = files[grepl('.xlsx',files)]
files_xlsx = data.frame(as.character(files_xlsx), stringsAsFactors = F) # create data frame of all files in directory

splitr = function(x){strsplit(x,' - ')}

files2_xlsx = apply(files_xlsx, 1,splitr)

files2_xlsx = t(data.frame(files2_xlsx))
files2_xlsx = files2_xlsx[,2]

files2_xlsx = data.frame(files2_xlsx, stringsAsFactors = F)

splitr2 = function(x){str_replace_all(x,'_','')}

files2_xlsx = apply(files2_xlsx,1,splitr2)
files2_xlsx = data.frame(files2_xlsx,stringsAsFactors = F)

rownames(files2_xlsx) = 1:nrow(files2_xlsx)

splitr3 = function(x){strsplit(x,'CV')}
files3_xlsx = apply(files2_xlsx, 1, splitr3)
files3_xlsx = t(data.frame(files3_xlsx,stringsAsFactors = F))

rownames(files3_xlsx) = 1:nrow(files3_xlsx)
colnames(files3_xlsx) = c('Coder','Environment')

splitr2 = function(x){strsplit(x,'[.]')}
files3 = apply(file_sub, 1, splitr2)
files3 = t(data.frame(files3))

files2[,2] = files3[,1]

files2 = data.frame(files2)
files2$Coder = as.character(files2$Coder)
files2$Environment = as.character(files2$Environment)

# Can use read_xlsx from readxl package to read in noldus output, consider exporting once read in as .csv to "master" DO coding cert destination
# data = read_xlsx('/Users/rmarcotte/Box/Sirard Lab/MOCA - R01/Coding/Coding Certification - Fall 2017/Audrey_Malloy_18-20 - AMCVC2 - Event Logs.xlsx',col_names =T)



## Begin batch processing to get reliability data frame----
for(i in 5:nrow(files3_xlsx)) {

  file.1 <- read_xlsx(paste(wd, "/", files_xlsx[i,], sep=""), col_names =T)
  file.2<- read.csv(paste(wd, "/","MOCACV", files3_xlsx[i,2],".csv", sep=""), header=T)
  temp.results <- DO.rater.reliability(file.1,file.2)
  ifelse(!exists('real.results'),
         real.results<- data.frame(files3_xlsx[i,1],files3_xlsx[i,2],temp.results$perc.agreements, stringsAsFactors = F),
         real.results[i,] <- data.frame(files3_xlsx[i,1],files3_xlsx[i,2],temp.results$perc.agreements,stringsAsFactors = F))

}

real.results.2<- gather(real.results, key="modifier", value="Per.Agree", perc.agree.Behavior:perc.agree.MET.level)
colnames(real.results.2) <- c("coder", "environment", "modifier", "agreement")
real.results.2$modifier<- gsub("perc.agree.", "", real.results.2$modifier)
real.results.3 <- filter(real.results.2, coder!="MOCA" )


greg<- filter(real.results.3, coder=="RM")

# Identify coder, video, and modifier that did not reach > 80% agreement
bad_coders <- real.results.3 %>%
  group_by(coder, environment,modifier, agreement) %>%
  tally(agreement < .795) %>% filter(n > 0)
bad_coders_nolimb = filter(bad_coders, modifier != 'LimbMovement')



