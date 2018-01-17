
rm(list = ls())   # Clear Workspace
library(chron)
library(PhysicalActivity)
library(dplyr)
library(activpalProcessing)
library(xlsx)
library(stringr)

AG.sed.analysis = function (posture, epoch = 1, n) # create descriptives of sedentary analysis
{
  sed <- posture == 1 # find all epochs that subject was sedentary
  lengths.of.continuous.bouts.of.sed <- apply(as.matrix(strsplit(paste(sed,
                                                                       collapse = ""), split = "FALSE", fixed = TRUE)[[1]]),
                                              1, function(x) {
                                                nchar(x)/4
                                              }) #find per-bout duration of consecutive sedentary epochs
  lengths <- lengths.of.continuous.bouts.of.sed > n * (60/epoch)
  inds.lengths <- (1:length(lengths.of.continuous.bouts.of.sed))[lengths.of.continuous.bouts.of.sed >
                                                                   n * (60/epoch)]
  inds.lengths = cbind(inds.lengths,lengths.of.continuous.bouts.of.sed[inds.lengths], lengths.of.continuous.bouts.of.sed[inds.lengths]/4)
  colnames(inds.lengths) = c('sed.bout.index','bout.length.epoch','bout.length.minutes')
  sed.bouts = length(inds.lengths[,1])
  sed.bout.total.time = sum(lengths.of.continuous.bouts.of.sed[lengths])/(60/epoch)
  sed.bout.avg = mean(inds.lengths[,3])
  sed.bout.max = max(inds.lengths[,3])
  sed.bout.min = min(inds.lengths[,3])

  sed.bout.analysis = data.frame(cbind(sed.bouts, sed.bout.total.time,sed.bout.avg,sed.bout.max, sed.bout.min))
  colnames(sed.bout.analysis) = c('Sed.Bouts','Sed.Bouts.Total.Time','Sed.Bout.Avg','Sed.Bout.Max','Sed.Bout.Min')

  return(sed.bout.analysis)
}
# Set working directory for all files, sort by Child/Parent
setwd('/Users/gpetrucci/Desktop/britt data')
Files = Sys.glob(dir())     # list all files in directory
wd = getwd()    # establish variable wd as the directory
SubFiles = Files[grepl(".dat",Files)] # subset files to those that only have the data table headers

##########
###### Need to wrap for loop starting here for batch procesing, predispose mastersheet for subject collapsing
###########

 FITT.AG.Mastersheet = matrix(data=NA,nrow=length(SubFiles),ncol=26)
for (sss in 1:length(SubFiles)){
  ID = strsplit(SubFiles[sss], split="_")
  ID = str_sub(ID[[1]][2], 1,3)
  

  R.Sample = readCountsData(paste(wd,SubFiles[sss],sep = '/'), ctPerSec = 1/15) # read in unfiltered DAT file


  # identify epochs that are wear/nonwear
WTdata = wearingMarking(R.Sample, TS = 'TimeStamp',frame = 90, streamFrame = 30, allowanceFrame = 2, perMinuteCts = 4, cts = 'counts', newcolname = 'wearing')

NWT.ref = which(WTdata$wearing =='nw')   # find row index that is marked non-wear
#create R-processed WT data and ActiLife WTdata that have same structure
R.WTdata = WTdata [-NWT.ref,]   # create new dataframe w/ only weartime
Days.WT = R.WTdata %>% group_by(days) %>% summarise(Freq = n())
index = which(Days.WT$Freq < (480*4)) #find the days that have less than the WT/day criteria of 480 minutes
Days.WT.valid = if(any(Days.WT$Freq < (480*4))) {Days.WT[-index,]} else {Days.WT}
R.WTdata.valid = R.WTdata[R.WTdata$days %in%  Days.WT.valid$days,] # remove all days that have insufficient weartime by the threshold used to create object index

if (is.na(R.WTdata.valid[1,1])){
  fill.na = matrix(data = NA, nrow = 1, ncol = 24)
    Summary.data = cbind(SubFiles[sss],ID,fill.na)
    colnames(Summary.data) = c('Filename','ID','Tot_Sed','Tot_Light','Tot_Mod','Tot_Vig','Tot_MVPA','Pct_Sed','Pct_Light','Pct_Mod','Pct_Vig','Pct_MVPA','Avg_Sed/Day','Avg_Light/Day','Avg_Mod/Day','Avg_Vig/Day','Avg_MVPA/Day',
                               '#Epochs','TotalTime','ValidDays','#SedBouts','Tot_Time_SedBouts','Avg_SedBout_Duration','Max_SedBout_Duration','Min_SedBout_Duration','Avg_SedBout_Duration/Day')
    3 = rbind(FITT.AG.Mastersheet,Summary.data)
    next
}

# identify for each epoch if sed or what intensity of PA
for (iii in 1:length(R.WTdata.valid[,1])) {
  if (R.WTdata.valid$counts[iii] <=25)
  {
    PAint = 1}
  else
  {if (R.WTdata.valid$counts[iii] >25 & R.WTdata.valid$counts[iii] <=573){
    PAint = 2}
    else
    {if (R.WTdata.valid$counts[iii] >573 & R.WTdata.valid$counts[iii] <=1002){
      PAint = 3}
      else
      {if (R.WTdata.valid$counts[iii] > 1002){
        PAint = 4}
      }
    }
  }
  R.WTdata.valid$intensity[iii] = PAint
}

# Calculate total time sedentary and in PA intensities, proportions
Total.time = R.WTdata.valid %>% group_by(intensity) %>% summarise(time = n()) #table(R.WTdata.valid$intensity)

Total.time$time = Total.time$time*15/60
Total.time$PctTime = Total.time$time/sum(Total.time$time)
insert = c(5,sum(Total.time[3:4,2]),sum(Total.time[3:4,2])/sum(Total.time$time))
Total.time = rbind(Total.time,insert)

# Calculate minutes of sed/activity per day
Total.time$AvgTime = (Total.time$time)/length(Days.WT.valid$days)

# Other Summary stats
NumEpochs = length(R.WTdata[,1])
Time = NumEpochs*15
ValidDays = length(Days.WT.valid$days)
# Sedentary Analysis
#sed.bout.analysis = AG.sed.analysis(R.WTdata.valid$intensity,epoch = 15,10)
#sed.bout.analysis$Sed.Bout.Dailyavg = sed.bout.analysis$Sed.Bouts.Total.Time/length(Days.WT.valid[,1])

# Organized data for entry into mastersheet
Summary.data = data.frame(cbind(SubFiles[sss],ID,t(Total.time$time),t(Total.time$PctTime),t(Total.time$AvgTime),NumEpochs,Time,ValidDays,sed.bout.analysis))
colnames(Summary.data) = c('ID','Tot_Sed','Tot_Light','Tot_Mod','Tot_Vig','Tot_MVPA','Pct_Sed','Pct_Light','Pct_Mod','Pct_Vig','Pct_MVPA','Avg_Sed/Day','Avg_Light/Day','Avg_Mod/Day','Avg_Vig/Day','Avg_MVPA/Day',
                           '#Epochs','TotalTime','ValidDays','#SedBouts','Tot_Time_SedBouts','Avg_SedBout_Duration','Max_SedBout_Duration','Min_SedBout_Duration','Avg_SedBout_Duration/Day')

if(!exists("Snapshot.AG.Mastersheet")){
  Snapshot.AG.Mastersheet = Summary.data
} else {Snapshot.AG.Mastersheet = rbind(Snapshot.AG.Mastersheet,Summary.data)
}

print(SubFiles[sss])
}

######
# Process post data
######
setwd('/Users/rmarcotte/Desktop/Research Projects/Snapshot/Post_All_DAT_15sec')
Files = Sys.glob(dir())     # list all files in directory
wd = getwd()    # establish variable wd as the directory
SubFiles = Files[grepl(".dat",Files)] # subset files to those that only have the data table headers
sss = 1
for (sss in 1:length(SubFiles)){
  ID = strsplit(SubFiles[sss],'_Post')[[1]][1]

  R.Sample = readCountsData(paste(wd,SubFiles[sss],sep = '/'), ctPerSec = 1/15) # read in unfiltered DAT file

  # identify epochs that are wear/nonwear
  WTdata = wearingMarking(R.Sample, TS = 'TimeStamp',frame = 90, streamFrame = 30, allowanceFrame = 2, perMinuteCts = 4, cts = 'counts', newcolname = 'wearing')

  NWT.ref = which(WTdata$wearing =='nw')   # find row index that is marked non-wear

  #create R-processed WT data and ActiLife WTdata that have same structure
  R.WTdata = WTdata[-NWT.ref,]   # create new dataframe w/ only weartime

  Days.WT = R.WTdata %>% group_by(days) %>% summarise(Freq = n())
  index = which(Days.WT$Freq < (480*4)) #find the days that have less than the WT/day criteria of 480 minutes

  Days.WT.valid = if(any(Days.WT$Freq < (480*4))) {Days.WT[-index,]} else {Days.WT}


  R.WTdata.valid = R.WTdata[R.WTdata$days %in%  Days.WT.valid$days,] # remove all days that have insufficient weartime by the threshold used to create object index

  if (is.na(R.WTdata.valid[1,1])){
    fill.na = matrix(data = NA, nrow = 1, ncol = 24)
    Summary.data = cbind(SubFiles[sss],ID,fill.na)
    colnames(Summary.data) = c('Filename','ID','Tot_Sed','Tot_Light','Tot_Mod','Tot_Vig','Tot_MVPA','Pct_Sed','Pct_Light','Pct_Mod','Pct_Vig','Pct_MVPA','Avg_Sed/Day','Avg_Light/Day','Avg_Mod/Day','Avg_Vig/Day','Avg_MVPA/Day',
                               '#Epochs','TotalTime','ValidDays','#SedBouts','Tot_Time_SedBouts','Avg_SedBout_Duration','Max_SedBout_Duration','Min_SedBout_Duration','Avg_SedBout_Duration/Day')
    Snapshot.AG.PostMastersheet = rbind(Snapshot.AG.PostMastersheet,Summary.data)
    next
  }

  # identify for each epoch if sed or what intensity of PA
  for (iii in 1:length(R.WTdata.valid[,1])) {
    if (R.WTdata.valid$counts[iii] <=25)
    {
      PAint = 1}
    else
    {if (R.WTdata.valid$counts[iii] >25 & R.WTdata.valid$counts[iii] <=573){
      PAint = 2}
      else
      {if (R.WTdata.valid$counts[iii] >573 & R.WTdata.valid$counts[iii] <=1002){
        PAint = 3}
        else
        {if (R.WTdata.valid$counts[iii] > 1002){
          PAint = 4}
        }
      }
    }
    R.WTdata.valid$intensity[iii] = PAint
  }

  # Calculate total time sedentary and in PA intensities, proportions
  Total.time = R.WTdata.valid %>% group_by(intensity) %>% summarise(time = n()) #table(R.WTdata.valid$intensity)

  Total.time$time = Total.time$time*15/60
  Total.time$PctTime = Total.time$time/sum(Total.time$time)
  insert = c(5,sum(Total.time[3:4,2]),sum(Total.time[3:4,2])/sum(Total.time$time))
  Total.time = rbind(Total.time,insert)

  # Calculate minutes of sed/activity per day
  Total.time$AvgTime = (Total.time$time)/length(Days.WT.valid$days)

  # Other Summary stats
  NumEpochs = length(R.WTdata[,1])
  Time = NumEpochs*15
  ValidDays = length(Days.WT.valid$days)

  # Sedentary Analysis
  sed.bout.analysis = AG.sed.analysis(R.WTdata.valid$intensity,epoch = 15,10)

  sed.bout.analysis$Sed.Bout.Dailyavg = sed.bout.analysis$Sed.Bouts.Total.Time/length(Days.WT.valid[,1])


  # Organized data for entry into mastersheet
  Summary.data = data.frame(cbind(SubFiles[sss],ID,t(Total.time$time),t(Total.time$PctTime),t(Total.time$AvgTime),NumEpochs,Time,ValidDays,sed.bout.analysis))
  colnames(Summary.data) = c('Filename','ID','Tot_Sed','Tot_Light','Tot_Mod','Tot_Vig','Tot_MVPA','Pct_Sed','Pct_Light','Pct_Mod','Pct_Vig','Pct_MVPA','Avg_Sed/Day','Avg_Light/Day','Avg_Mod/Day','Avg_Vig/Day','Avg_MVPA/Day',
                             '#Epochs','TotalTime','ValidDays','#SedBouts','Tot_Time_SedBouts','Avg_SedBout_Duration','Max_SedBout_Duration','Min_SedBout_Duration','Avg_SedBout_Duration/Day')

  if(!exists("Snapshot.AG.PostMastersheet")){
    Snapshot.AG.PostMastersheet = Summary.data
  } else {Snapshot.AG.PostMastersheet = rbind(Snapshot.AG.PostMastersheet,Summary.data)
  }

  print(SubFiles[sss])
}

Snapshot.AG.Mastersheet2 <- lapply(Snapshot.AG.Mastersheet, function(x)
  {if(is.character(x)) as.numeric(as.character(x))
  else x
  })
Snapshot.AG.Mastersheet2 = as.data.frame(Snapshot.AG.Mastersheet2)
colnames(Snapshot.AG.Mastersheet2) = c('Filename','ID','Sedentary_1','Light_1','Moderate_1','Vigorous_1','TotalMVPA_1','PctSED_1','PctLIG_1','PctMOD_1','PctVIG_1','PctMVPA_1','AveSedDay_1','AveLigDay_1','AveModDay_1','AveVigDay_1','AveMVPADay_1','NEpochs_1','Time_1','CalendarDays_1','TotalSedBouts_1','TotalTimeinSedBouts_1','AveLengthofSedBouts_1','MaxLengthofSedBouts_1','MinLengthofSedBouts_1','DailyAveofSedBouts_1')

Snapshot.AG.PostMastersheet2 <- lapply(Snapshot.AG.PostMastersheet, function(x)
{if(is.character(x)) as.numeric(as.character(x))
  else x
})
Snapshot.AG.PostMastersheet2 = as.data.frame(Snapshot.AG.PostMastersheet2)
colnames(Snapshot.AG.PostMastersheet2) = c('Filename','ID','Sedentary_2','Light_2','Moderate_2','Vigorous_2','TotalMVPA_2','PctSED_2','PctLIG_2','PctMOD_2','PctVIG_2','PctMVPA_2','AveSedDay_2','AveLigDay_2','AveModDay_2','AveVigDay_2','AveMVPADay_2','NEpochs_2','Time_2','CalendarDays_2','TotalSedBouts_2','TotalTimeinSedBouts_2','AveLengthofSedBouts_2','MaxLengthofSedBouts_2','MinLengthofSedBouts_2','DailyAveofSedBouts_2')

# Change proportions to percent values
Snapshot.AG.Mastersheet2[,8:12] = Snapshot.AG.Mastersheet2[,8:12]*100
Snapshot.AG.PostMastersheet2[,8:12] = Snapshot.AG.PostMastersheet2[,8:12]*100

# Change Time from seconds to minutes
Snapshot.AG.Mastersheet2$Time_1 = Snapshot.AG.Mastersheet2$Time_1/60
Snapshot.AG.PostMastersheet2$Time_2 = Snapshot.AG.PostMastersheet2$Time_2/60

## Clean Specific Cases----
Snapshot.AG.Mastersheet3[74,8:12] = Snapshot.AG.Mastersheet3[74,8:12]*100
Snapshot.AG.Mastersheet3[99,8:12] = Snapshot.AG.Mastersheet3[99,8:12]*100

Snapshot.AG.Mastersheet3$Time_1 = Snapshot.AG.Mastersheet3$NEpochs_1*15/60
Snapshot.AG.Mastersheet3$Time_1[99] = Snapshot.AG.Mastersheet3$NEpochs_1[99]*15/60

Snapshot.AG.PostMastersheet3[68,8:12] = Snapshot.AG.PostMastersheet3[68,8:12]*100
Snapshot.AG.PostMastersheet3$Time_1 = Snapshot.AG.PostMastersheet3$NEpochs_1*15/60

colnames(Snapshot.AG.Mastersheet3) = c('Filename','ID','Sedentary_1','Light_1','Moderate_1','Vigorous_1','TotalMVPA_1','PctSED_1','PctLIG_1','PctMOD_1','PctVIG_1','PctMVPA_1','AveSedDay_1','AveLigDay_1','AveModDay_1','AveVigDay_1','AveMVPADay_1','NEpochs_1','Time_1','CalendarDays_1','TotalSedBouts_1','TotalTimeinSedBouts_1','AveLengthofSedBouts_1','MaxLengthofSedBouts_1','MinLengthofSedBouts_1','DailyAveofSedBouts_1')
colnames(Snapshot.AG.PostMastersheet3) = c('Filename','ID','Sedentary_2','Light_2','Moderate_2','Vigorous_2','TotalMVPA_2','PctSED_2','PctLIG_2','PctMOD_2','PctVIG_2','PctMVPA_2','AveSedDay_2','AveLigDay_2','AveModDay_2','AveVigDay_2','AveMVPADay_2','NEpochs_2','Time_2','CalendarDays_2','TotalSedBouts_2','TotalTimeinSedBouts_2','AveLengthofSedBouts_2','MaxLengthofSedBouts_2','MinLengthofSedBouts_2','DailyAveofSedBouts_2')

# Export excel workbook with Pre and Post sheets together
#write.xlsx(Snapshot.AG.Mastersheet3, file="SnapshotPrePostSummary.xlsx", sheetName="Pre-Intervention", row.names=FALSE,showNA = F)
#write.xlsx(Snapshot.AG.PostMastersheet3, file="SnapshotPrePostSummary.xlsx", sheetName="Post-Intervention", append=TRUE, row.names=FALSE,showNA = F)


# ##### Below is playcode, above is the Weartime comparisons
# length(newWTdata[,1])/4
#
#
# Day.ref = which(WTdata$weekday == 'Saturday')   # find which observations are on Saturday
#
#
# # Add in counter for unique days for minutes of Sed, LPA, MPA, VPA
#
#
#
# CollapseData = dataCollapser(Sample,TS = 'date', by = 60, col = 'Axis1')
#
# Days = unique(Sample$Date)
# Date = as.Date(Days, format = '%m/%d/%Y')
#
# weekday.end.ref = is.weekend(Date)
#
#
#
#
# Files = list.files()
# Child.Files = list.files(pattern = 'Child')
# Parent.Files = list.files(pattern = 'Parent')
#
# #Import Aggregated Actigraph File and Cohort data
# AG.all = read.csv('All Baseline C1-3 15sec AGD 2-14-17.csv',header = TRUE,skip = 1)
# AG.all = subset(AG.all,select = -c(SerialNumber,Filename))
#
# C1 = AG.all[!rowSums(AG.all[1] >200),]
#
# C1.Child.Survey.Base = read.csv('ChildSurveyCohort1Baseline.csv',header = TRUE, skip = 0)
# C1.Child.Anthro.Base = read.csv('ChildAnthroCohort1Baseline.csv',header = TRUE, skip = 0)
# C1.Child.Puberty.Base = read.csv('ChildPubertyCohort1Baseline.csv',header = TRUE, skip = 0)
# C1.Parent.Survey.Base = read.csv('ParentSurveyCohort1Baseline.csv',header = TRUE, skip = 0)
#
# C2.Child.Survey.Base = read.csv('ChildSurveyCohort2Baseline.csv',header = TRUE, skip = 0)
# C2.Child.Anthro.Base = read.csv('ChildAnthroCohort2Baseline.csv',header = TRUE, skip = 0)
# C2.Child.Puberty.Base = read.csv('ChildPubertyCohort2Baseline.csv',header = TRUE, skip = 0)
# C2.Parent.Survey.Base = read.csv('ParentSurveyCohort2Baseline.csv',header = TRUE, skip = 0)
#
# # Remove trials for anthro variables and keep averages
# Anthro.keep = c("ID",'chtmean_1','cwtmean_1','cBMI_1','cbfmean_1','p2chsex_1','chagecalcyrs_1')
# C1.Child.Anthro.Base = C1.Child.Anthro.Base[,Anthro.keep]
# C2.Child.Anthro.Base = C2.Child.Anthro.Base[,Anthro.keep]
#
# #
#
# # Match ID in Quest to AG data
# match(C2.Child.Survey.Base$ID[6],AG.all$ID)
#
# # Principal Components Analysis
# C1 = lapply(C1,as.numeric)
#
# pr.out = prcomp(C1,scale = TRUE)
# pr.out$center
# pr.out$scale
#
# pr.out$rotation
# biplot(pr.out,scale = 0)
#
# pr.var = pr.out$sdev^2
#
# pve = pr.var/sum(pr.var)
#
# # K-means clustering analysis
# set.seed(2)
# km.out = kmeans(C1,2,nstart = 20)
# km.out$cluster
#
# par(mar=c(1,1,1,1))
# plot(C1,col = (km.out$cluster+1),main = "K-Means Clustering Results with K=2",pch = 20,cex = 2)
#
# # Used to test ActiLife output vs R-Processing WT data
# AG.SubFiles = Files[grepl('.csv',Files)]
# AG.WTdata = read.csv(paste(wd,AG.SubFiles[5],sep = '/'),skip = 1, header = T) # read in ActiLife filtered for WT csv file
# AG.WTdata$TimeStamp = as.character(with(AG.WTdata,as.POSIXct(paste(date,epoch),format = '%m/%d/%Y %H:%M:%S'))) # create TimeStamp that's similar to DAT file
# AG.WTdata = data.frame(AG.WTdata$TimeStamp,AG.WTdata$axis1)
# colnames(AG.WTdata) = c('Timestamp','axis1')
#
# R.WTdata = data.frame(R.WTdata$TimeStamp,R.WTdata$counts)
# colnames(R.WTdata) = c('Timestamp','axis1')
#
# for (iii in 1:length(AG.WTdata[,2])){
#   if (AG.WTdata$axis1[iii] != R.WTdata$axis1[iii]){
#
#     break
#   }
# }


### 11/13/17 Had to check on participant 401 for NA values in final table, confirmed that insufficient weartime on all days
#sss = which(str_detect(SubFiles,'401_Base'))
#verify_401 = R.WTdata %>% group_by(days) %>% summarise(wear_per_day_minutes = n()/4)
#write.table(verify_401, "verify_401.txt",sep = ',')
#write.table(verify_401, file="/Users/rmarcotte/Desktop/verify_401.csv",sep=",",row.names=F)

