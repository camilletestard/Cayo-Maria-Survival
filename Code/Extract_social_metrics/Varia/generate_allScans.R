#generate_allScans: generates scan data from all group and years
#Input: proximity data from "normal" data collection protocol years (including 2019) & scan data from 2018 (hurricane year)
#Output: Combination of all scan data in "allScans.txt" file. This will be the input to many other scripts
# Camille Testard - 2021

#load required libraries
library(dplyr)
library(lubridate)
library(hms)
library(varhandle)
library(stringr)
library(schoolmath)

######################################################
#Data from separate files
######################################################

#Load proximity scans from groups and years of interest in the focal format: 
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles") 

group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","S","V","F","V","TT","V","F")
years = c(2013,2013, 2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2019, 2019,2021,2021,2022,2022,2022)
groupyears = c("F2013","KK2013","F2014","HH2014","F2015","V2015","R2015","KK2015",
               "R2016","V2016","F2016","HH2016","F2017","KK2017","V2017",
               "S2019","V2019","F2021","V2021","TT2022","V2022","F2022")

allScans2 = data.frame(); count = 0; total_count=0; gy =1
for (gy in 1:length(groupyears)){ #for all group & years
  
  prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = "")) #load prox data from groupyear gy
  names(prox_data)[7]="partners.activity"
  
  #Add group info
  prox_data$group =group[gy]
  
  #Format time and add year + quarter info
  prox_data$time <- parse_date_time(as.character(prox_data$time), order="ymdHMS")
  prox_data$time <- strftime(prox_data$time, format="%H:%M:%S",tz="GMT")
  prox_data$time <- as_hms(prox_data$time)
  prox_data$year <- lubridate::year(prox_data$date)
  prox_data$Q    <- lubridate::quarter(prox_data$date)
  prox_data$date <- as.character(prox_data$date) #re-format to character after finding year and quarter
  
  #Add hurricane info
  prox_data$isPost = ifelse(years[gy] < 2018, 0, 1)
  
  #Add timeBlock info
  prox_data$timeBlock = NA
  prox_data$timeBlock[which(prox_data$time <= as_hms("11:00:00"))] = "AM";
  prox_data$timeBlock[which(prox_data$time > as_hms("11:00:00"))] = "PM";
  
  #Format activity
  prox_data$focal.activity = as.character(prox_data$focal.activity)
  prox_data$focal.activity[which(prox_data$focal.activity=="feedplant"|prox_data$focal.activity=="feedchow")]="feed"
  prox_data$focal.activity[which(prox_data$focal.activity=="feedwater")]="drink"
  
  #To be consistent with post-hurricane data add focal activity and partner ID columns
  prox_data$focal.activity.isPost=NA
  prox_data$partner.ID=NA
  
  #Format name if needed
  unique(prox_data$focal.monkey)#check focal names
  prox_data$focal.monkey=sub("'E","E",as.character(prox_data$focal.monkey)) #Replace 'XEX by XEX names if needed
  
  #Clean up: rename and delete unused columns
  names(prox_data)[4] = "focalID"
  
  #Exclude focal from in.proximity, count number of prox partners
  partners = as.data.frame(str_split_fixed(prox_data$in.proximity,",",2)) #split prox ID info at each ","
  prox_data$in.proximity = partners[,2] #don't consider the first column (= focal ID)
  prox_data[prox_data==""]=NA; #if empty, transform into NA
  prox_data[] = lapply(prox_data,str_trim) ##remove blanks from all entries
  prox_data$num.prox = str_count(as.character(prox_data$in.proximity),",")+1#Count the number of proximity partners
  prox_data$num.prox[is.na(prox_data$num.prox)]=0 # if na count as 0
  
  #Add social information
  prox_data$isProx=0; prox_data$isProx[which(prox_data$num.prox!=0)]=1
  
  if (years[gy]<2021){
    
    prox_data$isSocial=0; prox_data$isSocial[which(prox_data$focal.activity=="social")]=1
    
    #Add grooming partner.ID and direction information from focal data
    focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = ""), sep=",")
    scans_grooming = which(prox_data$isSocial==1)
    
    ii=1; total_count= total_count+length(scans_grooming)
    for (ii in 1:length(scans_grooming)){ #for all scans in grooming state
      obs_name = prox_data$observation.name[scans_grooming[ii]] #find the obervation name for that scan
      scan_num = as.numeric(prox_data$scan.number[scans_grooming[ii]]) #find the scan number within that observation
      idx_obs_name = which(!is.na(match(focal_data$observation_name, obs_name))) #find the observation in the focal data
      
      groomBehav = which(focal_data$behaviour[idx_obs_name] == "GroomGET" | 
                           focal_data$behaviour[idx_obs_name] == "GroomGIVE") #find the grooming interaction in the focal data
      focal_data[idx_obs_name,]
      #IMPORTANT NOTE: for a certain number of scans we do not know the direction of grooming. 
      #This is problematic because these observations are then excluded from the networks
      
      if (length(groomBehav)>1 & scan_num!=1){count = count +1} #check the number of times individual was grooming with different partners during a focal 
      #when the scan is not 1. Because if the scan in 1 then there is no uncertainty, the first instance of grooming should be used for partnerID and direction
      #Note: There is uncertainty only 9/2987 scans with grooming. 
      
      #Add partner info. 
      if(length(groomBehav)>=scan_num) #if the number of grooming events is larger than or equal to the scan number
        #i.e. if thre are two grooming events in one focal, and the scan number is "2", than consider the second grooming event as the appropriate one
      {prox_data$partner.ID[scans_grooming[ii]] = as.character(focal_data$partner_id[idx_obs_name[groomBehav[scan_num]]])
      prox_data$focal.activity.isPost[scans_grooming[ii]] = as.character(focal_data$behaviour[idx_obs_name[groomBehav[scan_num]]])}#Add direction of grooming}
      else #if the number of grooming events is less than the scan number, simply consider the first grooming event. In most cases 
        #this will happen when there is one grooming event ony happening at the second scan of this focal.
      {prox_data$partner.ID[scans_grooming[ii]] = as.character(focal_data$partner_id[idx_obs_name[groomBehav[1]]]);
      prox_data$focal.activity.isPost[scans_grooming[ii]] = as.character(focal_data$behaviour[idx_obs_name[groomBehav[1]]])}#Add direction of grooming
      
    }
    prox_data$focal.activity.isPost[which(prox_data$focal.activity.isPost=="GroomGIVE")]="G"#if groomGive, then code behavior as in post-hurricane data "G"
    prox_data$focal.activity.isPost[which(prox_data$focal.activity.isPost=="GroomGET")]="E" #if groomGet, then code behavior as in post-hurricane data "E"
    prox_data$isSocialGive = 0; prox_data$isSocialGive[which(prox_data$focal.activity.isPost=="G")]=1
    prox_data$isSocialGet = 0; prox_data$isSocialGet[which(prox_data$focal.activity.isPost=="E")]=1
    
    prox_data[,c("time","partners.activity")] = NULL;
    
    #add unq scan id to match up 2018 data
    prox_data$unq.scan.id = NA
    
  }else{
    
    prox_data$isSocial=ifelse(prox_data$focal.activity=="Groom Give"|prox_data$focal.activity=="Groom Get",1,0)
    #Add grooming partner.ID and direction information from focal data
    focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = ""), sep=",")
    scans_grooming = which(prox_data$isSocial==1)
    
    ii=1; total_count= total_count+length(scans_grooming)
    for (ii in 1:length(scans_grooming)){ #for all scans in grooming state
      obs_name = prox_data$observation.name[scans_grooming[ii]] #find the obervation name for that scan
      scan_num = as.numeric(prox_data$scan.number[scans_grooming[ii]]) #find the scan number within that observation
      idx_obs_name = which(!is.na(match(focal_data$observation_name, obs_name))) #find the obervation in the focal data
      
      groomBehav = which(focal_data$behaviour[idx_obs_name] == "Groom Start") #find the grooming interaction in the focal data
      t<-focal_data[idx_obs_name,]
      
      if (length(groomBehav)>1 & scan_num!=1){count = count +1} #check the number of times individual was grooming with different partners during a focal 
      #when the scan is not 1. Because if the scan in 1 then there is no uncertainty, the first instance of grooming should be used for partnerID and direction
      #Note: There is uncertainty only 9/2987 scans with grooming. 
      
      #Add partner info. 
      if(length(groomBehav)>=scan_num) #if the number of grooming events is larger than or equal to the scan number
        #i.e. if thre are two grooming events in one focal, and the scan number is "2", than consider the second grooming event as the appropriate one
      {prox_data$partner.ID[scans_grooming[ii]] = as.character(focal_data$partner_id[idx_obs_name[groomBehav[scan_num]]])
      prox_data$focal.activity.isPost[scans_grooming[ii]] = as.character(focal_data$behaviour[idx_obs_name[groomBehav[scan_num]]])}#Add direction of grooming}
      else #if the number of grooming events is less than the scan number, simply consider the first grooming event. In most cases 
        #this will happen when there is one grooming event ony happening at the second scan of this focal.
      {prox_data$partner.ID[scans_grooming[ii]] = as.character(focal_data$partner_id[idx_obs_name[groomBehav[1]]]);
      prox_data$focal.activity.isPost[scans_grooming[ii]] = as.character(focal_data$direction[idx_obs_name[groomBehav[1]]])}#Add direction of grooming
      
    }
    prox_data$focal.activity.isPost[which(prox_data$focal.activity.isPost=="give")]="G"#if groomGive, then code behavior as in post-hurricane data "G"
    prox_data$focal.activity.isPost[which(prox_data$focal.activity.isPost=="receive")]="E" #if groomGet, then code behavior as in post-hurricane data "E"
    prox_data$isSocialGive = ifelse(prox_data$focal.activity.isPost=="G",1,0); 
    prox_data$isSocialGet = ifelse(prox_data$focal.activity.isPost=="E",1,0);
    
    prox_data[,c("time","partners.activity")] = NULL;
    
    #add unq scan id to match up 2018 data
    prox_data$unq.scan.id = NA
    
  }

#Order columns
col_order <- c("date","observation.name","focalID","group","year","scan.number","focal.activity","focal.activity.isPost","partner.ID","in.proximity","num.prox","isProx","isSocial","isSocialGive", "isSocialGet", "Q","isPost","timeBlock")
prox_data <- prox_data[, col_order]

allScans2= rbind(allScans2, prox_data)

print(groupyears[gy])
}
percentNoPartnerID = length(which(is.na(allScans2$partner.ID)))/length(which(allScans2$isSocial ==1))
percentNoDirection = length(which(allScans2$isSocialGive==0 & allScans2$isSocialGet==0 & allScans2$isSocial==1))/length(which(allScans2$isSocial ==1))
numUncertainCases = count/total_count

#

######################################################
#Data structured differently (2018)
######################################################

#Load scans from 2018 (weird format, only scans)
allScans3=data.frame()
V2018 = read.csv("GroupV2018_scansamples_FULL_CLEANED.csv"); 
V2018[,"partner.activity"]=NULL #Because we don't have partner activity in post-hurricane KK, butwe need both matrices to match
KK2018 = read.csv("GroupKK2018_scansamples_FULL_CLEANED.csv"); names(KK2018)[1]="date"
allScans3=rbind(V2018,KK2018)

allScans3$date <- lubridate::mdy(as.character(allScans3$date))
allScans3$year <- lubridate::year(allScans3$date)
allScans3$Q    <- lubridate::quarter(allScans3$date)
allScans3$date <- as.character(allScans3$date) #re-format to character after finding year and quarter

#Add unique scan identifier
allScans3$observation.name = as.factor(paste(allScans3$date, allScans3$scan.num,sep="."))

#Add hurricane info
allScans3$isPost = 1

#Format time and create timeBlock column
#IMPORTANT: MAKE SURE TIME COLUMNS ARE FORMATTED IN EXCEL IN FORMAT "13:55:00"
allScans3$start.time = as_hms(as.character(allScans3$start.time))
allScans3$stop.time = as_hms(as.character(allScans3$stop.time))

allScans3$timeBlock = NA
allScans3$timeBlock[which(allScans3$start.time <= as_hms("11:00:00"))] = "AM";
allScans3$timeBlock[which(allScans3$start.time > as_hms("11:00:00"))] = "PM";

#Format XEX names
unique(allScans3$subject.ID) #check spelling of subject id
allScans3$subject.ID=sub("'","",as.character(allScans3$subject.ID)) #Replace 'XEX byXEX names if needed
allScans3$subject.ID=str_trim(allScans3$subject.ID,side="both") #Remove blanks
unique(allScans3$prox.adult.IDs)
allScans3$prox.adult.IDs=sub("'","",as.character(allScans3$prox.adult.IDs))

#Clean up: rename and delete unused columns
allScans3[,c("stop.time","observer.initials","cayo.map.code","nearest.adult.neighbour.ID","distance.nearest.neighbour","start.time")]=NULL
names(allScans3)[3]="scan.number"; names(allScans3)[4]="focalID"; names(allScans3)[5]="focal.activity"; names(allScans3)[7]="in.proximity"

#create column with equivalent activity to pre-hurricane
allScans3[] = lapply(allScans3,str_trim)
allScans3$focal.activity.isPost = as.character(allScans3$focal.activity) #preserve post-hurricane activity code
#re-code activity to pre-hurricane for comparison
allScans3$focal.activity = as.character(allScans3$focal.activity)
allScans3$focal.activity[unique(c(which(allScans3$focal.activity=='G'),which(allScans3$focal.activity=='E'),which(allScans3$focal.activity=='E,P'),which(allScans3$focal.activity=='G,E')))]="social"
allScans3$focal.activity[unique(c(which(allScans3$focal.activity=='R'),which(allScans3$focal.activity=='P')))]="rest"
allScans3$focal.activity[unique(c(which(allScans3$focal.activity=='AG'),which(allScans3$focal.activity=='AR')))]="aggression"
allScans3$focal.activity[unique(c(which(allScans3$focal.activity=='SR'),which(allScans3$focal.activity=='SG')))]="submit"
allScans3$focal.activity[grep('T',allScans3$focal.activity)]="travel"
allScans3$focal.activity[grep('F',allScans3$focal.activity)]="feed"
allScans3$focal.activity[grep('D',allScans3$focal.activity)]="drink"
allScans3$focal.activity[grep('SD',allScans3$focal.activity)]="sdb"
allScans3$focal.activity[grep('N/A',allScans3$focal.activity)]="UNK"
#unique(allScans3$focal.activity) #Check correct activity categories

#Format in.proximity, count number of prox partners
allScans3$partner.ID = as.character(allScans3$partner.ID); allScans3$partner.ID[which(allScans3$partner.ID=="N/A")]=NA
allScans3$in.proximity = as.character(allScans3$in.proximity); allScans3$in.proximity[which(allScans3$in.proximity=="N/A")]=NA
allScans3$num.prox = str_count(as.character(allScans3$in.proximity),",")+1
allScans3$num.prox[is.na(allScans3$num.prox)]=0

#Add social information
allScans3$isProx=1; allScans3$isProx[which(allScans3$num.prox==0)]=0
allScans3$isSocial=0; allScans3$isSocial[which(allScans3$focal.activity=="social")]=1
allScans3$isSocialGive = 0; allScans3$isSocialGive[which(allScans3$focal.activity.isPost=="G")]=1
allScans3$isSocialGet = 0; allScans3$isSocialGet[which(allScans3$focal.activity.isPost=="E")]=1

#Order columns
col_order <- c("date","observation.name","focalID","group","year","scan.number","focal.activity","focal.activity.isPost","partner.ID","in.proximity","num.prox","isProx","isSocial","isSocialGive", "isSocialGet", "Q","isPost","timeBlock")
allScans3 <- allScans3[, col_order]

######################################################
#COMBINE PRE-/POST-HURRICANE DATA
######################################################
allScans=rbind(allScans2,allScans3)

#Find all unique IDs
a = str_split(allScans$in.proximity, c(","), simplify = TRUE)
proxIDs = str_trim(c(a[,1],a[,2],a[,3],a[,4],a[,5],a[,6],a[,7],a[,8],a[,9]))
allIDs = unique(c(as.character(allScans$focalID),proxIDs))
allIDs[which(nchar(allIDs)!=3)]
#Make appropriate ID corrections
allScans[which(allScans == "5.00E+06",arr.ind = TRUE)] = "5E6"
allScans[which(allScans == "2.00E+04",arr.ind = TRUE)] = "2E4"
allScans[which(allScans == "4.00E+02",arr.ind = TRUE)] = "4E2"
allScans[which(allScans == "9.00E+03",arr.ind = TRUE)] = "9E3"
allScans[which(allScans == "6.00E+06",arr.ind = TRUE)] = "6E6"
allScans[which(allScans == "1.00E+05",arr.ind = TRUE)] = "1E5"
allScans[which(allScans == "1.00E+02",arr.ind = TRUE)] = "1E2"
allScans[which(allScans == "3.00E+04",arr.ind = TRUE)] = "3E4"

# #Check again
# a = str_split(allScans$in.proximity, c(","), simplify = TRUE)
# proxIDs = str_trim(c(a[,1],a[,2],a[,3],a[,4],a[,5],a[,6],a[,7],a[,8],a[,9]))
# allIDs = unique(c(as.character(allScans$focalID),proxIDs))
# allIDs[which(nchar(allIDs)!=3)]

#################################
## Check for how many samples i do not have direction or partner ID
#################################
percentNoPartnerID_all = length(which(is.na(allScans$partner.ID)))/length(which(allScans$isSocial ==1))
percentNoDirection_all = length(which(allScans$isSocialGive==0 & allScans$isSocialGet==0 & allScans$isSocial==1))/length(which(allScans$isSocial ==1))

#To make sure all observations are included, add directionality (random assignment)
#Note that this will partially invalidate directional measures of networks but not overall rates
missing_direction = which(allScans$isSocialGive==0 & allScans$isSocialGet==0 & allScans$isSocial==1)
for (i in 1:length(missing_direction)){
  
  rand_dir = sample(1:2,1)
  
  if (rand_dir==1){
    allScans$focal.activity.isPost[missing_direction[i]]="G"
    allScans$isSocialGive[missing_direction[i]]=1
  } else{
    allScans$focal.activity.isPost[missing_direction[i]]="E"
    allScans$isSocialGet[missing_direction[i]]=1
  }
  
}

######################################################
#SAVE ALLSCANS.TXT
######################################################

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles") 
write.csv(allScans,"allScans.txt", row.names = F)

#as.data.frame(table(allScans$samplingCateg))