#Get_aggression_data.R
#This script aggregates the aggression data from all groups and years from 2015 to 2021
#from the cayo database. The output of this script will be used to generate 
# networks with bison.
#C Testard August 2022

library(stringr)
library(lubridate)
library(hms)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

#Load functions
setwd("~/Documents/GitHub/Cayo-Maria-Survival/")
source("Code/Functions/functions_GlobalNetworkMetrics.R")

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","S","V","F","V","TT","V","F")
years = c(2013, 2013, 2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2019, 2019,2021,2021,2022,2022,2022)
groupyears = paste0(group,years)

gy=9#16
edgelist.all = data.frame()
savePath = '~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/'

for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  
  if (years[gy]==2018) {
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Disease-Modeling/Data/Data All Cleaned/BehavioralDataFiles')
    scans2018= read.csv(paste("Group",groupyears[gy],"_scansamples_FULL_CLEANED.txt", sep = ""))
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
    
    #Initialize SocialCaiptalData
    SocialCapitalData= meta_data[,c("id","sex","age","ordinal.rank","percofsex.dominanted","hrs.focalfollowed","focalcutoff_met")]
    SocialCapitalData$group = group[gy]
    SocialCapitalData$year = years[gy]
    
    ##Format data##
    scans2018$date <- lubridate::mdy(as.character(scans2018$date))
    scans2018$year <- lubridate::year(scans2018$date)
    scans2018$Q    <- lubridate::quarter(scans2018$date)
    scans2018$date <- as.character(scans2018$date) #re-format to character after finding year and quarter
    
    #Add unique scan identifier
    scans2018$observation.name = as.factor(paste(scans2018$date, scans2018$scan.num,sep="."))
    
    #Add hurricane info
    scans2018$isPost = 1
    
    #Format time and create timeBlock column
    #IMPORTANT: MAKE SURE TIME COLUMNS ARE FORMATTED IN EXCEL IN FORMAT "13:55:00"
    scans2018$start.time = as_hms(as.character(scans2018$start.time))
    scans2018$stop.time = as_hms(as.character(scans2018$stop.time))
    
    scans2018$timeBlock = NA
    scans2018$timeBlock[which(scans2018$start.time <= as_hms("11:00:00"))] = "AM";
    scans2018$timeBlock[which(scans2018$start.time > as_hms("11:00:00"))] = "PM";
    
    #Format XEX names
    unique(scans2018$subject.ID) #check spelling of subject id
    scans2018$subject.ID=sub("'","",as.character(scans2018$subject.ID)) #Replace 'XEX byXEX names if needed
    scans2018$subject.ID=str_trim(scans2018$subject.ID,side="both") #Remove blanks
    unique(scans2018$prox.adult.IDs)
    scans2018$prox.adult.IDs=sub("'","",as.character(scans2018$prox.adult.IDs))
    
    #Clean up: rename and delete unused columns
    scans2018[,c("stop.time","observer.initials","cayo.map.code","nearest.adult.neighbour.ID","distance.nearest.neighbour","start.time")]=NULL
    names(scans2018)[3]="scan.number"; names(scans2018)[4]="focalID"; names(scans2018)[5]="focal.activity"; names(scans2018)[7]="in.proximity"
    
    #create column with equivalent activity to pre-hurricane
    scans2018[] = lapply(scans2018,str_trim)
    scans2018$focal.activity.isPost = as.character(scans2018$focal.activity) #preserve post-hurricane activity code
    #re-code activity to pre-hurricane for comparison
    scans2018$focal.activity = as.character(scans2018$focal.activity)
    scans2018$focal.activity.recoded = scans2018$focal.activity
    scans2018$focal.activity.recoded[unique(c(which(scans2018$focal.activity=='G'),which(scans2018$focal.activity=='E'),which(scans2018$focal.activity=='E,P'),which(scans2018$focal.activity=='G,E')))]="social"
    scans2018$focal.activity.recoded[unique(c(which(scans2018$focal.activity=='R'),which(scans2018$focal.activity=='P')))]="rest"
    scans2018$focal.activity.recoded[unique(c(which(scans2018$focal.activity=='AG'),which(scans2018$focal.activity=='AR')))]="aggression"
    scans2018$focal.activity.recoded[unique(c(which(scans2018$focal.activity=='SR'),which(scans2018$focal.activity=='SG')))]="submit"
    scans2018$focal.activity.recoded[grep('T',scans2018$focal.activity)]="travel"
    scans2018$focal.activity.recoded[grep('F',scans2018$focal.activity)]="feed"
    scans2018$focal.activity.recoded[grep('D',scans2018$focal.activity)]="drink"
    scans2018$focal.activity.recoded[grep('SD',scans2018$focal.activity)]="sdb"
    scans2018$focal.activity.recoded[grep('N/A',scans2018$focal.activity)]="UNK"
    #unique(scans2018$focal.activity) #Check correct activity categories
  
    #Add aggression information
    scans2018$isAgg=0; scans2018$isAgg[which(scans2018$focal.activity.recoded=="aggression" | scans2018$focal.activity.recoded=="submit")]=1
    agg_data = scans2018[scans2018$isAgg==1,c("focalID","partner.ID",'focal.activity','date')]
    
    agg_data$agonism_winner = ifelse( agg_data$focal.activity=="AG"|agg_data$focal.activity=="SR", agg_data$focalID, agg_data$partner.ID)
    agg_data$agonism_loser = ifelse( agg_data$focal.activity=="AR"|agg_data$focal.activity=="SG", agg_data$focalID, agg_data$partner.ID)
    agg_data$agonism_type = ifelse( agg_data$focal.activity=="AG"|agg_data$focal.activity=="AR", "aggression", "submition")
    agg_data$data_source="scan"
    agg_data$observer=ifelse(group[gy]=="V","Daniel","Josue")
    agg_data$focal_individual=ifelse(agg_data$focalID == agg_data$agonism_winner, "agonism_winner", "agonism_loser")
    col_order=c("agonism_winner","agonism_loser","agonism_type",
                "date","observer","data_source","focal_individual")
    agg_data=agg_data[,col_order]
    
    agg_data$agonism_winner[nchar(agg_data$agonism_winner)!=3|
                           agg_data$agonism_winner=="INF"|
                           agg_data$agonism_winner=="JUV"|
                           agg_data$agonism_winner=="INF"]="UNK"
    agg_data$agonism_loser[nchar(agg_data$agonism_loser)!=3|
                              agg_data$agonism_loser=="INF"|
                              agg_data$agonism_loser=="JUV"|
                              agg_data$agonism_loser=="INF"]="UNK"
    unique(c(unique(agg_data$agonism_winner), unique(agg_data$agonism_loser)))
    
    
    agg_data$conc=paste(agg_data$agonism_winner,agg_data$agonism_loser,sep=".")
    
  }else{ #if not 2018 (i.e. regular focal data)
    ############################################################################ 
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    agg_data = read.csv(paste("Group",groupyears[gy],"_AgonisticActions.txt", sep = ""))
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
    meta_data=meta_data[meta_data$hrs.focalfollowed>0,]
    #cleaned_data = read.csv(paste("Group",groupyears[gy],"_CleanedData.txt", sep = ""))
    
    agg_data$agonism_winner[nchar(agg_data$agonism_winner)>3]="UNK"
    agg_data$agonism_loser[nchar(agg_data$agonism_loser)>3]="UNK"
    agg_data$conc=paste(agg_data$agonism_winner,agg_data$agonism_loser,sep=".")
   
    if (groupyears[gy]=="HH2016"){
      #Add HH dominance for subadults. 
      hh.dominance <- read.csv("HH_Dominance.csv");names(hh.dominance)[1]="id"
      hh.dominance$id = as.character(hh.dominance$id)
      hh.dominance$id[hh.dominance$id=="2.00E+09"]="2E9"
      hh.dominance$id[hh.dominance$id=="2.00E+08"]="2E8"
      hh.dominance$id[hh.dominance$id=="7.00E+00"]="7E0"
      hh.dominance$id[hh.dominance$id=="7.00E+03"]="7E3"
      hh.dominance$id[hh.dominance$id=="8.00E+02"]="8E2"
      
      meta_data[,c("ordinal.rank","percofsex.dominanted")]=hh.dominance[match(meta_data$id, hh.dominance$id),c("ordinal.rank","percofsex.domianted")]
    }
    
    if (groupyears[gy]=="KK2017"){
      #Add HH dominance for juveniles. 
      kk.dominance <- read.csv("KK_dominance_withSubadults.csv");names(kk.dominance)[1]="id"
      
      meta_data[,c("ordinal.rank","percofsex.dominanted")]=kk.dominance[match(meta_data$id, kk.dominance$id),c("ordinal.rank","percofsex.domianted")]
    }
    
    
  } #end of year clause (2018 vs. other)
  
   #Format data with aggregate format
  # Output the Master Edgelist of all possible pairs given the unique IDs.
  unqIDs = c(meta_data$id,"UNK")
  edgelist = calcMasterEL_groom(unqIDs);
  x<-as.data.frame(table(agg_data$conc))
  edgelist$count = x$Freq[match(edgelist$conc, x$Var1)]
  
  df_obs_agg = edgelist
  df_obs_agg$count=ifelse(is.na(edgelist$count),0,edgelist$count)
  names(df_obs_agg)=c("ID1", "ID2", "dyad_id","count")
  
  
  #Get observation effort for each dyad
  #if (years[gy]==2018){
    # numscans = as.data.frame(table(scans2018$focalID))
    # df_obs_agg$ID1_obseff = numscans$Freq[match(df_obs_agg$ID1, numscans$Var1)]
    # df_obs_agg$ID2_obseff = numscans$Freq[match(df_obs_agg$ID2, numscans$Var1)]
  #}else{
    df_obs_agg$ID1_obseff = meta_data$hrs.focalfollowed[match(df_obs_agg$ID1, meta_data$id)]
    df_obs_agg$ID2_obseff = meta_data$hrs.focalfollowed[match(df_obs_agg$ID2, meta_data$id)]
  #}
  
  df_obs_agg$ID1_obseff[is.na(df_obs_agg$ID1_obseff)] <- 0; df_obs_agg$ID2_obseff[is.na(df_obs_agg$ID2_obseff)] <- 0
  df_obs_agg$total_obs_time = df_obs_agg$ID1_obseff + df_obs_agg$ID2_obseff 
  
  ## Add id qualifiers
  #sex
  df_obs_agg$ID1_sex = meta_data$sex[match(df_obs_agg$ID1, meta_data$id)]
  df_obs_agg$ID2_sex = meta_data$sex[match(df_obs_agg$ID2, meta_data$id)]
  #rank
  df_obs_agg$ID1_rank = meta_data$ordinal.rank[match(df_obs_agg$ID1, meta_data$id)]
  df_obs_agg$ID2_rank = meta_data$ordinal.rank[match(df_obs_agg$ID2, meta_data$id)]
  #age
  df_obs_agg$ID1_age = meta_data$age[match(df_obs_agg$ID1, meta_data$id)]
  df_obs_agg$ID2_age = meta_data$age[match(df_obs_agg$ID2, meta_data$id)]
  #group, year, Hurricane status
  df_obs_agg$group = group[gy]; df_obs_agg$year = years[gy]; 
  if(years[gy]>2017){df_obs_agg$isPost = "post"}else{df_obs_agg$isPost = "pre"}
  
  head(df_obs_agg)
  
  
  ###################################################################
  # Merge and save data
  edgelist.all = rbind(edgelist.all, df_obs_agg)
}

# #extract the number of unique IDs
# unique_names <- unique(c(edgelist.all$ID1, edgelist.all$ID2))
# nr_ind <- length(unique_names)
# nr_dyads <- nr_ind*(nr_ind-1)/2 # -1 to remove self-interactions e.g. AA & /2 because undirected so AB = BA
# 
# edgelist.all$ID1 = factor(edgelist.all$ID1, levels = unique_names); edgelist.all$ID2 = factor(edgelist.all$ID2, levels = unique_names); 
# edgelist.all$ID1_id = as.integer(edgelist.all$ID1); edgelist.all$ID2_id = as.integer(edgelist.all$ID2)
# edgelist.all$dyad_id = factor(edgelist.all$dyad_id, levels=edgelist.all$dyad_id)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
save(edgelist.all,file="aggression_data.RData")

