#Get_grooming_data.R
#This script aggregates the grooming data from all groups and years from 2015 to 2021
#from the cayo database. The output of this script will be used to generate 
# grooming networks with bison.
#C Testard August 2022

library(stringr)
library(igraph)
library(lubridate)
library(hms)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(bisonR)

#Load functions
setwd("~/Documents/GitHub/Cayo-Maria-Survival/")
source("Code/Functions/functions_GlobalNetworkMetrics.R")


group = c("F","V","KK","V","F","F","KK","V","S","V","F","V")
years = c(2015,2015,2015,
          2016,2016,2017,2017,2017,
          2019, 2019,2021,2021)
groupyears = c("F2015","V2015","KK2015",
               "V2016","F2016","F2017",
               "KK2017","V2017",
               "S2019","V2019","F2021","V2021")

gy=1#16
edgelist.all = data.frame()
savePath = '~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/'

for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  
  if (years[gy]==2018) {
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    scans2018= read.csv(paste("Group",groupyears[gy],"_scansamples_FULL_CLEANED.txt", sep = ""))
    
    # length(which(scans2018$subject.activity=="G "|scans2018$subject.activity=="E"))
    # idx=which(nchar(scans2018$prox.adult.IDs)==3 & scans2018$prox.adult.IDs!="N/A" &
    #             (scans2018$subject.activity=="F "|scans2018$subject.activity=="R"))
    # idx_change=sample(idx,2500)
    # scans2018$subject.activity[idx_change[1:length(idx_change)/2]]="G"
    # scans2018$subject.activity[idx_change[length(idx_change)/2+1:length(idx_change)]]="E"
    # 
    # idx.na=which(scans2018$partner.ID=="N/A" & nchar(scans2018$prox.adult.IDs)==3
    #              & scans2018$prox.adult.IDs!="N/A" &
    #                (scans2018$subject.activity=="G "|scans2018$subject.activity=="E"))
    # scans2018$partner.ID[idx.na]=scans2018$prox.adult.IDs[idx.na]
    # length(which(scans2018$subject.activity=="G "|scans2018$subject.activity=="E"))

    
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
    
    
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
    scans2018$focal.activity[unique(c(which(scans2018$focal.activity=='G'),which(scans2018$focal.activity=='E'),which(scans2018$focal.activity=='E,P'),which(scans2018$focal.activity=='G,E')))]="social"
    scans2018$focal.activity[unique(c(which(scans2018$focal.activity=='R'),which(scans2018$focal.activity=='P')))]="rest"
    scans2018$focal.activity[unique(c(which(scans2018$focal.activity=='AG'),which(scans2018$focal.activity=='AR')))]="aggression"
    scans2018$focal.activity[unique(c(which(scans2018$focal.activity=='SR'),which(scans2018$focal.activity=='SG')))]="submit"
    scans2018$focal.activity[grep('T',scans2018$focal.activity)]="travel"
    scans2018$focal.activity[grep('F',scans2018$focal.activity)]="feed"
    scans2018$focal.activity[grep('D',scans2018$focal.activity)]="drink"
    scans2018$focal.activity[grep('SD',scans2018$focal.activity)]="sdb"
    scans2018$focal.activity[grep('N/A',scans2018$focal.activity)]="UNK"
    #unique(scans2018$focal.activity) #Check correct activity categories
    
    #Format in.proximity, count number of prox partners
    scans2018$partner.ID = as.character(scans2018$partner.ID); scans2018$partner.ID[which(scans2018$partner.ID=="N/A")]=NA
    scans2018$in.proximity = as.character(scans2018$in.proximity); scans2018$in.proximity[which(scans2018$in.proximity=="N/A")]=NA
    scans2018$num.prox = str_count(as.character(scans2018$in.proximity),",")+1
    scans2018$num.prox[is.na(scans2018$num.prox)]=0
    
    #Add social information
    scans2018$isSocial=0; scans2018$isSocial[which(scans2018$focal.activity=="social")]=1
    scans2018$isSocialGive = 0; scans2018$isSocialGive[which(scans2018$focal.activity.isPost=="G")]=1
    scans2018$isSocialGet = 0; scans2018$isSocialGet[which(scans2018$focal.activity.isPost=="E")]=1
   
    groom_data = scans2018[scans2018$isSocial==1,c("focalID","partner.ID",'focal.activity.isPost')]
    
    groom_data$groom_giver = ifelse( groom_data$focal.activity.isPost=="G", groom_data$focalID, groom_data$partner.ID)
    groom_data$groom_reciever = ifelse( groom_data$focal.activity.isPost=="E", groom_data$focalID, groom_data$partner.ID)
    
    groom_data$groom_giver[nchar(groom_data$groom_giver)<3|
                             groom_data$groom_giver=="INF"|
                             groom_data$groom_giver=="JUV"]="UNK"
    groom_data$groom_reciever[nchar(groom_data$groom_reciever)<3|
                             groom_data$groom_reciever=="INF"|
                             groom_data$groom_reciever=="JUV"]="UNK"
    unique(groom_data$groom_giver); unique(groom_data$groom_reciever)
    
    groom_data$conc = paste(groom_data$groom_giver, groom_data$groom_reciever, sep=".")
    
    #Format data with aggregate format
    # Output the Master Edgelist of all possible pairs given the unique IDs.
    unqIDs = c(meta_data$id,"UNK")
    edgelist = calcMasterEL_groom(unqIDs);
    
    x<-as.data.frame(table(groom_data$conc))
    edgelist$count = x$Freq[match(edgelist$conc, x$Var1)]
    
    df_obs_agg = edgelist
    df_obs_agg$count=ifelse(is.na(edgelist$count),0,edgelist$count) 
    names(df_obs_agg)=c("ID1", "ID2", "dyad_id","count")
    #df_obs_agg$duration = NA; df_obs_agg$simulated_counts = NA
    
    numscans = as.data.frame(table(scans2018$focalID))
    df_obs_agg$ID1_obseff = numscans$Freq[match(df_obs_agg$ID1, numscans$Var1)]
    df_obs_agg$ID2_obseff = numscans$Freq[match(df_obs_agg$ID2, numscans$Var1)]
    df_obs_agg$ID1_obseff[is.na(df_obs_agg$ID1_obseff )]=0; df_obs_agg$ID2_obseff[is.na(df_obs_agg$ID2_obseff )]=0
    df_obs_agg$total_obseff = (df_obs_agg$ID1_obseff + df_obs_agg$ID2_obseff)
    
    df_obs_agg<- df_obs_agg[,c("ID1","ID2","dyad_id","count",   
                               "total_obseff")]

  }else{ #if not 2018 (i.e. regular focal data)
    ############################################################################ 
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = ""))
    groom_data = read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt", sep = ""))
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
    
    groom_data$groom_giver[nchar(groom_data$groom_giver)>3]="UNK"
    groom_data$groom_reciever[nchar(groom_data$groom_reciever)>3]="UNK"
    
    groom_data$conc = paste(groom_data$groom_giver, groom_data$groom_reciever, sep=".")
    
    
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
    
    #Format data with aggregate format
    # Output the Master Edgelist of all possible pairs given the unique IDs.
    unqIDs = c(meta_data$id,"UNK")
    edgelist = calcMasterEL_groom(unqIDs);
    
    x<-as.data.frame(table(groom_data$conc))
    edgelist$count = x$Freq[match(edgelist$conc, x$Var1)]
    
    df_obs_agg = edgelist
    df_obs_agg$count=ifelse(is.na(edgelist$count),0,edgelist$count)
    names(df_obs_agg)=c("ID1", "ID2", "dyad_id","count")
    df_obs_agg$duration=0
    
    for (r in 1:nrow(df_obs_agg)){
    df_obs_agg$duration[r] = sum(groom_data$constrained_duration[which(groom_data$conc==df_obs_agg$dyad_id[r])])
    }
    
    #Get observation effort for each dyad
    df_obs_agg$ID1_obseff = meta_data$hrs.focalfollowed[match(df_obs_agg$ID1, meta_data$id)]
    df_obs_agg$ID2_obseff = meta_data$hrs.focalfollowed[match(df_obs_agg$ID2, meta_data$id)]
    df_obs_agg$ID1_obseff[is.na(df_obs_agg$ID1_obseff )]=0; df_obs_agg$ID2_obseff[is.na(df_obs_agg$ID2_obseff )]=0
    df_obs_agg$total_obseff = (df_obs_agg$ID1_obseff + df_obs_agg$ID2_obseff)
   
    #Simulate binary data
    df=df_obs_agg; names(df)[5]="simulated_counts"
    df$total_obseff = (df$ID1_obseff + df$ID2_obseff)*3600
    df<-bisonR::convert_duration_to_binary(df, "simulated_counts","total_obseff", max(groom_data$constrained_duration))
    df$count<-df$simulated_counts
    df_obs_agg=df
    
    df_obs_agg<- df_obs_agg[,c("ID1","ID2","dyad_id","count",   
                              "total_obseff")]
    
  } #end of year clause (2018 vs. other)
  
  
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

# #Check V2019 data is normal
# df = edgelist.all[edgelist.all$group=="V" & edgelist.all$year=="2015",]
# length(which(df$count!=0))/nrow(df)

#extract the number of unique IDs
unique_names <- unique(c(df_obs_agg$ID1, df_obs_agg$ID2))
nr_ind <- length(unique_names)
nr_dyads <- nr_ind*(nr_ind-1)/2 # -1 to remove self-interactions e.g. AA & /2 because undirected so AB = BA

df_obs_agg$ID1 = factor(df_obs_agg$ID1, levels = unique_names); df_obs_agg$ID2 = factor(df_obs_agg$ID2, levels = unique_names); 
df_obs_agg$ID1_id = as.integer(df_obs_agg$ID1); df_obs_agg$ID2_id = as.integer(df_obs_agg$ID2)
df_obs_agg$dyad_id = factor(df_obs_agg$dyad_id, levels=df_obs_agg$dyad_id)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
save(edgelist.all, groupyears,file="grooming_data_no2018.RData")

