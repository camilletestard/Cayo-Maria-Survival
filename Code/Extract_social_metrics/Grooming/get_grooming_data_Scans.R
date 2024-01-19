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

#Load data
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
allScans= read.csv("allScans.txt")
allScans$groupyear=paste0(allScans$group,allScans$year)

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V","F")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
groupyears = paste0(group,years)

gy=1#16
edgelist.all = data.frame()
savePath = '~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/'

for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  scans = allScans[allScans$groupyear==groupyears[gy],]
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
  meta_data=meta_data[meta_data$hrs.focalfollowed>0,]  
  
  
  groom_data = scans[scans$isSocial==1,c("focalID","partner.ID",'focal.activity.isPost')]
  
  groom_data$groom_giver = ifelse( groom_data$focal.activity.isPost=="G", groom_data$focalID, groom_data$partner.ID)
  groom_data$groom_reciever = ifelse( groom_data$focal.activity.isPost=="E", groom_data$focalID, groom_data$partner.ID)
  
  groom_data$groom_giver[nchar(groom_data$groom_giver)<3|
                           is.na(groom_data$groom_giver)|
                           groom_data$groom_giver=="INF"|
                           groom_data$groom_giver=="JUV"]="UNK"
  groom_data$groom_reciever[nchar(groom_data$groom_reciever)<3|
                              is.na(groom_data$groom_reciever)|
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
  
  numscans = as.data.frame(table(scans$focalID))
  df_obs_agg$ID1_obseff = numscans$Freq[match(df_obs_agg$ID1, numscans$Var1)]
  df_obs_agg$ID2_obseff = numscans$Freq[match(df_obs_agg$ID2, numscans$Var1)]
  df_obs_agg$ID1_obseff[is.na(df_obs_agg$ID1_obseff )]=0; df_obs_agg$ID2_obseff[is.na(df_obs_agg$ID2_obseff )]=0
  df_obs_agg$total_obseff = (df_obs_agg$ID1_obseff + df_obs_agg$ID2_obseff)
  
  df_obs_agg<- df_obs_agg[,c("ID1","ID2","dyad_id","count",   
                             "total_obseff")]
  
  
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
save(edgelist.all,file="grooming_data_scans.RData")

