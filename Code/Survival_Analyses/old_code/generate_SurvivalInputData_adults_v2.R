#Generate input data for survival models.
#Feburary 2022, Camille Testard

library(stringr)
library(igraph)
library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#Load scan data and population info
setwd("~/Dropbox (Penn)/CayoBehavior/Data/Census")
census = xlsx::read.xlsx("2021-12-16-CENSO-FINAL.xlsx", 1)

#Update census to consider deaths in 2022
death_update2022 =xlsx::read.xlsx("Deaths2022.xlsx",1)
death_update2022$Status = "DEAD"
id=1
for (id in 1:nrow(death_update2022)){
  idx=which(census$AnimalID == death_update2022$AnimalID[id])
  if(!purrr::is_empty(idx)){
    census[idx,c("DOD","Status")]=death_update2022[id, c("DOD","Status")]
  }
}


#pedigree = read.csv("/PEDIGREE_2021.txt", sep = '\t')
rm(list = setdiff(ls(), "census"))


#Set study start and end dates
study.start.date = as.Date("2017-09-17")
study.end.date = as.Date("2022-09-15")
#study.end.date.KK = as.Date("2018-11-01") #Separate study end date for KK since they were culled in 2018


#Extract id list and parmeters
group = c("F","F","F","V","V","V","KK","KK")
years = c(2015, 2016, 2017, 2015, 2016, 2017, 2015, 2017)
groupyears = c("F2015", "F2016", "F2017", "V2015", "V2016", "V2017","KK2015", "KK2017")

meta_data.all = data.frame()
for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  #Load data
  setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
  meta_data$group=group[gy]
  meta_data$year = years[gy]
  meta_data$id.year = paste(meta_data$id, meta_data$year,sep=".")
  meta_data.all = rbind(meta_data.all, meta_data[,c("id","sex","group","year","id.year","ordinal.rank","percofsex.dominanted","hrs.focalfollowed","focalcutoff_met")])
  
}
SurvivalData = meta_data.all


### Get demographic data ###

#Set start and end dates
SurvivalData$study.start.date = study.start.date
SurvivalData$study.end.date = study.end.date; #SurvivalData$study.end.date[SurvivalData$group =="KK"] = study.end.date.KK
SurvivalData$study.period.days = SurvivalData$study.end.date - SurvivalData$study.start.date

ID.idx = match(SurvivalData$id, census$AnimalID) #get the idx for the appropriate individuals

SurvivalData$Status = census$Status[ID.idx] #get status (dead, removed or alive)
SurvivalData$DOB = census$DOB[ID.idx] #get DOB
SurvivalData$age = as.numeric(SurvivalData$study.start.date -SurvivalData$DOB)/365.25
SurvivalData$DOD = census$DOD[ID.idx] #get DOD
SurvivalData$DOT = census$DateTransfer[ID.idx]#get DOT
SurvivalData$Age_entry.days = SurvivalData$study.start.date - SurvivalData$DOB #age in days at the start of the study

#Get age in days at the end of the study (until event: death, removal or end of study)
SurvivalData$Age_event.days = SurvivalData$DOD- SurvivalData$DOB #event = death
SurvivalData$Age_event.days[!is.na(SurvivalData$DOT)] = #event = removal
  SurvivalData$DOT[!is.na(SurvivalData$DOT)]- SurvivalData$DOB[!is.na(SurvivalData$DOT)]
SurvivalData$Age_event.days[is.na(SurvivalData$Age_event.days)] = #event = end of study
  SurvivalData$Age_entry.days[is.na(SurvivalData$Age_event.days)] + SurvivalData$study.period.days[is.na(SurvivalData$Age_event.days)]

#If individual died after the study period, consider it alive
SurvivalData$Status[which(SurvivalData$DOT>SurvivalData$study.end.date)] = "IN CS"
SurvivalData$Status[which(SurvivalData$DOD>SurvivalData$study.end.date)] = "IN CS"
#Also set event days to end of study rather than DOD or DOT.
SurvivalData$Age_event.days[which(SurvivalData$DOD>SurvivalData$study.end.date)] = SurvivalData$Age_entry.days[which(SurvivalData$DOD>SurvivalData$study.end.date)] +
                                                                                   SurvivalData$study.period.days[which(SurvivalData$DOD>SurvivalData$study.end.date)]
SurvivalData$Age_event.days[which(SurvivalData$DOT>SurvivalData$study.end.date)] = SurvivalData$Age_entry.days[which(SurvivalData$DOT>SurvivalData$study.end.date)] +
                                                                                   SurvivalData$study.period.days[which(SurvivalData$DOT>SurvivalData$study.end.date)]

#Number of days in study. Total study period or until death/cull
SurvivalData$days.in.study = SurvivalData$Age_event.days-SurvivalData$Age_entry.days

#Create survival column which will be used in all models
SurvivalData$Survival = ifelse(SurvivalData$Status=="DEAD",1,0)

#Remove individuals that were dead or transferred before the start of the study
SurvivalData= SurvivalData[SurvivalData$days.in.study>0,]

#Get year of event (or end of study)
SurvivalData$YearOfEvent = NA
SurvivalData$YearOfEvent[!is.na(SurvivalData$DOD)] = as.numeric(format(as.Date(SurvivalData$DOD[!is.na(SurvivalData$DOD)], format="%d/%m/%Y"),"%Y"))
SurvivalData$YearOfEvent[!is.na(SurvivalData$DOT)] = as.numeric(format(as.Date(SurvivalData$DOT[!is.na(SurvivalData$DOT)], format="%d/%m/%Y"),"%Y"))
SurvivalData$YearOfEvent[is.na(SurvivalData$YearOfEvent)] = 2022

#Get year of death
SurvivalData$YearOfDeath = year(SurvivalData$DOD)
SurvivalData$YearOfDeath[SurvivalData$Survival == 0]=NA

#Get number of unique IDs included
allids = unique(SurvivalData$id); 
unique_Survival = SurvivalData[match(allids, SurvivalData$id),]
table(unique_Survival$group); table(unique_Survival$Survival)
table(unique_Survival$group, unique_Survival$Survival)

table(unique_Survival$group, unique_Survival$YearOfDeath)

#Save file for pre-hurricane sociality
save(SurvivalData,file ="~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/Survival_Adults.RData")
