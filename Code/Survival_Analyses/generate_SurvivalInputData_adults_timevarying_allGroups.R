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
setwd("~/Downloads/")
ages=read.csv('surv_ages.csv')

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

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V",
          "V","KK","S","V","F","V","TT","V","F")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
groupyears = paste0(group,years)

meta_data.all = data.frame()
for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  #Load data
  setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
  meta_data$group=group[gy]
  meta_data$year = years[gy]
  meta_data$id.year = paste0(meta_data$id, meta_data$year)
  meta_data.all = rbind(meta_data.all, meta_data[,c("id","sex","group","year","id.year","ordinal.rank","percofsex.dominanted","hrs.focalfollowed","focalcutoff_met")])
  
}
meta_data.all=meta_data.all[meta_data.all$hrs.focalfollowed>0,] #For group F2022


#Set study period dates (consider each year separately)
start.dates = c("2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01",
                "2017-09-18","2019-01-01","2020-01-01","2021-01-01","2022-01-01")
end.dates = c("2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-09-17",
              "2018-12-31","2019-12-31","2020-12-31","2021-12-31","2022-12-31")

yr=1; SurvivalData.ALL=data.frame()
for (yr in 1:length(start.dates)){
  
  SurvivalData = meta_data.all
  SurvivalData$study.start.date = as.Date(start.dates[yr])
  SurvivalData$study.end.date = as.Date(end.dates[yr])
  SurvivalData$study.period.days = SurvivalData$study.end.date - SurvivalData$study.start.date
  study.period=SurvivalData$study.period.days[1]
  SurvivalData$period = year(end.dates[yr])
  
  
  # if (group[gy] =="KK"){SurvivalData$study.end.date = study.end.date.KK}else{SurvivalData$study.end.date = study.end.date}
  # if (group[gy] =="KK"){study.perod = study.perod.days.KK}else{study.perod = study.perod.days}
  
  ### Get demographic data ###
  ID.idx = match(SurvivalData$id, census$AnimalID) #get the idx for the appropriate individuals
  
  #SANITY CHECK: females less likely to be dead than males?
  # census.study = census[ID.idx,]
  # table(census.study$Sex, census.study$Status) #--> actually no!
  
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
    SurvivalData$Age_entry.days[is.na(SurvivalData$Age_event.days)] + study.period
  
  #If individual died or was transferred after the study period, consider it alive
  SurvivalData$Status[which(SurvivalData$DOT>SurvivalData$study.end.date[1])] = "IN CS"
  SurvivalData$Status[which(SurvivalData$DOD>SurvivalData$study.end.date[1])] = "IN CS"
  #Also set event days to end of study rather than DOD or DOT.
  SurvivalData$Age_event.days[which(SurvivalData$DOD>SurvivalData$study.end.date[1])] = SurvivalData$Age_entry.days[which(SurvivalData$DOD>SurvivalData$study.end.date[1])] + study.period
  SurvivalData$Age_event.days[which(SurvivalData$DOT>SurvivalData$study.end.date[1])] = SurvivalData$Age_entry.days[which(SurvivalData$DOT>SurvivalData$study.end.date[1])] + study.period
  
  #Number of days in study. Total study period or until death/cull
  SurvivalData$days.in.study = SurvivalData$Age_event.days-SurvivalData$Age_entry.days
  
  #Create survival column which will be used in all models
  SurvivalData$Survival = 0
  SurvivalData$Survival[SurvivalData$Status=="DEAD"] = 1
  
  #Remove individuals that were dead or transferred before the start of the study
  SurvivalData= SurvivalData[SurvivalData$days.in.study>0,]
  
  # Merge and save data
  SurvivalData.ALL = rbind(SurvivalData.ALL, SurvivalData)
}


#Get year of death
SurvivalData.ALL$YearOfDeath = year(SurvivalData.ALL$DOD)

#Only keep one entry per individual and study period
SurvivalData.ALL<-SurvivalData.ALL[which(SurvivalData.ALL$period==2013 & SurvivalData.ALL$year==2013|
                                           SurvivalData.ALL$period==2014 & SurvivalData.ALL$year==2014|
                                           SurvivalData.ALL$period==2015 & SurvivalData.ALL$year==2015|
                                           SurvivalData.ALL$period==2016 & SurvivalData.ALL$year==2016|
                                           SurvivalData.ALL$period==2017 & SurvivalData.ALL$year==2017|
                                           SurvivalData.ALL$period==2018 & SurvivalData.ALL$year==2018|
                                           SurvivalData.ALL$period==2019 & SurvivalData.ALL$year==2019|
                                           SurvivalData.ALL$period==2020 & SurvivalData.ALL$year==2019|
                                           SurvivalData.ALL$period==2021 & SurvivalData.ALL$year==2021|
                                           SurvivalData.ALL$period==2022 & SurvivalData.ALL$year==2022),]

#Get number of IDs and deaths pre-hurricane
preHurrData = SurvivalData.ALL[SurvivalData.ALL$year<2018,]; length(unique(preHurrData$id))
postHurrData = SurvivalData.ALL[SurvivalData.ALL$year>2017,]; length(unique(postHurrData$id)); 
preHurrDataDeaths = SurvivalData.ALL[SurvivalData.ALL$year<2018 & SurvivalData.ALL$Survival==1,]; length(unique(preHurrDataDeaths$id))
postHurrDataDeaths = SurvivalData.ALL[SurvivalData.ALL$year>2017 & SurvivalData.ALL$Survival==1,]; length(unique(postHurrDataDeaths$id))



#Save file for pre-hurricane sociality
save(SurvivalData.ALL,file ="~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/Survival_Adults_TimeVarying_allgroups.RData")
# load("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/SocialCapital_Adults.RData")
# 
# #Add change in p(grooming) and p(proximity) to the dataframe
# #Because this only includes a subset of the individuals, save it in a separate file
# setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/')
# load('ChangeP.RData')
# names(dprob.ALL)[6] = "year.prehurr"
# full.data = merge(dprob.ALL, SurvivalData.ALL, by=c("id","year.prehurr","group"))
# #min_obs = 20
# #full.data<- full.data[as.numeric(full.data$num_obs)>=min_obs,];
# 
# save(full.data,file ="~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/SocialCapital_changeP_Adults.RData")
# 
# #Get the number of unique IDs for change in pre vs. post hurricane sociality
# example_data <- full.data[full.data$iter ==1,]
# allids_prepost = unique(example_data$id); 
# unique_exampleData <- example_data[match(allids_prepost, example_data$id),]
# table(unique_exampleData$group); table(unique_exampleData$Survival)
# table(unique_exampleData$group, unique_exampleData$Survival)
# 

