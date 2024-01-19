#Generate input data for survival models.
#Feburary 2022, Camille Testard

library(stringr)
library(igraph)
library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

#Load scan data and population info
setwd("~/Dropbox (Penn)/CayoBehavior/Data/Census")
census = xlsx::read.xlsx("2021-12-16-CENSO-FINAL.xlsx", 1)
rm(list = setdiff(ls(), "census")) #Remove all variables except census which takes a long time to load

group = c("F","F","F","V","V","V","KK","KK")
years = c(2015, 2016, 2017, 2015, 2016, 2017, 2015, 2017)
groupyears = c("F2015", "F2016", "F2017", "V2015", "V2016", "V2017","KK2015", "KK2017")
SocialCapital.ALL = data.frame()

study.start.date = as.Date("2017-09-17")
study.end.date = as.Date("2021-11-01") 
study.end.date.KK = as.Date("2018-11-01") #Separate study end date for KK since they were culled in 2018
study.perod.days = study.end.date - study.start.date
study.perod.days.KK = study.end.date.KK - study.start.date
birth.date = as.Date("2016-10-01") #Only include infants born after that age

gy=1
for (gy in 1:length(groupyears)){

  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))

  #Select end date based on group
  if (group[gy] =="KK"){end.date = study.end.date.KK}else{end.date = study.end.date}
  if (group[gy] =="KK"){study.perod = study.perod.days.KK}else{study.perod = study.perod.days}

  #####################################################################
  # Get all infants that match study criteria
  #####################################################################

  SocialCapitalData = census[census$DOB>birth.date & census$DOB<end.date & census$Natal.Group==group[gy],
                             c("AnimalID","DOB","Sex","Natal.Group","BehaviorMom","Status","DOD","DateTransfer")]
  SocialCapitalData$OneYearAnniversary = SocialCapitalData$DOB + 365
  SocialCapitalData$group = group[gy]
  SocialCapitalData$year.prehurr = years[gy]
  SocialCapitalData$age = as.numeric(study.start.date -SocialCapitalData$DOB)/365.25
  SocialCapitalData$study.end.date = end.date
  SocialCapitalData$Age_entry.days = study.start.date - SocialCapitalData$DOB #age in days at the start of the study
  SocialCapitalData$Age_entry.days[SocialCapitalData$Age_entry.days<0]=0 #set negative age to 0

  #Get age in days at the end of the study (until event: remove, death or end of study)
  SocialCapitalData$Age_event.days = SocialCapitalData$DOD- SocialCapitalData$DOB #age at death
  #Censor individuals that did not die "naturally" during the study period
  cull_censor = SocialCapitalData$Status=="REMOVE"
  SocialCapitalData$Age_event.days[cull_censor] = SocialCapitalData$DateTransfer[cull_censor]- SocialCapitalData$DOB[cull_censor] #age when removed
  #For individuals that were not removed and did not die
  SocialCapitalData$Age_event.days[is.na(SocialCapitalData$Age_event.days)] = end.date - SocialCapitalData$DOB[is.na(SocialCapitalData$Age_event.days)]
  
  #If individual was removed or died after the study period, consider it alive
  SocialCapitalData$Status[which(SocialCapitalData$DateTransfer>end.date)] = "IN CS"
  SocialCapitalData$Status[which(SocialCapitalData$DOD>end.date)] = "IN CS"
  #Also set event days to end of study rather than DOD.
  SocialCapitalData$Age_event.days[which(SocialCapitalData$DOD>end.date)] = end.date - SocialCapitalData$DOB[which(SocialCapitalData$DOD>end.date)]

  #Number of days in study. Total study period or until death
  SocialCapitalData$days.in.study = SocialCapitalData$Age_event.days-SocialCapitalData$Age_entry.days

  #Create survival column which will be used in all models
  SocialCapitalData$Survival = 0
  SocialCapitalData$Survival[SocialCapitalData$Status=="DEAD"] = 1

  #Only consider individuals who were alive at the start of the study
  SocialCapitalData = SocialCapitalData[SocialCapitalData$days.in.study>0,]

  #####################################################################
  # Compute Social Capital Metrics for mothers per year pre-hurricane
  #####################################################################

  #Load data
  setwd("~/Dropbox (Penn)/CayoBehavior/Data/BehaviorAllGroups_2010-2019/BehaviouralData/")
  groom_data = read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt", sep = ""))
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
  prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = ""))

  #Create Social Capital Data frame & add Sex, Age, Rank, Group and Year
  adult_data= meta_data[meta_data$focalcutoff_met=="Y",c("id","sex","ordinal.rank","percofsex.dominanted","hrs.focalfollowed")]
  names(adult_data)=c("id","sex","ordrank","percentrank","hrs.followed")

  #Get group size at study start
  alive.idx = which((census$DOB < study.start.date &
                       (census$DOD > study.start.date | census$DateTransfer > study.start.date))|
                      census$Status == "IN CS")
  monkeys_alive_studystart = census[alive.idx,]
  adult_data$group.size = length(which(monkeys_alive_studystart$Natal.Group==group[gy])) #Includes ALL individuals alive at study start date

  #####################################################################
  ## Extract social integration measure from grooming data
  #####################################################################

  ## Format data
  groom_data$conc = paste(groom_data$groom_giver,groom_data$groom_reciever,sep=".")
  groom_data_compiled = aggregate(constrained_duration ~ conc, data=groom_data, FUN=sum)
  groom_data_compiled$giver = substring(groom_data_compiled$conc,1,3)
  groom_data_compiled$receiver = substring(groom_data_compiled$conc,5,8)
  groom_data_compiled = select(groom_data_compiled, giver, receiver, conc, constrained_duration)
  groom_data_compiled$giver.hrsfollowed = meta_data$hrs.focalfollowed[match(groom_data_compiled$giver, meta_data$id)]
  groom_data_compiled$receiver.hrsfollowed = meta_data$hrs.focalfollowed[match(groom_data_compiled$receiver, meta_data$id)]

  #Create grooming matrix
  adjMat = dils::AdjacencyFromEdgelist(select(groom_data_compiled, giver, receiver, constrained_duration))
  groomMat = adjMat[["adjacency"]]; rownames(groomMat) = adjMat[["nodelist"]]; colnames(groomMat) = adjMat[["nodelist"]]

  #Find number of unique partners
  unqIDs = as.character(adult_data$id)
  id=1; social_integration= data.frame(); partner_strength=matrix(NA, nrow = length(unqIDs), ncol=20); partner_DSI=matrix(NA, nrow = length(unqIDs), ncol=20)
  for (id in 1:length(unqIDs)){
    social_integration[id,"id"] = unqIDs[id];
    partners = unique(c(groom_data_compiled$receiver[groom_data_compiled$giver == unqIDs[id]],
                        groom_data_compiled$giver[groom_data_compiled$receiver == unqIDs[id]]))
    if (length(partners)!=0){
      for (p in 1:length(partners)){
        partner_strength[id,p]= groomMat[unqIDs[id], partners[p]] + groomMat[partners[p], unqIDs[id]]
        partner_DSI[id, p]= partner_strength[id,p]/sum(meta_data$hrs.focalfollowed[match(unqIDs[id], meta_data$id)], meta_data$hrs.focalfollowed[match(partners[p], meta_data$id)], na.rm = T)
      }
    }else{
      partner_DSI[id, 1]=0}
    social_integration[id,"num.partners"] = length(partners)
    social_integration[id,"top.partner"] = max(partner_DSI[id,], na.rm = T)
  }
  mean_groom_rate = mean(partner_DSI, na.rm = T)
  social_integration$top.partner=social_integration$top.partner/mean_groom_rate
  social_integration$std.num.partners = social_integration$num.partners/mean(social_integration$num.partners)
  adult_data = merge(adult_data, social_integration, by="id")
  names(adult_data)[1]="BehaviorMom"
  full.data=merge(SocialCapitalData, adult_data, by="BehaviorMom")

  final.data=full.data[,c("AnimalID", "Sex","age","group","DOB","DOD","DateTransfer","year.prehurr","study.end.date",
                         "Age_entry.days","Age_event.days","days.in.study","Survival","BehaviorMom","ordrank","percentrank",
                         "hrs.followed","group.size","num.partners","top.partner","std.num.partners")]
  names(final.data) = c("id","sex","age.startofsutdy","group","DOB","DOD","DOT","year.prehurr","study.end.date",
                       "Age_entry.days","Age_event.days","days.in.study","Survival","mom.id","mom.ordrank","mom.percentrank",
                       "mom.hrs.followed","group.size","mom.num.partners","mom.top.partner","mom.std.num.partners")

  final.data$sex = toupper(final.data$sex)
  ###################################################################
  # Merge and save data
  ###################################################################

  ### Merge infant and mother data ###

  SocialCapital.ALL = rbind(SocialCapital.ALL, final.data)
}

SocialCapital.ALL$idyear = paste(SocialCapital.ALL$id, SocialCapital.ALL$year.prehurr, sep=".")

allids = unique(SocialCapital.ALL$id); table(SocialCapital.ALL$group[match(allids, SocialCapital.ALL$id)])
length(which(SocialCapital.ALL$Survival[match(allids, SocialCapital.ALL$id)]==1)); length(SocialCapital.ALL$Survival[match(allids, SocialCapital.ALL$id)]==1)
save(SocialCapital.ALL,file ="~/Documents/GitHub/Cayo-Maria-Survival/R.Data/SocialCapital_Infants.RData")

setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data/')
load('ChangeP.RData')
names(dprob.ALL)[6] = "year.prehurr"; names(dprob.ALL)[1] = "mom.id"; names(dprob.ALL)[2]="mom.dpAcc"; names(dprob.ALL)[3]="mom.dpSocial"
full.data = merge(dprob.ALL, SocialCapital.ALL, by=c("mom.id","year.prehurr","group"))
min_obs = 20
full.data<- full.data[as.numeric(full.data$num_obs)>=min_obs,];

save(full.data,file ="~/Documents/GitHub/Cayo-Maria-Survival/R.Data/SocialCapital_changeP_infants.RData")

example_data <- full.data[full.data$iter ==1,]
allids_prepost = unique(example_data$id); table(example_data$group[match(allids, example_data$id)])
length(which(example_data$Survival[match(allids_prepost, example_data$id)]==1)); length(example_data$Survival[match(allids_prepost, example_data$id)]==1)
setwd('~/Documents/GitHub/Cayo-Maria-Survival/'); write.csv(example_data, "example_Datasheet_survivalAnalysis.csv", row.names = F)

