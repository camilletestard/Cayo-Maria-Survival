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
#pedigree = read.csv("/PEDIGREE_2021.txt", sep = '\t')
rm(list = setdiff(ls(), "census"))

group = c("F","F","F","V","V","V","KK","KK")
years = c(2015, 2016, 2017, 2015, 2016, 2017, 2015, 2017)
groupyears = c("F2015", "F2016", "F2017", "V2015", "V2016", "V2017","KK2015", "KK2017")
SocialCapital.ALL = data.frame()

study.start.date = as.Date("2017-09-17")
study.end.date = as.Date("2021-11-01")
study.end.date.KK = as.Date("2018-11-01") #Separate study end date for KK since they were culled in 2018
study.perod.days = study.end.date - study.start.date
study.perod.days.KK = study.end.date.KK - study.start.date

#####################################################################
# Compute Social Capital Metrics, per individual, per year
#####################################################################

gy=1
for (gy in 1:length(groupyears)){

  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))

  #Load data
  setwd("~/Dropbox (Penn)/CayoBehavior/Data/BehaviorAllGroups_2010-2019/BehaviouralData/")
  groom_data = read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt", sep = ""))
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
  prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = ""))

  #Create Social Capital Data frame & add Sex, Age, Rank, Group and Year
  SocialCapitalData= meta_data[,c("id","sex","ordinal.rank","percofsex.dominanted","hrs.focalfollowed","focalcutoff_met")]
  names(SocialCapitalData)=c("id","sex","ordrank","percentrank","hrs.followed","focalcutoff_met")
  SocialCapitalData$group = group[gy]
  SocialCapitalData$year.prehurr = years[gy]
  if (group[gy] =="KK"){SocialCapitalData$study.end.date = study.end.date.KK}else{SocialCapitalData$study.end.date = study.end.date}
  if (group[gy] =="KK"){study.perod = study.perod.days.KK}else{study.perod = study.perod.days}

  #Get group size at study start
  alive.idx = which((census$DOB < study.start.date &
                       (census$DOD > study.start.date | census$DateTransfer > study.start.date))|
                      census$Status == "IN CS")
  monkeys_alive_studystart = census[alive.idx,]
  SocialCapitalData$group.size = length(which(monkeys_alive_studystart$Natal.Group==group[gy])) #Includes ALL individuals alive at study start date

  ### Get demographic data ###
  ID.idx = match(SocialCapitalData$id, census$AnimalID) #get the idx for the appropriate individuals

  SocialCapitalData$Status = census$Status[ID.idx] #get status (dead, removed or alive)
  SocialCapitalData$DOB = census$DOB[ID.idx] #get DOB
  SocialCapitalData$age = as.numeric(study.start.date -SocialCapitalData$DOB)/365.25
  SocialCapitalData$DOD = census$DOD[ID.idx] #get DOD
  SocialCapitalData$DOT = census$DateTransfer[ID.idx]#get DOT
  SocialCapitalData$Age_entry.days = study.start.date - census$DOB[ID.idx] #age in days at the start of the study

  #Get age in days at the end of the study (until event: death or end of study)
  SocialCapitalData$Age_event.days = SocialCapitalData$DOD- SocialCapitalData$DOB
  SocialCapitalData$Age_event.days[is.na(SocialCapitalData$Age_event.days)] = SocialCapitalData$Age_entry.days[is.na(SocialCapitalData$Age_event.days)] + study.perod

  #If individual was removed or died after the study period, consider it alive
  SocialCapitalData$Status[which(SocialCapitalData$DOT>SocialCapitalData$study.end.date[1])] = "IN CS"
  SocialCapitalData$Status[which(SocialCapitalData$DOD>SocialCapitalData$study.end.date[1])] = "IN CS"
  #Also set event days to end of study rather than DOD.
  SocialCapitalData$Age_event.days[which(SocialCapitalData$DOD>SocialCapitalData$study.end.date[1])] = SocialCapitalData$Age_entry.days[which(SocialCapitalData$DOD>SocialCapitalData$study.end.date[1])] + study.perod

  #Censor individuals that did not die "naturally" during the study period
  cull_censor = SocialCapitalData$Status=="REMOVE"
  SocialCapitalData$Age_event.days[cull_censor] = SocialCapitalData$DOT[cull_censor]- SocialCapitalData$DOB[cull_censor]

  #Number of days in study. Total study period or until death/cull
  SocialCapitalData$days.in.study = SocialCapitalData$Age_event.days-SocialCapitalData$Age_entry.days

  #Create survival column which will be used in all models
  SocialCapitalData$Survival = 0
  SocialCapitalData$Survival[SocialCapitalData$Status=="DEAD"] = 1


  #####################################################################
  ## Extract social integration measure from grooming data
  #Number of grooming partners
  #Strength of bond to top partner
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
  unqIDs = as.character(SocialCapitalData$id)
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

  SocialCapitalData = merge(SocialCapitalData, social_integration, by="id")

  #####################################################################
  ## Extract social integration measure from proximity data
  #Number of proximity partners
  #Probability of proximity 
  #Probability of grooming?
  #####################################################################
  
  prox_partners = as.data.frame(str_split(prox_data$in.proximity, c(","), simplify = TRUE))
  colnames(prox_partners)[1]="focalID"
  
  #Find the number of scans per focal ID
  proxRate = data.frame()
  for (id in 1:length(unqIDs)){
    proxRate[id, "id"]= unqIDs[id]
    scans = which(prox_partners$focalID == unqIDs[id]) #find the number of scans where focal ID = id
    proxRate[id, "numScans"] = length(scans) #num scans
    proxRate[id, "numPartners"] = 0; proxRate[id, "is.in.prox"] = 0;
    
    #Number of unique proximity partners
    prox.scans = prox_data[scans,]
    id.in.prox = str_split(prox.scans$in.proximity, c(","))
    unique.partners.prox = unique(unlist(c(id.in.prox)))
    unique.partners.prox =as.character(gsub("'",'',unique.partners.prox))
    if(length(which(unique.partners.prox =="N/A")!=0)){unique.partners.prox =unique.partners.prox[-which(unique.partners.prox =="N/A")]}
    if(length(which(is.na(unique.partners.prox))!=0)){unique.partners.prox =unique.partners.prox[-which(is.na(unique.partners.prox))]}
    proxRate[id,"num.prox.partners"] = length(unique.partners.prox)
    
    #Find the number of partners in proximity at each scan
    for (i in 1:length(scans)){ #for all scans of id
      numProxPartners = length(which(prox_partners[scans[i],2:length(prox_partners)] != "")) #find the number of partners at that scan
      proxRate[id, "numPartners"] = proxRate[id, "numPartners"] + numProxPartners #add #partners through scans
      if (numProxPartners>0){proxRate[id, "is.in.prox"]=proxRate[id, "is.in.prox"]+1}
    }
    
    #Find the number of groom events
    proxRate[id,"groom.events"] = sum(prox_data$focal.activity[prox_data$focal.monkey == unqIDs[id]] == "social")
    
  }
  
  proxRate$proxRate = proxRate$numPartners/proxRate$numScans #rate is the average number of partner per proximity scan
  proxRate$probProx = proxRate$is.in.prox/proxRate$numScans
  
  SocialCapitalData$numScans = proxRate$numScans
  SocialCapitalData$numPartnersProx = proxRate$num.prox.partners
  SocialCapitalData$prox.events = proxRate$is.in.prox
  SocialCapitalData$prob.prox = proxRate$probProx
  SocialCapitalData$prox.rate = proxRate$proxRate
  SocialCapitalData$groom.events = proxRate$groom.events
  SocialCapitalData$prob.groom =  SocialCapitalData$groom.events/SocialCapitalData$numScans
  
  
  ###################################################################
  #Discard individuals who died before the start of the study
  SocialCapitalData = SocialCapitalData[SocialCapitalData$days.in.study>0,]
  
  # Merge and save data
  SocialCapital.ALL = rbind(SocialCapital.ALL, SocialCapitalData)
}

#Get year of event (or end of study)
SocialCapital.ALL$YearOfEvent = NA
SocialCapital.ALL$YearOfEvent[!is.na(SocialCapital.ALL$DOD)] = as.numeric(format(as.Date(SocialCapital.ALL$DOD[!is.na(SocialCapital.ALL$DOD)], format="%d/%m/%Y"),"%Y"))
SocialCapital.ALL$YearOfEvent[!is.na(SocialCapital.ALL$DOT)] = as.numeric(format(as.Date(SocialCapital.ALL$DOT[!is.na(SocialCapital.ALL$DOT)], format="%d/%m/%Y"),"%Y"))
SocialCapital.ALL$YearOfEvent[is.na(SocialCapital.ALL$YearOfEvent)] = 2021

#PLACE HOLDER TO GET RANK AT TIME OF EVENT.
#Note: for now we exclude rank because of sample size limits

#Only keep individuals that meet the cut off
SocialCapital.ALL=SocialCapital.ALL[SocialCapital.ALL$hrs.followed>=1,]

#Get number of unique IDs included
allids = unique(SocialCapital.ALL$id); table(SocialCapital.ALL$group[match(allids, SocialCapital.ALL$id)])
length(which(SocialCapital.ALL$Survival[match(allids, SocialCapital.ALL$id)]==1)); length(SocialCapital.ALL$Survival[match(allids, SocialCapital.ALL$id)]==1)

#Save file for pre-hurricane sociality
save(SocialCapital.ALL,file ="~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/SocialCapital_Adults.RData")

#Add change in p(grooming) and p(proximity) to the dataframe
#Because this only includes a subset of the individuals, save it in a separate file
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/')
load('ChangeP.RData')
names(dprob.ALL)[6] = "year.prehurr"
full.data = merge(dprob.ALL, SocialCapital.ALL, by=c("id","year.prehurr","group"))
min_obs = 20
full.data<- full.data[as.numeric(full.data$num_obs)>=min_obs,];

save(full.data,file ="~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/SocialCapital_changeP_Adults.RData")

#Get the numebr of unique IDs for change in pre vs. post hurricane sociality
example_data <- full.data[full.data$iter ==1,]
allids_prepost = unique(example_data$id); table(example_data$group[match(allids, example_data$id)])
length(which(example_data$Survival[match(allids_prepost, example_data$id)]==1)); length(example_data$Survival[match(allids_prepost, example_data$id)]==1)
setwd('~/Documents/GitHub/Cayo-Maria-Survival/'); write.csv(example_data, "example_Datasheet_survivalAnalysis.csv", row.names = F)


