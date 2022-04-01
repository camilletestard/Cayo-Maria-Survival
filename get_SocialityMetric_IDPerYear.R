#Get_SocialityMetric_IDPerYear.R

library(stringr)
library(igraph)
library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lmerTest)
library(lme4)

group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","S","V","F","V")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2019, 2019,2021,2021)
groupyears = c("F2013","KK2013","F2014","HH2014","F2015","V2015","R2015","KK2015",
               "R2016","V2016","F2016","HH2016","F2017","KK2017","V2017",
               "S2019","V2019","F2021","V2021") 
gy=1
SocialCapital.ALL = data.frame()

for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  #Load data
  setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data All Cleaned/BehavioralDataFiles')
  groom_data = read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt", sep = ""))
  agg_data = read.csv(paste("Group",groupyears[gy],"_AgonisticActions.txt", sep = ""))
  focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = ""))
  prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = ""))
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data

  SocialCapitalData= meta_data[meta_data$focalcutoff_met=="Y",c("id","sex","age","ordinal.rank","percofsex.dominanted","hrs.focalfollowed")]
  SocialCapitalData$group = group[gy]
  SocialCapitalData$year = years[gy]
  
  #####################################################################
  ## Extract social integration measure from grooming data
  #Number of grooming partners
  #Strength of bond to top partner
  #Groom rate
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
    social_integration[id,"groom.rec"] = sum(groom_data_compiled$constrained_duration[groom_data_compiled$receiver == unqIDs[id]])
    social_integration[id,"groom.give"] = sum(groom_data_compiled$constrained_duration[groom_data_compiled$giver == unqIDs[id]])
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
  social_integration$top.partner.DSI=social_integration$top.partner/mean_groom_rate
  social_integration$std.num.partners = social_integration$num.partners/mean(social_integration$num.partners)
  social_integration$groom.rate = (social_integration$groom.rec + social_integration$groom.give)/
    SocialCapitalData$hrs.focalfollowed[match(social_integration$id, SocialCapitalData$id)]
  
  SocialCapitalData[,c("num.partners","groom.rate")] = social_integration[match(SocialCapitalData$id, social_integration$id),c("num.partners","groom.rate")]
  
  #####################################################################
  ## Extract social integration measure from proximity data
  #Number of proximity partners
  #Probability of proximity? Proximity rate?
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
    #Find the number of partners
    for (i in 1:length(scans)){ #for all scans of id
      numProxPartners = length(which(prox_partners[scans[i],2:length(prox_partners)] != "")) #find the number of partners at that scan
      proxRate[id, "numPartners"] = proxRate[id, "numPartners"] + numProxPartners #add #partners through scans
      if (numProxPartners>0){proxRate[id, "is.in.prox"]=proxRate[id, "is.in.prox"]+1}
    }
  }
  proxRate$proxRate = proxRate$numPartners/proxRate$numScans #rate is the average number of partner per proximity scan
  proxRate$probProx = proxRate$is.in.prox/proxRate$numScans
 
  SocialCapitalData$CSIprox = proxRate$proxRate[match(SocialCapitalData$id,proxRate$id)]
  SocialCapitalData$prob.prox = proxRate$probProx[match(SocialCapitalData$id,proxRate$id)]
  
  #####################################################################
  ## Extract aggression rate
  #####################################################################
  
  aggressionRate = data.frame()
  for (id in 1:length(unqIDs)){
    aggressionRate[id, "id"]= unqIDs[id]
    aggressionRate[id, "agg.events"]=length(which(agg_data$agonism_winner == unqIDs[id] |
             agg_data$agonism_loser == unqIDs[id]))
  }
  SocialCapitalData$agg.rate = aggressionRate$agg.events[match(SocialCapitalData$id, aggressionRate$id)]/SocialCapitalData$hrs.focalfollowed
  
  ###################################################################
  # Merge and save data
  SocialCapital.ALL = rbind(SocialCapital.ALL, SocialCapitalData)
}
SocialCapital.ALL$isPost =0; SocialCapital.ALL$isPost[SocialCapital.ALL$year>2017]=1; 
SocialCapital.ALL$isPost.year =0; SocialCapital.ALL$isPost[SocialCapital.ALL$years==2019]=1; SocialCapital.ALL$isPost[SocialCapital.ALL$years==2021]=2;
SocialCapital.ALL$isPost=as.factor(SocialCapital.ALL$isPost)

#Test change in aggression rates
ggplot(SocialCapital.ALL, aes(x=isPost, y=agg.rate, fill=isPost))+
  geom_violin()+ theme_classic(base_size=15)+
  ylab('Aggression rate')+ xlab('')
mean(SocialCapital.ALL$agg.rate[SocialCapital.ALL$isPost==0]); mean(SocialCapital.ALL$agg.rate[SocialCapital.ALL$isPost==1])
agg.model<-glmer(agg.rate~ isPost + sex + ordinal.rank + (1|id), data = SocialCapital.ALL, family = poisson(link = "log"))
summary(agg.model)
car::Anova(agg.model)

#Test change in probability(proximity)
ggplot(SocialCapital.ALL, aes(x=isPost, y=prob.prox, fill=isPost))+
  geom_violin()+ theme_classic(base_size=15)+
  ylab('p(proximity)')+ xlab('')
mean(SocialCapital.ALL$prob.prox[SocialCapital.ALL$isPost==0]); mean(SocialCapital.ALL$prob.prox[SocialCapital.ALL$isPost==1])
prox.model<-lme4::lmer(prob.prox~ isPost + sex + (1|id), data = SocialCapital.ALL)
summary(prox.model)
car::Anova(agg.model)

ggplot(SocialCapital.ALL, aes(x=isPost, y=CSIprox, fill=isPost))+
  geom_violin()+ theme_classic(base_size=15)+
  ylab('Average #monkeys in proximity')+ xlab('')
mean(SocialCapital.ALL$CSIprox[SocialCapital.ALL$isPost==0]); mean(SocialCapital.ALL$CSIprox[SocialCapital.ALL$isPost==1])

#Test change in groom rate
ggplot(SocialCapital.ALL, aes(x=isPost, y=groom.rate, fill=isPost))+
  geom_violin()+ theme_classic(base_size=15)+
  ylab('Grooming rate')+ xlab('')
mean(SocialCapital.ALL$groom.rate[SocialCapital.ALL$isPost==0]); mean(SocialCapital.ALL$groom.rate[SocialCapital.ALL$isPost==1])
groom.model<-lme4::lmer(groom.rate~ isPost + sex + (1|id), data = SocialCapital.ALL)
summary(groom.model)
car::Anova(groom.model)

#Test change in number of grooming partners
ggplot(SocialCapital.ALL, aes(x=isPost, y=num.partners, fill=isPost))+
  geom_violin()+ theme_classic(base_size=15)+
  ylab('Number grooming partners')+ xlab('')
mean(SocialCapital.ALL$num.partners[SocialCapital.ALL$isPost==0]); mean(SocialCapital.ALL$num.partners[SocialCapital.ALL$isPost==1])
nump.model<-lmer(num.partners~ isPost + sex + (1|id), data = SocialCapital.ALL)
summary(nump.model)
car::Anova(nump.model)
