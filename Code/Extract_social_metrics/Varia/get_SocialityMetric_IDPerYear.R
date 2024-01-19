#Get_SocialityMetric_IDPerYear.R
#This script generates 

library(stringr)
library(igraph)
library(lubridate)
library(hms)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V","F")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
groupyears = c("F2013","KK2013","F2014","HH2014","F2015","V2015","R2015","KK2015",
               "R2016","V2016","F2016","HH2016","F2017","KK2017","V2017",
               "V2018","KK2018","S2019","V2019","F2021","V2021","TT2022","V2022","F2022")
gy=1
SocialCapital.ALL = data.frame()
savePath = '~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/'

for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  
  if (years[gy]==2018) {
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    scans2018= read.csv(paste("Group",groupyears[gy],"_scansamples_FULL_CLEANED.csv", sep = ""))
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
    
    #Creat focal cutoff value
    
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
    scans2018$isProx=1; scans2018$isProx[which(scans2018$num.prox==0)]=0
    scans2018$isSocial=0; scans2018$isSocial[which(scans2018$focal.activity=="social")]=1
    scans2018$isSocialGive = 0; scans2018$isSocialGive[which(scans2018$focal.activity.isPost=="G")]=1
    scans2018$isSocialGet = 0; scans2018$isSocialGet[which(scans2018$focal.activity.isPost=="E")]=1
    scans2018$isAgg=0; scans2018$isAgg[which(scans2018$focal.activity=="aggression" | scans2018$focal.activity=="submit")]=1
    
    #Order columns
    col_order <- c("date","observation.name","focalID","group","year","scan.number","focal.activity","focal.activity.isPost","partner.ID","in.proximity","num.prox","isProx","isSocial","isSocialGive", "isSocialGet","isAgg", "Q","isPost","timeBlock")
    scans2018 <- scans2018[, col_order]
    
    #############################
    ### Get proximity metrics ###
    
    groom_data = scans2018[scans2018$focal.activity=="social",c("focalID","partner.ID")]
    prox_data =  scans2018[,c("focalID","in.proximity")]
    
    unqIDs = as.character(meta_data$id); social.data = data.frame(); id=1
    for (id in 1:length(unqIDs)){
      
      social.data[id, "id"]= unqIDs[id]
      scans = which(prox_data$focalID == unqIDs[id])
      social.data[id, "numscans"]=length(scans)
      
      #get number of unique partners in proximity
      prox.scans = prox_data[scans,]
      id.in.prox = str_split(prox.scans$in.proximity, c(","))
      unique.partners.prox = unique(unlist(c(id.in.prox)))
      unique.partners.prox =as.character(gsub("'",'',unique.partners.prox))
      if(length(which(unique.partners.prox =="N/A")!=0)){unique.partners.prox =unique.partners.prox[-which(unique.partners.prox =="N/A")]}
      if(length(which(is.na(unique.partners.prox))!=0)){unique.partners.prox =unique.partners.prox[-which(is.na(unique.partners.prox))]}
      
      social.data[id,"num.prox.partners"] = length(unique.partners.prox)
      social.data[id,"adj.num.prox.partners"] = social.data[id,"num.prox.partners"]/social.data[id, "numscans"] #NOT SURE HWO TO ADJUST FOR NUMBER OF PROXIMITY PARTNERS HERE
      social.data[id,"num.prox.events"] = length(which(!is.na(prox.scans$in.proximity)))
      
      # #Find the number of partners in proximity at each scan
      # for (i in 1:length(prox.scans)){ #for all scans of id
      #   numProxPartners = length(which(prox_partners[scans[i],2:length(prox_partners)] != "")) #find the number of partners at that scan
      #   proxRate[id, "numPartners"] = proxRate[id, "numPartners"] + numProxPartners #add #partners through scans
      #   if (numProxPartners>0){proxRate[id, "is.in.prox"]=proxRate[id, "is.in.prox"]+1}
      # }
      
      groom.scans = groom_data[which(groom_data$focalID == unqIDs[id]),]
      id.groom = unique(str_trim(unlist(str_split(unique(groom.scans$partner.ID),c(",")))))
      if(length(which(id.groom =="N/A")!=0)){id.groom =id.groom[-which(id.groom =="N/A")]}
      if(length(which(is.na(id.groom))!=0)){unique.partners.prox =unique.partners.prox[-which(is.na(id.groom))]}
      
      social.data[id,"num.groom.partners"] = length(id.groom)
      social.data[id,"adj.num.groom.partners"] = social.data[id,"num.groom.partners"]/social.data[id, "numscans"]
      social.data[id,"num.groom.events"] = nrow(groom.scans)
      
      # plot(social.data$num.groom.partners, social.data$num.prox.partners)
      # plot(social.data$numscans, social.data$num.prox.partners)
      # plot(social.data$numscans, social.data$num.groom.partners)
      
    }
    
    SocialCapitalData[,c("numPartnersGroom","top.partner.DSI","groom.events.focal","groom.dur","groom.rate")] = NA
    SocialCapitalData$numScans = social.data$numscans
    SocialCapitalData$numPartnersProx = social.data$num.prox.partners
    SocialCapitalData$prox.events = social.data$num.prox.events
    SocialCapitalData$prob.prox = social.data$num.prox.events/social.data$numscans
    SocialCapitalData$prox.rate = social.data$proxRate
    SocialCapitalData$numPartnersGroom= social.data$num.groom.partners
    SocialCapitalData$groom.events.scans= social.data$num.groom.events
    SocialCapitalData$prob.groom =  SocialCapitalData$groom.events.scans/SocialCapitalData$numScans

    #############################
    ### Get aggression metrics ###
    
    agg_data = scans2018[scans2018$focal.activity=="social",c("focalID","partner.ID")]
    aggRate = data.frame()
    for (id in 1:nrow(SocialCapitalData)){
      aggRate[id, 'id'] = SocialCapitalData$id[id]
      scans_id = scans2018[scans2018$focalID == SocialCapitalData$id[id],]
      aggRate[id, 'agg.events'] = sum(scans_id$isAgg)
      aggRate[id, "agg.partners"]= length(unique(scans_id$partner.ID[scans_id$isAgg==1]))
    }
    SocialCapitalData$agg.events = aggRate$agg.events[match(SocialCapitalData$id,aggRate$id)]
    SocialCapitalData$agg.rate = SocialCapitalData$agg.events/SocialCapitalData$hrs.focalfollowed
    SocialCapitalData$numPartnersAgg = aggRate$agg.partners
   
  ############################################################################
  }else{ #if not 2018 (i.e. regular focal data)
  ############################################################################ 
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    groom_data = read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt", sep = ""))
    agg_data = read.csv(paste("Group",groupyears[gy],"_AgonisticActions.txt", sep = ""))
    focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = ""))
    prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = ""))
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
    meta_data=meta_data[meta_data$hrs.focalfollowed>0,]
    #cleaned_data = read.csv(paste("Group",groupyears[gy],"_CleanedData.txt", sep = ""))
    
    #Set NA focal activity in scans to "Rest"
    prox_data$focal.activity[is.na(prox_data$focal.activity)]="rest"
    
    SocialCapitalData= meta_data[,c("id","sex","age","ordinal.rank","percofsex.dominanted","hrs.focalfollowed","focalcutoff_met")]
    SocialCapitalData$group = group[gy]
    SocialCapitalData$year = years[gy]
    
    if (group[gy]=="HH"){
      #Add HH dominance for subadults. 
      hh.dominance <- read.csv("HH_Dominance.csv");names(hh.dominance)[1]="id"
      hh.dominance$id = as.character(hh.dominance$id)
      hh.dominance$id[hh.dominance$id=="2.00E+09"]="2E9"
      hh.dominance$id[hh.dominance$id=="2.00E+08"]="2E8"
      hh.dominance$id[hh.dominance$id=="7.00E+00"]="7E0"
      hh.dominance$id[hh.dominance$id=="7.00E+03"]="7E3"
      hh.dominance$id[hh.dominance$id=="8.00E+02"]="8E2"
      
      SocialCapitalData[,c("ordinal.rank","percofsex.dominanted")]=hh.dominance[match(SocialCapitalData$id, hh.dominance$id),c("ordinal.rank","percofsex.domianted")]
    }
    
    if (groupyears[gy]=="KK2017"){
      #Add HH dominance for juveniles. 
      kk.dominance <- read.csv("KK_dominance_withSubadults.csv");names(kk.dominance)[1]="id"
      
      SocialCapitalData[,c("ordinal.rank","percofsex.dominanted")]=kk.dominance[match(SocialCapitalData$id, kk.dominance$id),c("ordinal.rank","percofsex.domianted")]
    }
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
    
    #Extract groom metrics
    unqIDs = as.character(SocialCapitalData$id)
    id=1; social_integration= data.frame(); partner_strength=matrix(NA, nrow = length(unqIDs), ncol=20); partner_DSI=matrix(NA, nrow = length(unqIDs), ncol=20)
    for (id in 1:length(unqIDs)){
      social_integration[id,"id"] = unqIDs[id];
      
      #Find the number of groom events
      if(groupyears[gy]!="F2021" & groupyears[gy]!="V2021"){
      social_integration[id,"groom.events.scans"] = sum(prox_data$focal.activity[prox_data$focal.monkey == unqIDs[id]] == "social")
      }else{
        social_integration[id,"groom.events.scans"] = sum(prox_data$focal.activity[prox_data$focal.monkey == unqIDs[id]] == "Groom Get"|
                                                            prox_data$focal.activity[prox_data$focal.monkey == unqIDs[id]] == "Groom Give")
      }
      
      #Get strength to top partner and number of grooming partners
      partners = unique(c(groom_data_compiled$receiver[groom_data_compiled$giver == unqIDs[id]],
                          groom_data_compiled$giver[groom_data_compiled$receiver == unqIDs[id]]))
      social_integration[id,"groom.rec"] = sum(groom_data_compiled$constrained_duration[groom_data_compiled$receiver == unqIDs[id]])
      social_integration[id,"groom.give"] = sum(groom_data_compiled$constrained_duration[groom_data_compiled$giver == unqIDs[id]])
      social_integration[id,"groom.events.focal"] = length(groom_data_compiled$constrained_duration[groom_data_compiled$receiver == unqIDs[id]])+
        length(groom_data_compiled$constrained_duration[groom_data_compiled$giver == unqIDs[id]])
      if (length(partners)!=0){
        for (p in 1:length(partners)){
          partner_strength[id,p]= groomMat[unqIDs[id], partners[p]] + groomMat[partners[p], unqIDs[id]]
          partner_DSI[id, p]= partner_strength[id,p]/sum(meta_data$hrs.focalfollowed[match(unqIDs[id], meta_data$id)], meta_data$hrs.focalfollowed[match(partners[p], meta_data$id)], na.rm = T)
        }
      }else{
        partner_DSI[id, 1]=0}
      social_integration[id,"numPartnersGroom"] = length(partners)
      social_integration[id,"top.partner"] = max(partner_DSI[id,], na.rm = T)
    }
    mean_groom_rate = mean(partner_DSI, na.rm = T)
    social_integration$top.partner.DSI=social_integration$top.partner/mean_groom_rate
    social_integration$std.numPartnersGroom = social_integration$numPartnersGroom/mean(social_integration$numPartnersGroom)
    social_integration$groom.dur = (social_integration$groom.rec + social_integration$groom.give)
    social_integration$groom.rate = social_integration$groom.dur/SocialCapitalData$hrs.focalfollowed[match(social_integration$id, SocialCapitalData$id)]
    
    SocialCapitalData[,c("numPartnersGroom","top.partner.DSI","groom.events.focal",
                         "groom.dur","groom.rate","groom.events.scans")] = 
      social_integration[,c("numPartnersGroom","top.partner.DSI","groom.events.focal",
                            "groom.dur","groom.rate","groom.events.scans")]
    
    #####################################################################
    ## Extract social integration measure from proximity data
    #Number of proximity partners
    #Probability of proximity? Proximity rate?
    #####################################################################
    
    if (groupyears[gy] == "V2019"){
      prox_data$in.proximity[!is.na(prox_data$partners.activity..sequential.)]=
        paste(prox_data$focal.monkey[!is.na(prox_data$partners.activity..sequential.)],
              prox_data$in.proximity[!is.na(prox_data$partners.activity..sequential.)],sep=",")
    }
    
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
      
    }
    
    proxRate$proxRate = proxRate$numPartners/proxRate$numScans #rate is the average number of partner per proximity scan
    proxRate$probProx = proxRate$is.in.prox/proxRate$numScans
    
    SocialCapitalData$numScans = proxRate$numScans
    SocialCapitalData$numPartnersProx = proxRate$num.prox.partners
    SocialCapitalData$prox.events = proxRate$is.in.prox
    SocialCapitalData$prob.prox = proxRate$probProx
    #SocialCapitalData$prox.rate = proxRate$proxRate
    SocialCapitalData$prob.groom =  SocialCapitalData$groom.events.scans/SocialCapitalData$numScans
    
    #####################################################################
    ## Extract aggression rate
    #####################################################################
    
    aggressionRate = data.frame()
    for (id in 1:length(unqIDs)){
      aggressionRate[id, "id"]= unqIDs[id]
      aggressionRate[id, "agg.events"]=length(which(agg_data$agonism_winner == unqIDs[id] |
                                                      agg_data$agonism_loser == unqIDs[id]))
      aggressionRate[id, "agg.partners"]= length(unique(c(agg_data$agonism_loser[which(agg_data$agonism_winner == unqIDs[id])], 
       agg_data$agonism_winner[which(agg_data$agonism_loser == unqIDs[id])])))
    }
    SocialCapitalData$agg.events = aggressionRate$agg.events[match(SocialCapitalData$id, aggressionRate$id)]
    SocialCapitalData$agg.rate = SocialCapitalData$agg.events/SocialCapitalData$hrs.focalfollowed
    SocialCapitalData$numPartnersAgg = aggressionRate$agg.partners
    
  } #end of year clause (2018 vs. other)
  
  col_order = c("id","sex","age","ordinal.rank","percofsex.dominanted",
                "hrs.focalfollowed","numScans","focalcutoff_met","group","year",     
                "groom.dur","groom.rate","numPartnersGroom","top.partner.DSI",
                "groom.events.focal","groom.events.scans","prob.groom",          
                "prox.events","prob.prox","numPartnersProx",
                "agg.events","agg.rate","numPartnersAgg")
  SocialCapitalData=SocialCapitalData[,col_order]
  ###################################################################
  # Merge and save data
  SocialCapital.ALL = rbind(SocialCapital.ALL, SocialCapitalData)
}
SocialCapital.ALL$isPost ="pre"; SocialCapital.ALL$isPost[SocialCapital.ALL$year>2017]="post"; 
SocialCapital.ALL$isPost.year ="pre"; SocialCapital.ALL$isPost.year[SocialCapital.ALL$year==2018]="post2018"; 
SocialCapital.ALL$isPost.year[SocialCapital.ALL$year==2019]="post2019"; SocialCapital.ALL$isPost.year[SocialCapital.ALL$year==2021]="post2021"; SocialCapital.ALL$isPost.year[SocialCapital.ALL$year==2022]="post2022";

SocialCapital.ALL.cutoffmet=SocialCapital.ALL[SocialCapital.ALL$focalcutoff_met=="Y",]
  
# indices=which(is.na(SocialCapital.ALL.cutoffmet),arr.ind = T)
# test=SocialCapital.ALL.cutoffmet[unique(indices[,1]),]

save(SocialCapital.ALL, file=paste(savePath, 'SocialCapital.ALL.RData', sep=""))

