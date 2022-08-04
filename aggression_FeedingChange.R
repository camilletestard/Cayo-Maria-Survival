library(stringr)
library(igraph)
library(lubridate)
library(hms)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


group = c("F","KK","V","V")
years = c( 2017,2017,2017,2016)
groupyears = c("F2017","KK2017","V2017","V2016") 
gy=1
SocialCapital.ALL = data.frame()
savePath = '~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/'

for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  #Load data
  setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
  focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = ""))
  agg_data = read.csv(paste("Group",groupyears[gy],"_AgonisticActions.txt", sep = ""))
  
  focal_data$date = as.Date(focal_data$date)
  focal_data$feeding.isPost=0
  focal_data$feeding.isPost[focal_data$date>"2017-04-01"]=1
  table(focal_data$feeding.isPost)
  
  agg_data$date = as.Date(agg_data$date)
  agg_data$feeding.isPost=0
  agg_data$feeding.isPost[agg_data$date>"2017-04-01"]=1
  table(agg_data$feeding.isPost)
  
  focal_data_preFeedChange = focal_data[focal_data$feeding.isPost==0,]
  num_total_focals_pre = length(unique(focal_data_preFeedChange$observation_name))
  agg_data_preFeedChange = agg_data[agg_data$feeding.isPost==0,]
  num_total_agg_pre = nrow(agg_data_preFeedChange)
  
  focal_data_postFeedChange = focal_data[focal_data$feeding.isPost==1,]
  num_total_focals_post = length(unique(focal_data_postFeedChange$observation_name))
  agg_data_postFeedChange = agg_data[agg_data$feeding.isPost==1,]
  num_total_agg_post = nrow(agg_data_postFeedChange)
  
  print(num_total_agg_pre/num_total_focals_pre)
  print(num_total_agg_post/num_total_focals_post)
  
  # unqIDs = unique(meta_data$id)
  # aggressionRate = data.frame()
  # for (id in 1:length(unqIDs)){
  #   aggressionRate[id, "id"]= unqIDs[id]
  #   aggressionRate[id, "agg.events"]=length(which(agg_data$agonism_winner == unqIDs[id] |
  #                                                   agg_data$agonism_loser == unqIDs[id]))
  #   aggressionRate[id, "agg.partners"]= length(unique(c(agg_data$agonism_loser[which(agg_data$agonism_winner == unqIDs[id])], 
  #                                                       agg_data$agonism_winner[which(agg_data$agonism_loser == unqIDs[id])])))
  # }
  # SocialCapitalData$agg.events = aggressionRate$agg.events[match(SocialCapitalData$id, aggressionRate$id)]
  # SocialCapitalData$agg.rate = SocialCapitalData$agg.events/SocialCapitalData$hrs.focalfollowed
  # SocialCapitalData$numPartnersAgg = aggressionRate$agg.partners
  # 
  # data %>%
  #   ggplot(aes(x=isPost, y=agg.rate, fill=isPost))+
  #   geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  #   theme_classic(base_size=15)+
  #   ylab('Aggression rate')+ xlab('Hurricane Status')+ facet_grid(~ group)
}



