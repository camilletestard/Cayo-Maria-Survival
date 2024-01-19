#Relationship between feeding and proximity.

group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V",
          "KK","V","S","V","F","V","V","F")
years = c(2013,2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,
          2017,2017,2017,
          2018, 2018,
          2019, 2019,2021,2021, 2022,2022)
groupyears = paste0(group, years)

gy=5; data.all=data.frame(); corr.feed.prox=vector(); pcorr.feed.prox=vector();
for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  if (years[gy]==2018) {
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    scans2018 = read.csv(paste("Group",groupyears[gy],"_scansamples_FULL_CLEANED.txt", sep = ""))
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
    meta_data$year = years[gy]
    meta_data$group = group[gy]
    
    scans2018$start.time=hms(scans2018$start.time)
    scans2018$am.pm = "am"
    scans2018$am.pm [scans2018$start.time>hms("11:00:00")]="pm"
    
    id=1
    for (id in 1:nrow(meta_data)){
      scans  = scans2018[scans2018$subject.ID == meta_data$id[id] &scans2018$am.pm=="am",]
      meta_data$nscans[id] = nrow(scans)
      meta_data$feedingTime[id] = length(which(scans$subject.activity=="F"))
      meta_data$feedingRate[id] = meta_data$feedingTime[id]/meta_data$nscans[id]
      
      meta_data$proxEvents[id] = length(which(scans$prox.adult.IDs !="N/A"))
      meta_data$pProx[id] = meta_data$proxEvents[id]/meta_data$nscans[id]
    }
    
    meta_data=meta_data[meta_data$nscans>0,]
    
    c_order = names(data.all)
    meta_data$std.feedingRate = scale(meta_data$feedingRate)
    meta_data=meta_data[,c_order]
    
  }else{ #pre-hurricane
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")); meta_data$idcode=NULL #load meta data
    meta_data = meta_data[meta_data$hrs.focalfollowed>0,]
    meta_data$year = years[gy]
    meta_data$group = group[gy]
    
    focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = "")) 
    proximity_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = ""))
    
    focal_data$am.pm = "am"
    focal_data$am.pm[hms(str_sub(focal_data$obs_start, 12, 19))>hms("11:00:00")]="pm"
    proximity_data$am.pm = "am"
    proximity_data$am.pm[hms(str_sub(proximity_data$time, 12, 19))>hms("11:00:00")]="pm"
    
    feeding_data = focal_data[focal_data$behaviour=="Feed" & focal_data$am.pm=="am",]
    proximity_data = proximity_data[proximity_data$am.pm=="am",]
    
    id=1
    for (id in 1:nrow(meta_data)){
      
      meta_data$nscans[id] = nrow(proximity_data[proximity_data$focal.monkey == meta_data$id[id],])
      
      meta_data$feedingTime[id] = sum(feeding_data$complete_duration[which(feeding_data$focal_id==meta_data$id[id])])
      meta_data$feedingRate[id] = meta_data$feedingTime[id]/meta_data$hrs.focalfollowed[id]
      
      meta_data$proxEvents[id] = length(which(!is.na(proximity_data$partners.activity..sequential.[proximity_data$focal.monkey == meta_data$id[id]])))
      meta_data$pProx[id] = meta_data$proxEvents[id]/meta_data$nscans[id]
    }
    
    meta_data$std.feedingRate = scale(meta_data$feedingRate)
  } # end of is post clause
  
  meta_data=meta_data[meta_data$nscans>0,]
  
  corr.feed.prox[gy] = cor(meta_data$feedingRate, meta_data$pProx)
  c=cor.test(meta_data$feedingRate, meta_data$pProx)
  pcorr.feed.prox[gy] = c$p.value
  
  data.all = rbind(data.all, meta_data)
  
}

corr.feed.prox[16:23]
pcorr.feed.prox[16:23]

corr.feed.prox[1:15]
pcorr.feed.prox[1:15]

data.post = data.all[data.all$year>2017,]
data.pre = data.all[data.all$year<2018,]

cor.test(data.post$std.feedingRate, data.post$pProx)
cor.test(data.pre$std.feedingRate, data.pre$pProx)

ggplot(data.post, aes(x=std.feedingRate, y=pProx))+
  geom_point(alpha=0.3)+
  geom_smooth(method = lm)+
  xlim(-4, 4)+
  xlab("Morning feeding rate (scaled)")+
  ylab("Morning p(proximity)")+
  #facet_grid(~year)+
  theme_light()

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Correl_AMprox_feeding.pdf")



library(lmerTest)
mdl<-lmer(std.feedingRate ~ pProx + (1|year) +(1|group), data=data.post)
summary(mdl)
