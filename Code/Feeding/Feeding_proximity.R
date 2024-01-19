#Relationship between feeding and proximity.

group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V",
          "KK","V","S","V","F","V","V", "F")
years = c(2013,2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,
          2017,2017,2017,
          2018, 2018,
          2019, 2019,2021,2021, 2022, 2022)
groupyears = paste0(group, years)

gy=17; data.all=data.frame(); corr.feed.prox=vector(); pcorr.feed.prox=vector();
for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  if (years[gy]==2018) {
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    scans2018 = read.csv(paste("Group",groupyears[gy],"_scansamples_FULL_CLEANED.txt", sep = ""))
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
    meta_data$year = years[gy]
    meta_data$group = group[gy]
    
    id=1
    for (id in 1:nrow(meta_data)){
      scans  = scans2018[scans2018$subject.ID == meta_data$id[id],]
      meta_data$nscans[id] = nrow(scans)
      meta_data$feedingTime[id] = length(which(scans$subject.activity=="F"))
      meta_data$feedingRate[id] = meta_data$feedingTime[id]/meta_data$nscans[id]
      
      meta_data$proxEvents[id] = length(which(scans$prox.adult.IDs !="N/A"))
      meta_data$pProx[id] = meta_data$proxEvents[id]/meta_data$nscans[id]
    }
    
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
    feeding_data = focal_data[focal_data$behaviour=="Feed",]
    
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
  
  corr.feed.prox[gy] = cor(meta_data$feedingRate, meta_data$pProx)
  c=cor.test(meta_data$feedingRate, meta_data$pProx)
  pcorr.feed.prox[gy] = c$p.value
  
  data.all = rbind(data.all, meta_data)
  
}

data.post = data.all[data.all$year>2017,]
data.pre = data.all[data.all$year<2018,]

cor.test(data.post$std.feedingRate, data.post$pProx)
cor.test(data.pre$std.feedingRate, data.pre$pProx)

ggplot(data.post, aes(x=std.feedingRate, y=pProx, color = group))+
  geom_point()+
  geom_smooth(method = lm)+
  xlim(-2, 2)+
  facet_grid(~year)+
  theme_light()

mdl<-lmer(feedingRate ~ pProx + (1|year) +(1|group), data=data.all)

##################
## Test relationship between feeding rate and survival 

#Survival analysis 
library(survival)
library(survminer)
library(simsurv)
library(coxme)

#Set group year list
group = c("V","V","V","V","KK","S","F","TT","F")
years = c(2018, 2019, 2021,2022, 2018, 2019, 2021,2022,2022)
groupyears = paste0(group,years)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load('Survival_Adults_TimeVarying_allgroups.RData') 

data<-SurvivalData.ALL[,c("id","id.year","period","year","group","sex",
                          "Age_entry.days","Age_event.days",
                          "Survival","days.in.study","percofsex.dominanted")]
data$percofsex.dominanted=as.numeric(scale(data$percofsex.dominanted))
data$id = as.factor(data$id)
data$sex[data$sex=="FALSE"]="F"; data$sex = as.factor(data$sex); 
data$group = as.factor(data$group)
data$location = ifelse(data$group=="V","small cayo","big cayo")
data$isPost="pre"; data$isPost[data$year>2017]="post"
data$year=as.factor(data$year)
data$period=as.factor(data$period)
data$id.year = paste(data$id, data$year, data$group,sep='.')
data$days.in.study=as.numeric(data$days.in.study)
data$Age_entry.years=as.numeric(data$Age_entry.days)/365

data.feeding = data.post[,c("id","year","group","std.feedingRate", "pProx")]
data.feeding$id.year = paste(data.feeding$id, data.feeding$year, data.feeding$group,sep='.')

data.final<-merge(data, data.feeding[,c("id.year", "std.feedingRate", "pProx")], by="id.year")

ggplot(data.final, aes(x=as.factor(Survival), y=std.feedingRate))+
  geom_boxplot()
mean(data.final$std.feedingRate[data.final$Survival==1])
mean(data.final$std.feedingRate[data.final$Survival==0])

ggplot(data.final, aes(x=std.feedingRate, y=pProx, color=as.factor(Survival)))+
  geom_point(alpha=0.5)+
  #geom_smooth(method = lm)+
  theme_light()


mdl<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
        1+ std.feedingRate+ sex + 
        (1|group) + (1|id) +(1|period), data=data.final)
mdl.output<-summary(mdl)
est = mdl.output$coefficients[1] ; print(est)
se = mdl.output$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est); print(HR)
HR_CI = exp(CI); print(HR_CI)

