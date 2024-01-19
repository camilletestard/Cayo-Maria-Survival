#Estimate hours followed from # scans for group V and KK 2018
# Use linear interpolation to estimate # hours followed 
# from number of observations in the focals
# Camille Testard - 2021

library(ggplot2)
library(scales)

#Set path
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/") 
groupyears = c("V2015", "V2016", "V2017","KK2015", "KK2017")
gy=1; focal_obs_all=data.frame()

for (gy in 1:length(groupyears)){ #for all groups & years
  
  #load the data
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
  focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = "")) #load prox data from groupyear gy
  focal_obs = data.frame(table(focal_data$focal_id)); names(focal_obs)=c("id",'obs.in.focal')
  focal_obs$hrs.followed = meta_data$hrs.focalfollowed[match(as.character(focal_obs$id),meta_data$id)]
  focal_obs$groupyear = groupyears[gy]
  
  #plot the relationship between # hours followed and #observations in the focal
  # ggplot(focal_obs, aes(y=obs.in.focal, x=hrs.followed))+
  #   geom_point()+
  #   geom_smooth()+theme_classic(base_size = 15)
  
  focal_obs_all = rbind(focal_obs_all, focal_obs)
  
}
focal_obs_all$groupyear =as.factor(focal_obs_all$groupyear)

#plot the relationship between # hours followed and #observations in the focal
#for all group-years
ggplot(focal_obs_all, aes(x=obs.in.focal, y=hrs.followed))+
  geom_point()+
  #geom_point(aes(color=groupyear))+
  geom_smooth(method='lm')+theme_classic(base_size = 15)

#Get a linear fit
fit  <- lm(focal_obs_all$hrs.followed~focal_obs_all$obs.in.focal)
summary(fit)
intercept=fit$coefficients[[1]]; b=fit$coefficients[[2]];

#Interpolate and add to the group by year file stored locally
groupyears = c("V2018", "KK2018")
gy=1;
for (gy in 1:length(groupyears)){ #for all groups & years
  
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
  meta_data$hrs.focalfollowed = intercept + b*meta_data$numObs
  #meta_data$hrs.focalfollowed.v2 = 5*meta_data$numObs/3600
  
}

