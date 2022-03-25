#Adult_Survival_ChangePrePost_allYearsPre.R
#This script runs survival models on adults to test whether change in sociality
#pre-to-post hurricane predicts survival up to 4 years after hurricane Maria.
#Input: SocialCapital_changeP_Adults.RData
#Camille Testard Feb. 2022, adapted from Chloe Shergold code.

#Load libraries:
library(survival)
library(survminer)
library(coxme)
library(ggplot2)

#############################################
# Change in sociality pre-to-post hurricane
#############################################
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
load('SocialCapital_changeP_Adults.RData') #input dataframe generated previously.

#Initialize variables
fit.groom.hr=data.frame(matrix(ncol=5)); names(fit.groom.hr)=c("coef","exp(coef)","se(coef)","z","Pr(>|z|)")
fit.prox.hr=data.frame(matrix(ncol=5)); names(fit.prox.hr)=c("coef","exp(coef)","se(coef)","z","Pr(>|z|)")

i=1; max_iter = max(full.data$iter)
for (i in 1:max_iter){
  data2= full.data[full.data$iter==i,];
  
  # #Plot the raw data
  # setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results')
  # dpAcc.hist<-ggplot(data2,aes(x=dpAcc))+
  #   geom_histogram()+ theme_classic(base_size = 15)+
  #   geom_vline(xintercept = 0, linetype="dashed", color = "red",size=2)+
  #   xlab('Change in p(proximity) pre-to-post hurricane')
  # ggsave("dpAcc.hist.png")
  
  # dpSocial.hist<-ggplot(data2,aes(x=dpSocial))+
  #   geom_histogram()+ theme_classic(base_size = 15)+
  #   geom_vline(xintercept = 0, linetype="dashed", color = "red",size=2 )+
  #   xlab('Change in p(grooming) pre-to-post hurricane')
  # ggsave("dpSocial.hist.png")
  # mean(data2$dpSocial)
  # 
  # ggplot(data2,aes(x=dpAcc, y=dpSocial))+
  #   geom_point()+ xlab('Change in p(proximity)')+ ylab('Change in p(grooming)')+
  #   geom_smooth(method='lm',formula= y~x)+ theme_classic(base_size = 15)
  
  #Format the data
  data2<-within(data2,{
    sex<-factor(sex,labels=c("M","F"))
    ordrank<-factor(ordrank, labels=c("L","M","H"))
    group<-factor(group,labels=c("V","KK"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
    Age_entry.days<-as.numeric(Age_entry.days)
    Age_event.days<-as.numeric(Age_event.days)
    days.in.study<-as.numeric(days.in.study)
    num_obs<-scale(as.numeric(num_obs))
    #year.prehurr<-as.factor(year.prehurr)
  })
  length(which(data2$Survival==1))/nrow(data2)
  data2$sexF.dpSocial <- as.numeric(data2$sex) * data2$dpSocial
  data2$sexF.dpAcc <- as.numeric(data2$sex) * data2$dpAcc
  
  
  #Fit the survival models
  fitsocial.groom<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~dpSocial*sex+percentrank+ num_obs+ year.prehurr,data=data2) #Runs a cox PH model with age as the time scale.
  summary(fitsocial.groom)
  cz <- cox.zph(fitsocial.groom)
  #print(cz)
  
  fitsocial.prox<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~dpAcc*sex +num_obs+ year.prehurr,data=data2) #Runs a cox PH model with age as the time scale.
  summary(fitsocial.prox)
  cz <- cox.zph(fitsocial.prox)
  #print(cz)
  
  data2$death.status="Alive"; data2$death.status[data2$Survival==1]="Dead"
  ggplot(data2, aes(x=death.status, y=dpAcc))+
    geom_violin()+
    geom_boxplot(width=0.5)+ theme_classic(base_size = 20)+
    ylab('Change in proximity pre-to-post hurricane')+ xlab('')
  mean(data2$dpAcc[data2$Survival==0]); mean(data2$dpAcc[data2$Survival==1])
  sum(data2$Survival[data2$sex=='F']);sum(data2$Survival[data2$sex=='M'])

  
  # #IMPORTANT NOTE: rank doesn't follow the proportional assumption.
  # #Running models without rank to check if results change
  # fitsocial.groom.norank<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~dpSocial*sex +year.prehurr+num_obs,data=data2) #Runs a cox PH model with age as the time scale.
  # summary(fitsocial.groom.norank)
  # cz <- cox.zph(fitsocial.groom.norank)
  # print(cz)
  # 
  # fitsocial.prox.norank<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~dpAcc*sex +year.prehurr+num_obs,data=data2) #Runs a cox PH model with age as the time scale.
  # summary(fitsocial.prox.norank)
  # cz <- cox.zph(fitsocial.prox.norank)
  # print(cz)
  # #Results don't change.
  
  # ### Hazard ratio plots ###
  # setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results')
  # png("HazardRatio_dprox.png", width=6.25,height=7.25,units="in",res=1200)
  # ggforest(fitsocial.prox, data = data2)
  # dev.off()
  # 
  # png("HazardRatio_dgroom.png", width=5.25,height=7.25,units="in",res=1200)
  # ggforest(fitsocial.groom, data = data2)
  # dev.off()
  # 
  fit.groom.hr = rbind(fit.groom.hr, summary(fitsocial.groom)$coefficients[1,])
  fit.prox.hr = rbind(fit.prox.hr, summary(fitsocial.prox)$coefficients[1,])
  
  #print(i)
  
}

hist(fit.prox.hr$`exp(coef)`, 50, xlab = "Hazard ratio for change in proximity", main = "Histogram of Hazard Ratios for change in proximity")
mean(fit.prox.hr$`exp(coef)`, na.rm=T)
hist(fit.prox.hr$`Pr(>|z|)`, 50, xlab = "p-value", main = "Histogram of p-values for change in proximity")
length(which(fit.prox.hr$`Pr(>|z|)`<0.05))/500

hist(fit.groom.hr$`Pr(>|z|)`, 50, xlab = "p-value",main = "Histogram of p-values for change in grooming")
length(which(fit.groom.hr$`Pr(>|z|)`<0.05))/500
