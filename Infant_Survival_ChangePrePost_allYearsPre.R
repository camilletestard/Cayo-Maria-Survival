#Infant_Survival_ChangePrePost_allYearsPre.R
#This script runs survival models on infants to test whether change in sociality
#pre-to-post hurricane predicts infant survival up to 4 years after hurricane Maria.
#Input: SocialCapital_changeP_Infants.RData
#Camille Testard Feb. 2022, adapted from Chloe Shergold code.

#Load libraries:
library(survival)
library(survminer)
library(coxme)

#############################################
# Change in sociality pre-to-post hurricane
#############################################
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
load('SocialCapital_changeP_infants.RData') #input dataframe generated previously.
#data=full.data

#Initialize variables
fit.groom.hr=data.frame(matrix(ncol=5)); names(fit.groom.hr)=c("coef","exp(coef)","se(coef)","z","Pr(>|z|)")
fit.prox.hr=data.frame(matrix(ncol=5)); names(fit.prox.hr)=c("coef","exp(coef)","se(coef)","z","Pr(>|z|)")

i=1
for (i in 1:max(full.data$iter)){
  data= full.data[full.data$iter==i,];
  
  data<-within(data,{
    sex<-factor(sex,labels=c("MALE","FEMALE"))
    group<-factor(group,labels=c("V","KK"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
    Age_entry.days<-as.numeric(Age_entry.days)
    Age_event.days<-as.numeric(Age_event.days)
    days.in.study<-as.numeric(days.in.study)
    num_obs<-scale(as.numeric(num_obs))
    mom.dpAcc<- 100*mom.dpAcc
    mom.dpSocial<- 100*mom.dpSocial
  })
  length(which(data$Survival==1))/nrow(data)
  
  fitsocial.groom<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~mom.dpSocial+ strata(sex) +mom.percentrank +year.prehurr+num_obs,data=data) #Runs a cox PH model with age as the time scale.
  summary(fitsocial.groom)
  cz <- cox.zph(fitsocial.groom) #Check model assumptions
  #print(cz)
  # Important note: sex does not follow the proportionality assumption. We therefore
  # fit a proportional hazard model with sex stratified (strata(sex))
  
  fitsocial.prox<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~mom.dpAcc+ strata(sex) +mom.percentrank+ num_obs + year.prehurr,data=data) #Runs a cox PH model with age as the time scale.
  summary(fitsocial.prox)
  cz <- cox.zph(fitsocial.prox)
  #print(cz)
  # Same as above for strata(sex)

  
  ### Hazard ratio plots ###
  # setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results')
  # png("HazardRatio_dprox_infant.png", width=5.25,height=7.25,units="in",res=1200)
  # ggforest(fitsocial.prox, data = data)
  # dev.off()
  # 
  # png("HazardRatio_dgroom_infant.png", width=5.25,height=7.25,units="in",res=1200)
  # ggforest(fitsocial.groom, data = data)
  # dev.off()
  
  fit.groom.hr = rbind(fit.groom.hr, summary(fitsocial.groom)$coefficients[1,])
  fit.prox.hr = rbind(fit.prox.hr, summary(fitsocial.prox)$coefficients[1,])
  print(i)
  
}

hist(fit.prox.hr$`exp(coef)`, 50, xlab = "Hazard ratio for change in proximity", main = "Histogram of Hazard Ratios for change in proximity")
mean(fit.prox.hr$`exp(coef)`, na.rm=T)
hist(fit.prox.hr$`Pr(>|z|)`, 50, xlab = "p-value", main = "Histogram of p-values for change in proximity")
length(which(fit.prox.hr$`Pr(>|z|)`<0.05))/500

hist(fit.groom.hr$`Pr(>|z|)`, 50, xlab = "p-value",main = "Histogram of p-values for change in grooming")
length(which(fit.groom.hr$`Pr(>|z|)`<0.05))/500
