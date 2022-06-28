#Infant_Survival_PreHurrSoc_allYearsPre.R
#This script runs survival models on infants to test whether pre-hurricane
#sociality predicts survival up to 4 years after hurricane Maria.
#Input: SocialCapital_Infants.RData
#Camille Testard Feb. 2022, adapted from Chloe Shergold code.

#### Load libraries: #### 
library(survival)
library(survminer)

#### Load data #### 
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
load('SocialCapital_Infants.RData')

#### Format data #### 
Scale=365.25
data = SocialCapital.ALL[SocialCapital.ALL$year.prehurr==2017,]
data$sex=toupper(data$sex)
length(which(data$Survival==1))/nrow(data)

data<-within(data,{
  sex<-factor(sex,labels=c("M","F"))
  ordrank<-factor(mom.ordrank, labels=c("L","M","H"))
  group<-factor(group,labels=c("F","V","KK"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
  Age_entry.days<-as.numeric(Age_entry.days)
  Age_event.days<-as.numeric(Age_event.days)
  days.in.study<-as.numeric(days.in.study)
})

#### Run model #### 
fitsocial.numpartner<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~mom.num.partners+ mom.ordrank +sex +group.size,data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial.numpartner)
print(cz)
#Note: mother rank &num partner does not follow proportionality assumption!
summary(fitsocial.numpartner)

fitsocial.toppartner<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~mom.top.partner+ mom.ordrank +sex ,data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial.toppartner)
print(cz)
summary(fitsocial.toppartner)

### Hazard ratio plots ###
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results')
png("HazardRatio_numP_infant_only2017.png", width=5.25,height=7.25,units="in",res=1200)
ggforest(fitsocial.numpartner, data = data)
dev.off()
# plot(data$Survival, data$std.num.partners)
# mean(data$std.num.partners[data$Survival==0]); mean(data$std.num.partners[data$Survival==1])
# sum(data$Survival[data$sex=='F']);sum(data$Survival[data$sex=='M'])

png("HazardRatio_topP_infant_only2017.png", width=5.25,height=7.25,units="in",res=1200)
ggforest(fitsocial.toppartner, data = data)
dev.off()

