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
data = SocialCapital.ALL
data$sex=toupper(data$sex)
length(which(data$Survival==1))/nrow(data)

data<-within(data,{
  sex<-factor(sex,labels=c("M","F"))
  mom.ordrank<-factor(mom.ordrank, labels=c("L","M","H"))
  group<-factor(group,labels=c("F","V","KK"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
  Age_entry.days<-as.numeric(Age_entry.days)
  Age_event.days<-as.numeric(Age_event.days)
  days.in.study<-as.numeric(days.in.study)
  year.prehurr<-as.factor(year.prehurr)
  id<-as.factor(id)
})
table(data$id, data$year.prehurr)

#### Run model #### 
fitsocial.numpartner<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~mom.num.partners+ mom.percentrank +sex +group.size +(1|year.prehurr)+(1|id),data=data) #Runs a cox PH model with age as the time scale.
#Note: group size should be stratified and rank should be ordinal to avoid breaking proportional hazard assumption
cz <- cox.zph(fitsocial.numpartner)
print(cz)
#Note: mother rank does not follow proportionality assumption!
summary(fitsocial.numpartner)

fitsocial.toppartner<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~mom.top.partner+ mom.percentrank +sex +group.size +(1|year.prehurr/id),data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial.toppartner)
print(cz)
summary(fitsocial.toppartner)

### Hazard ratio plots ###
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results')
png("HazardRatio_numP_infant.png", width=5.25,height=7.25,units="in",res=1200)
ggforest(fitsocial.numpartner, data = data)
dev.off()
# plot(data$Survival, data$std.num.partners)
# mean(data$std.num.partners[data$Survival==0]); mean(data$std.num.partners[data$Survival==1])
# sum(data$Survival[data$sex=='F']);sum(data$Survival[data$sex=='M'])

png("HazardRatio_topP_infant.png", width=5.25,height=7.25,units="in",res=1200)
ggforest(fitsocial.toppartner, data = data)
dev.off()

