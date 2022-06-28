###Final Cox PH model, social integration sole covariate###
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
load('SocialCapital_Infants.RData')
library(survival)
library(survminer)
Scale=365.25
data = SocialCapital.ALL
data$sex=toupper(data$sex)
length(which(data$Survival==1))/nrow(data)


#Format data
data<-within(data,{
  sex<-factor(sex,levels=c("M","F"))
  ordrank<-factor(mom.ordrank, levels=c("L","M","H"))
  group<-factor(group,levels=c("F","V","KK"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
  Age_entry.days<-as.numeric(Age_entry.days)
  Age_event.days<-as.numeric(Age_event.days)
  days.in.study<-as.numeric(days.in.study)
})

#Run model
fitsocial<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~mom.std.num.partners+ mom.percentrank+sex +(1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial)
print(cz)
ggcoxzph(cz)
summary(fitsocial)

fitsocial2<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~mom.top.partner+ mom.percentrank +sex +(1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
summary(fitsocial2)
