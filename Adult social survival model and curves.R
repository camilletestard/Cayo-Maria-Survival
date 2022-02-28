###Final Cox PH model, social integration sole covariate###
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
load('SocialCapital_Adults.RData'); data = SocialCapital.ALL
length(which(data$Survival==1))/nrow(data)
library(survival)
library(survminer)
Scale=365.25


#Format data
data<-within(data,{
  sex<-factor(sex,labels=c("M","F"))
  ordrank<-factor(ordrank, labels=c("L","M","H"))
  group<-factor(group,labels=c("F","V","KK"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
  Age_entry.days<-as.numeric(Age_entry.days)
  Age_event.days<-as.numeric(Age_event.days)
  days.in.study<-as.numeric(days.in.study)
})

#Run model
fitsocial<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~std.num.partners*sex +percentrank +(1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
fitsocial<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~std.num.partners +(1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial)
print(cz)
ggcoxzph(cz)
summary(fitsocial)

fitsocial2<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~top.partner*sex+ percentrank+ (1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
summary(fitsocial2)

###Survival curve###
dataScurve<-read.csv(file.choose()) ##social survival curve data##
fit2=survfit(Surv(Age_entry.days., Age_event.days., Survival==2)~Social_ord,data=dataScurve)
ggsurvplot(fit2,
           data = dataScurve,
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           pval = FALSE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs =
             c("Below average social integration", "Above average social integration"),    # Change legend labels
           risk.table.height = 0.25,# Useful to change when you have multiple groups
           xlim=c(2500,8000),
           xlab="Age in days",
           break.time.by = 1000,
           ggtheme = theme_bw()
)
###Cumulative hazard and cumulative event####
datahigh<-read.csv(file.choose()) ##High social data##
datalow<-read.csv(file.choose()) ##Low social data##
fith<-survfit(Surv(Age_entry.days., Age_event.days., Survival)~Social_ord, data = datahigh)
fitl<-survfit(Surv(Age_entry.days., Age_event.days., Survival)~Social_ord, data = datalow)
splots<-list()
splots[[1]]<-ggsurvplot(fith, data = datahigh, fun = "event", xlab = "Age in days", xlim = c(2500, 8000), legend.labs = "High social integration", legend = "bottom")
splots[[2]]<-ggsurvplot(fitl, data = datalow, fun = "event", xlab = "Age in days", xlim = c(2500, 8000), legend.labs = "Low social integration", legend = "bottom")
splots[[3]]<-ggsurvplot(fith, data = datahigh, fun = "cumhaz", xlab = "Age in days", xlim = c(2500, 8000), legend.labs = "High social integration", legend = "bottom")
splots[[4]]<-ggsurvplot(fitl, data = datalow, fun = "cumhaz", xlab = "Age in days", xlim = c(2500, 8000), legend.labs = "Low social integration", legend = "bottom")
arrange_ggsurvplots(splots,print=TRUE, ncol=2, nrow=2)

#####################
load('SocialCapital_changeP_adults.RData')
#data2= full.data[full.data$iter==1,]
data2= full.data[full.data$iter==sample(max(full.data$iter),1),];
data2$num_obs=scale(data2$num_obs)
data2$num_obs=as.numeric(data2$num_obs)
length(which(data2$Survival==1))/nrow(data2)

fitsocial<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~dpSocial*sex +ordrank +num_obs +(1|year.prehurr),data=data2) #Runs a cox PH model with age as the time scale.
summary(fitsocial)
fitsocial<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~dpAcc*sex+num_obs+ (1|year.prehurr),data=data2) #Runs a cox PH model with age as the time scale.
summary(fitsocial)

cz <- cox.zph(fitsocial)
print(cz)
ggcoxzph(cz)



