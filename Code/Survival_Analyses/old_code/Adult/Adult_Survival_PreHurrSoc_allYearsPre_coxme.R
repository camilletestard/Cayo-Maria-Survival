#Adult_Survival_PreHurrSoc_allYearsPre.R
#This script runs survival models on adults to test whether pre-hurricane
#sociality predicts survival up to 4 years after hurricane Maria.
#Input: SocialCapital_Adults.RData
#Camille Testard Feb. 2022, adapted from Chloe Shergold code.

#Load libraries:
library(survival)
library(survminer)
library(coxme)

#############################################
# Pre-hurricane sociality 
#############################################

### Load data ###
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data')
load('SocialCapital_Adults.RData'); data = SocialCapital.ALL
length(which(data$Survival==1))/nrow(data)
Scale=365.25

### Format data ###

#Binarise sociality
data$std.partners.factor = "M"
data$std.partners.factor[data$std.num.partners<0.7] = "L"
data$std.partners.factor[data$std.num.partners>1.7] = "H"

# #Create interaction terms (important for later plotting using ggforest)
# data$sexF.numpartner <- as.numeric(data$sex) * data$num.partners
# data$sexF.partners.factor = NA
# data$sexF.partners.factor[data$sex.num.partners<1] = "L"
# data$sexF.partners.factor[data$sex.num.partners>1] = "H"
# data$sexF.toppartner <- as.numeric(data$sex) * data$top.partner

#Remove KK?
#data=data[data$group!="KK",]

#Format data
data<-within(data,{
  group<-as.factor(group)  ##Informs the model of the levels within the categorical covariates (helps identify where the differences are)
  sex<-as.factor(sex)
  Age_entry.days<-as.numeric(Age_entry.days)
  Age_event.days<-as.numeric(Age_event.days)
  days.in.study<-as.numeric(days.in.study)
  year.prehurr<-as.factor(year.prehurr)
  id<-as.factor(id)
})

### Run models ###

#Number of partners pre-hurricnae
fitsocial.numpartner<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~std.num.partners + group +sex +(1|id)+(1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
#fitsocial.numpartner<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~std.num.partners + group +sex ,data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial.numpartner) #Check model assumptions
print(cz) ; #ggcoxzph(cz)
summary(fitsocial.numpartner)

#probability of grooming pre-hurricane
fitsocial.probgroom<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~prob.groom + sex+ group +(1|id)+(1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial.probgroom) #Check model assumptions
print(cz) ; #ggcoxzph(cz)
summary(fitsocial.probgroom)

#strength of bond to top partner
fitsocial.toppartner<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~top.partner+ sex+ group +(1|id)+(1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial.toppartner)
print(cz)
summary(fitsocial.toppartner)

#Probability of proximity pre-hurricane
fitsocial.probprox<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~prob.prox + sex+ group +(1|id)+(1|year.prehurr),data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial.probprox) #Check model assumptions
print(cz) ; #ggcoxzph(cz)
summary(fitsocial.probprox)

#All factors combined
fitsocial.all<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~std.num.partners +top.partner + prob.prox + group +sex +(1|id),data=data) #Runs a cox PH model with age as the time scale.
#fitsocial.all<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~std.num.partners +top.partner + prob.prox + group +sex ,data=data) #Runs a cox PH model with age as the time scale.
cz <- cox.zph(fitsocial.all) #Check model assumptions
print(cz) ; #ggcoxzph(cz)
summary(fitsocial.all)


### Hazard ratio plots ###
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results')
png("HazardRatio_numP.png", width=5.25,height=7.25,units="in",res=1200)
ggforest(fitsocial.all, data = data)
dev.off()
# ggplot(data, aes(x=as.factor(Survival), y=std.num.partners))+
#   geom_violin()+
#   geom_boxplot()+ theme_classic(base_size = 20)+ 
#   ylab('#partners relative to group mean')+ xlab('Death status')
# mean(data$std.num.partners[data$Survival==0]); mean(data$std.num.partners[data$Survival==1])
# sum(data$Survival[data$sex=='F']);sum(data$Survival[data$sex=='M'])

png("HazardRatio_topP.png", width=5.25,height=7.25,units="in",res=1200)
ggforest(fitsocial.toppartner, data = data)
dev.off()

# ### Survival curves ###
# 
# # Plot the baseline survival function
# ggsurvplot(survfit(fitsocial.numpartner), color = "#2E9FDF",
#            ggtheme = theme_minimal(), data=data)
# 
# fit2=survfit(Surv(Age_entry.days, Age_event.days, Survival)~std.partners.factor,data=data)
# ggsurvplot(fit2,
#            data = data,
#            size = 1,                 # change line size
#            palette =
#              c("#E7B800", "#2E9FDF"),# custom color palettes
#            conf.int = TRUE,          # Add confidence interval
#            pval = TRUE,              # Add p-value
#            risk.table = FALSE,        # Add risk table
#            risk.table.col = "strata",# Risk table color by groups
#            legend.labs =
#              c("Below average #partners", "Above average #partners"),    # Change legend labels
#            risk.table.height = 0.25,# Useful to change when you have multiple groups
#            xlim=c(2500,8000),
#            xlab="Age in days",
#            break.time.by = 1000,
#            ggtheme = theme_bw()
# )
