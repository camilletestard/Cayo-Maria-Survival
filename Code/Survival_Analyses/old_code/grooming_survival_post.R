#grooming_survival_post.R

#1. Creates imputed dataset combining demographic information and
#the distribution of possible grooming networks post-hurricane
#generated with bison from the observed data
#2. Run survival models on imputed datasets, pooling uncertainty across each model

#C. Testard October 2022

#Load libraries
#bison package
library(dplyr)

#Survival analysis
library(survival)
library(survminer)
library(simsurv)
library(coxme)

#modelling
library(marginaleffects)

#For plotting
library(ggplot2)

#multiple imputation
library(mice)
library(broom.mixed)
library(eha)
library(ehahelper)

#Set seed for reproducibility
set.seed(1234)

#Load data
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load('BisonGrooming.RData')
load('Survival_Adults_TimeVarying.RData') 

#Set group year list
group = c("V","V","V","KK","S","F")
years = c(2018, 2019, 2021, 2018, 2019, 2021)
groupyears = paste0(group,years)

# Extract change in proximity from each draw to create imputed data set:
imputed.data=list(); i=1; gy=1;
for (i in 1:num_iter){
  
   data<-SurvivalData.ALL[,c("id","period","year","group","sex",
                             "Age_entry.days","Age_event.days",
                             "Survival","days.in.study")]
  #data$percofsex.dominanted=scale(data$percofsex.dominanted)
  #data$percentrank=scale(data$percentrank)
  data$id = as.factor(data$id)
  data$sex = as.factor(data$sex)
  data$group = as.factor(data$group)
  data$year=as.factor(data$year)
  data$period=as.factor(data$period)
  data$id.year = paste(data$id, data$year,sep='.')
  data$days.in.study=as.numeric(data$days.in.study)
  data$Age_entry.years=as.numeric(data$Age_entry.days)/365
  #data$Survival=as.factor(data$Survival)
  
  grooming.all=data.frame()
  for (gy in 1:length(groupyears)){
    
    nodes = node_ids[[groupyears[gy]]]
    strengthID = node_strength_all[[groupyears[gy]]]
    degreeID = node_degree_all[[groupyears[gy]]]
    grooming = data.frame(id.year = paste(nodes,years[gy],sep='.'))
    grooming$node_strength = as.numeric(strengthID[i,])
    grooming$degree = as.numeric(degreeID[i,])
    grooming.all=rbind(grooming.all, grooming)
  }
  
  data.final<-merge(data, grooming.all)
  
  #Make sure that we only consider the social data of the current (or preceding
  #year if there is no data in current year) to predict current survival
  data.final<-data.final[which(data.final$period==1 & data.final$year==2018|
                     data.final$period==2 & data.final$year==2019|
                     data.final$period==3 & data.final$year==2019|
                     data.final$period==4 & data.final$year==2021|
                     data.final$period==5 & data.final$year==2021),]
  
  #Standardize for later interpretation of models
  data.final$std.node_strength = (data.final$node_strength-mean(data.final$node_strength))/sd(data.final$node_strength)
  data.final$std.degree = (data.final$degree-mean(data.final$degree))/sd(data.final$degree)
  
  
  imputed.data[[i]]=data.final
}

#Create imputed dataset to use in frequentist models
imp<-miceadds::datalist2mids(imputed.data, progress=T)

################################################
## STEP 3: Run downstream survival analyses ###
################################################
#This step propagates the uncertainty from step 1 through 
#subsequent survival models
### Test survival effects of post hurricane proximity ###

###########################################
# Run frequentist models with imputed data

degree.groom.post <- with(imp, coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
                           1+ std.degree+ sex + 
                           (1|group) + (1|id) +(1|period)))
mdl.pool.degree<-summary(mice::pool(degree.groom.post))
est = mdl.pool.degree$estimate[1] 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)/(1+exp(est))
HR_CI = exp(CI)/(1+exp(CI))

strength.groom.post <- with(imp, coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
                                      1+ std.node_strength+ sex + 
                                      (1|group) + (1|id) +(1|period)))
mdl.pool.strength<-summary(mice::pool(strength.groom.post))
est = mdl.pool.strength$estimate[1] 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)/(1+exp(est))
HR_CI = exp(CI)/(1+exp(CI))

save(strength.groom.post, degree.groom.post, file = "GroomMdlOutput_PostHurr.RData")

#setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
#load("GroomMdlOutput_PostHurr.RData")



