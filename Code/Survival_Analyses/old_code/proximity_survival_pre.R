#proximity_survival_pre.R

#1. Creates imputed dataset combining demographic information and
#the distribution of possible proximity networks pre-hurricane
#generated with bison from the observed data
#2. Run survival models on imputed datasets, pooling uncertainty across each model

#C. Testard October 2022

#Load libraries
#Data wrangling
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

#Load demographic data and behavioral data from bison networks
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load("BisonProximity.RData")
load('Survival_Adults.RData') 

#Set group year list
group = c("F","V","KK",
          "V","F","F","KK","V")
years = c(2015,2015,2015,
          2016,2016,2017,2017,2017)
groupyears = paste0(group,years)

######################################
## STEP 1: Create imputed data set ###
######################################

# Extract change in proximity from each draw to create imputed data set:
imputed.data=list(); i=1; gy=1;
for (i in 1:num_iter){

  data<-SurvivalData[,c("id","year","group","sex",
                             "Age_entry.days","Age_event.days",
                             "Survival","days.in.study","percofsex.dominanted")]
  data$percofsex.dominanted=as.numeric(scale(data$percofsex.dominanted))
  #data$percentrank=scale(data$percentrank)
  data$id = as.factor(data$id)
  data$sex = as.factor(data$sex)
  data$group = as.factor(data$group)
  data$year=as.factor(data$year)
  data$id.year = paste(data$id, data$year,sep='.')
  data$days.in.study=as.numeric(data$days.in.study)
  data$Age_entry.years=as.numeric(data$Age_entry.days)/365
  #data$Survival=as.factor(data$Survival)
  
  proximity.all=data.frame()
  for (gy in 1:length(groupyears)){
    
    nodes = node_ids[[groupyears[gy]]]
    strengthID = node_strength_all[[groupyears[gy]]]
    degreeID = node_degree_all[[groupyears[gy]]]
    proximity = data.frame(id.year = paste(nodes,years[gy],sep='.'))
    proximity$node_strength = as.numeric(strengthID[i,])
    proximity$degree = as.numeric(degreeID[i,])
    proximity.all=rbind(proximity.all, proximity)
  }
  
  data.final<-merge(data, proximity.all)
  
  #Standardize for later interpretation of models
  data.final$std.node_strength = (data.final$node_strength-mean(data.final$node_strength))/sd(data.final$node_strength)
  data.final$std.degree = (data.final$degree-mean(data.final$degree))/sd(data.final$degree)
  
  sd.degree[i] = sd(data.final$degree)
  
  # allids_prepost = unique(data$id); 
  # unique_data <- data[match(allids_prepost, data$id),]
  # table(data$Survival); table(data$group); table(data$Survival,data$group)
  
  imputed.data[[i]]=data.final
}

#Create imputed dataset to use in frequentist models
imp<-miceadds::datalist2mids(imputed.data, progress=T)

################################################
## STEP 2: Run downstream survival analyses ###
################################################
#This step propagates the uncertainty from step 1 through 
#subsequent survival models

###########################################
# Run frequentist models with imputed data

status.prox.pre <- with(imp, coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
                                     1+ sex + percofsex.dominanted+
                                     (1|group) + (1|id) +(1|year) ))
mdl.pool.status<-summary(mice::pool(status.prox.pre))

degree.prox.pre <- with(imp, coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
                           1+ std.degree+ sex + percofsex.dominanted+
                           (1|group) + (1|id) +(1|year) ))
mdl.pool.degree<-summary(mice::pool(degree.prox.pre))

strength.prox.pre <- with(imp, coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
                                      1+ std.node_strength+ sex + percofsex.dominanted+
                                      (1|group) + (1|id) +(1|year)))
mdl.pool.strength<-summary(mice::pool(strength.prox.pre))

save(strength.prox.pre, degree.prox.pre, file = "ProxMdlOutput_PreHurr.RData")
# setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
#load("ProxMdlOutput_PreHurr.RData")
