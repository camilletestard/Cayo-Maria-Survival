#ReactionNorm_Survival.R

#Model proximity network with bison - get a distribution of possible networks from the observed data
#Regression from imputed dataset to test the change in proximity over time
#C. Testard October 2022

#Load libraries
#data wrangling
library(dplyr)
library(forcats)

#Modelling
library(lme4)
library(broom.mixed)

#For plotting
library(ggplot2)
library(sjPlot)

#Load data
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load('Survival_Adults_TimeVarying_allgroups.RData')   
load("BisonProximity.RData")

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V","F")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
groupyears = paste0(group,years)


######################################
## STEP 1: CREATE IMPUTED DATASSET ###
######################################

# Extract change in proximity from each draw to create imputed data set:
imputed.data=list(); imputed.data.prepost=list(); i=1; gy=1;
imputed.density=list()
for (i in 1:num_iter){
  
  data.all<-data.frame(); density.iter=data.frame(matrix(NA, nrow=num_iter)); names(density.iter)="dens"
  density.all<-data.frame(); gy=1
  
  for (gy in 1:length(groupyears)){
    
    #Extract survival data
    data.survival<-SurvivalData.ALL[,c("id","id.year","period","year","group","sex",
                              "Age_entry.days","Age_event.days",
                              "Survival","days.in.study","percofsex.dominanted")]
    data.survival$percofsex.dominanted=as.numeric(scale(data.survival$percofsex.dominanted))
    data.survival$id = as.factor(data.survival$id)
    data.survival$sex[data.survival$sex=="FALSE"]="F"; data.survival$sex = as.factor(data.survival$sex); 
    data.survival$group = as.factor(data.survival$group)
    data.survival$location = ifelse(data.survival$group=="V","small cayo","big cayo")
    data.survival$year=as.factor(data.survival$year)
    data.survival$period=as.factor(data.survival$period)
    data.survival$id.year = paste(data.survival$id, data.survival$year, sep='.')
    data.survival$days.in.study=as.numeric(data.survival$days.in.study)
    data.survival$Age_entry.years=as.numeric(data.survival$Age_entry.days)/365
    
    #Extract behavioral data
    data_path<-'~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/'
    data = read.csv(paste0(data_path,"Group",groupyears[gy],"_GroupByYear.txt")) #load meta data
    data = data[data$hrs.focalfollowed>0,]
    
    data$group = group[gy]
    data$year= years[gy]; 
    data$isPost = ifelse(years[gy]<2018, "pre","post")
    data$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
    data$id = as.factor(data$id)
    data$sex[data$sex=="FALSE"]="F"; data$sex = as.factor(data$sex);
    data$id.year = paste(data$id, data$year,sep='.')
    
    strength<-node_strength_all[[groupyears[gy]]][i,]
    degree<-node_degree_all[[groupyears[gy]]][i,]
    node.id<-node_ids[[groupyears[gy]]]
    data$prox.strength<-as.numeric(strength[match(data$id, node.id)])
    data$prox.degree<-as.numeric(degree[match(data$id, node.id)])
    
    data.merged<-merge(data,data.survival[,c("id.year","Age_entry.days","Age_event.days",
                                        "Survival","period")], by="id.year")
    
    data.final<-data.merged[,c("id","sex","age","group","year","isPost","isPost.year",
                               "id.year","prox.strength", "prox.degree", "Age_entry.days",
                               "Age_event.days","Survival","period")]
    
    data.all<-rbind(data.all, data.final)
    
    density.iter$dens=density_all[[groupyears[gy]]]
    density.iter$group=group[gy]
    density.iter$year=years[gy]
    density.iter$isPost = ifelse(years[gy]<2018, "pre","post")
    density.iter$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
    
    density.all<-rbind(density.all, density.iter)
  }
  
  #Standardize
  density.all$std.dens <-(density.all$dens-mean(density.all$dens))/sd(density.all$dens)
  data.all$std.prox.strength = (data.all$prox.strength-mean(data.all$prox.strength))/sd(data.all$prox.strength)
  data.all$std.prox.degree = (data.all$prox.degree-mean(data.all$prox.degree))/sd(data.all$prox.degree)
  
  #Create year factor
  data.all$year.factor = as.factor(data.all$year)
  
  #Adjust levels
  data.all$group<-as.factor(data.all$group)
  data.all = data.all %>%
    mutate(isPost = fct_relevel(isPost,
                                "pre", "post")) %>%
    mutate(isPost.year = fct_relevel(isPost.year,
                                     "pre", "post.2018","post.2019","post.2021", "post.2022"))
  #"post.2018","pre","post.2019","post.2021", "post.2022"))
  
  density.all = density.all %>%
    mutate(isPost = fct_relevel(isPost,
                                "pre", "post")) %>%
    mutate(isPost.year = fct_relevel(isPost.year,
                                     "pre", "post.2018","post.2019","post.2021", "post.2022"))
  #"post.2018","pre","post.2019","post.2021", "post.2022"))
  
  
  imputed.data[[i]]=data.all
  imputed.density[[i]]=density.all
  
  
  #If only consider IDs present both pre and post
  id.isPost = table(data.all$id, data.all$isPost)
  id.year = table(data.all$id, data.all$year); 
  id_pre = row.names(as.data.frame(which(id.isPost[,"pre"]>0))); id_post = row.names(as.data.frame(which(id.isPost[,"post"]>0)))
  id.PreAndPost = as.data.frame(row.names(as.data.frame(which(id.isPost[,"pre"]>0 & id.isPost[,"post"]>0)))); names(id.PreAndPost)="id"
  id.PreAndPost$group = data.all$group[match(id.PreAndPost$id, data.all$id)]
  table(id.PreAndPost$group)
  data.prepost = subset(data.all, id %in% id.PreAndPost$id)
  
  imputed.data.prepost[[i]]<-data.prepost
  
  print(i)
  
}

#Create imputed dataset to use in frequentist survival models
imp<-miceadds::datalist2mids(imputed.data)
imp.prepost<-miceadds::datalist2mids(imputed.data.prepost)

#Extract reaction norm by taking the random effect
data = imputed.data.prepost[[1]]
model<-lmer(std.prox.strength~ age+ sex +(1|group) + (1+ isPost|id), data=data)
random_effects <- ranef(model)
print(random_effects)

hist(random_effects[["id"]]$isPostpost)
mean((random_effects[["id"]]$isPostpost))

coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
        1 + std.node_strength + sex + #percofsex.dominanted+
        (1|group) + (1|id) + (1|period), data=data)
summary(strength.observed)


