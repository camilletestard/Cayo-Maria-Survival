#Model aggression network with bison 
#Regression from imputed dataset to test the change in aggression over time
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

#Load data
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load("BisonAggression_no2018.RData")


#Set group year list
group = c("F","KK","F","HH","F","R","KK",
          "R","F","HH","F","KK","S","F","TT","F")
years = c(2013, 2013,2014,2014,2015,2015,2015,
          2016,2016,2016,2017,2017,2019,2021,2022,2022)
groupyears = c("F2013","KK2013","F2014","HH2014","F2015","R2015","KK2015",
               "R2016","F2016","HH2016","F2017","KK2017",
               "S2019","F2021","TT2022","F2022")

######################################
## STEP 1: CREATE IMPUTED DATASET ###
######################################

# Extract proximity rate from each year, each draw to create imputed data set:
imputed.data=list(); imputed.data.prepost=list(); i=1; gy=1;
imputed.density=list()
  for (i in 1:num_iter){ #for each draw
    
    data.all<-data.frame(); density.test=data.frame(matrix(NA, nrow=num_iter)); names(density.test)="dens"
    density.all<-data.frame();
    
    for (gy in 1:length(groupyears)){ #for each group year
    
    data_path<-'~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/'
    data = read.csv(paste0(data_path,"Group",groupyears[gy],"_GroupByYear.txt")) #load meta data
    data=data[data$hrs.focalfollowed>0,]
    
    data$group = group[gy]
    data$year=years[gy]; 
    data$isPost = ifelse(years[gy]<2018, "pre","post")
    data$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
    data$id = as.factor(data$id)
    data$sex = as.factor(data$sex)
    data$id.year = paste(data$id, data$year,sep='.')
    
    strength<-node_strength_all[[groupyears[gy]]][i,]
    degree<-node_degree_all[[groupyears[gy]]][i,]
    node.id<-node_ids[[groupyears[gy]]]
    data$agg.strength<-as.numeric(strength[match(data$id, node.id)])
    data$agg.degree<-as.numeric(degree[match(data$id, node.id)])
    data.final<-data[,c("id","sex","age","group","year","isPost","isPost.year", "id.year","agg.strength","agg.degree")]
    
    data.all<-rbind(data.all, data.final)
    
    density.test$dens=density_all[[groupyears[gy]]]
    density.test$group=group[gy]
    density.test$year=years[gy]
    density.test$isPost = ifelse(years[gy]<2018, "pre","post")
    density.test$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
    
    density.all<-rbind(density.all, density.test)
  }
  
    #Adjust levels
    data.all$group<-as.factor(data.all$group)
    data.all = data.all %>%
      mutate(isPost = fct_relevel(isPost,
                                  "pre", "post")) %>%
      mutate(isPost.year = fct_relevel(isPost.year,
                                       "pre", "post.2019","post.2021","post.2022"))
    
    density.all = density.all %>%
      mutate(isPost = fct_relevel(isPost,
                                  "pre", "post")) %>%
      mutate(isPost.year = fct_relevel(isPost.year,
                                       "pre", "post.2019","post.2021","post.2022"))
    
    #Standardize for later interpretation of models
    data.all$std.agg.strength = (data.all$agg.strength-mean(data.all$agg.strength))/sd(data.all$agg.strength)
    data.all$std.agg.degree = (data.all$agg.degree-mean(data.all$agg.degree))/sd(data.all$agg.degree)
    density.all$std.dens <-(density.all$dens-mean(density.all$dens))/sd(density.all$dens)
    
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

###########################################################
## STEP 2: Run downstream analyses such as regression  ###
###########################################################
#This step propagates the uncertainty from step 1 through subsequent analyses such as regressions

data = imputed.data[[1]] #use one iteration to perform model checks.

### 1. Test the long-term effects of Hurricane Maria on aggression###

#1. Aggression network density
mdl.aggPrePost.BC<-lmer( std.dens~ 1+ isPost.year +(1|group) , data=density.all)
summary(mdl.aggPrePost.BC)
#performance::check_model(mdl.aggPrePost.BC)

#2. Individual level aggression strength
mdl.strength.aggPrePost.BC <- with(imp, lmer(std.agg.strength~ 1+ isPost.year + sex + age+
                                          +(1|group) + (1|id) ))
summary(mice::pool(mdl.strength.aggPrePost.BC))

# #check model performance
# mdl.strength.aggPrePost.iter<-lmer(std.agg.strength~ 1+ isPost.year +age+ sex +
#                                         (1|group) + (1|id), 
#                                       data=data)
# performance::check_model(mdl.strength.aggPrePost.iter)
# performance::check_collinearity(mdl.strength.aggPrePost.iter)

#3. Individual level aggression degree
mdl.degree.aggPrePost.BC <- with(imp, lmer(std.agg.degree~ 1+ isPost.year + sex +age+
                                            (1|group) + (1|id)))
summary(mice::pool(mdl.degree.aggPrePost.BC))

# # #check model performance
# mdl.degree.aggPrePost.iter<-lmer(std.agg.degree~ 1+ isPost.year + sex +
#                                     (1|group) + (1|id), data=data)
# performance::check_model(mdl.degree.aggPrePost.iter)
# performance::check_collinearity(mdl.degree.aggPrePost.iter)


save(mdl.aggPrePost.BC, mdl.strength.aggPrePost.BC, mdl.degree.aggPrePost.BC , file = "AggMdlOutput_PrePost_bigCayo.RData")

