#grooming_modelling_LongTermChange_bigCayo.R

#Model grooming network with bison 
#Regression from imputed dataset to test the change in grooming over time
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
load("BisonGroomingFocal.RData")

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","S","V","F","V","TT","V","F")
years = c(2013, 2013, 2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2019, 2019,2021,2021,2022,2022,2022)
groupyears = c("F2013","KK2013","F2014","HH2014","F2015","V2015","R2015","KK2015",
               "R2016","V2016","F2016","HH2016","F2017","KK2017","V2017",
               "S2019","V2019","F2021","V2021","TT2022","V2022","F2022")


######################################
## STEP 1: CREATE IMPUTED DATASET ###
######################################

# Extract change in proximity from each draw to create imputed data set:
imputed.data=list(); imputed.data.prepost=list(); i=1; gy=1;
imputed.density=list()
  for (i in 1:num_iter){
    
    data.all<-data.frame(); density.test=data.frame(matrix(NA, nrow=num_iter)); names(density.test)="dens"
    density.all<-data.frame();
    for (gy in 1:length(groupyears)){
    
    data_path<-'~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/'
    data = read.csv(paste0(data_path,"Group",groupyears[gy],"_GroupByYear.txt")) #load meta data
    data=data[data$hrs.focalfollowed>0,]
    
    data$group = group[gy]
    data$year=years[gy]; 
    data$isPost = ifelse(years[gy]<2018, "pre","post")
    data$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
    data$id = as.factor(data$id)
    data$sex[data$sex=="FALSE"]="F"; data$sex = as.factor(data$sex);
    data$id.year = paste(data$id, data$year,sep='.')
    
    strength<-node_strength_all[[groupyears[gy]]][i,]
    degree<-node_degree_all[[groupyears[gy]]][i,]
    node.id<-node_ids[[groupyears[gy]]]
    data$groom.strength<-as.numeric(strength[match(data$id, node.id)])
    data$groom.degree<-as.numeric(degree[match(data$id, node.id)])
    data.final<-data[,c("id","sex","age","group","year","isPost","isPost.year", "id.year","groom.strength", "groom.degree")]
    
    data.all<-rbind(data.all, data.final)
    
    density.test$dens=as.numeric(density_all[[groupyears[gy]]])
    density.test$group=group[gy]
    density.test$year=years[gy]
    density.test$isPost = ifelse(years[gy]<2018, "pre","post")
    density.test$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
    
    density.all<-rbind(density.all, density.test)
  }
  
    density.all$std.dens <-(density.all$dens-mean(density.all$dens))/sd(density.all$dens)
    data.all$std.groom.strength = (data.all$groom.strength-mean(data.all$groom.strength))/sd(data.all$groom.strength)
    data.all$std.groom.degree = (data.all$groom.degree-mean(data.all$groom.degree))/sd(data.all$groom.degree)
    
    #Adjust levels
    data.all$group<-as.factor(data.all$group)
    data.all = data.all %>%
      mutate(isPost = fct_relevel(isPost,
                                  "pre", "post")) %>%
      mutate(isPost.year = fct_relevel(isPost.year,
                                       "pre", "post.2018","post.2019","post.2021"))
    
    density.all = density.all %>%
      mutate(isPost = fct_relevel(isPost,
                                  "pre", "post")) %>%
      mutate(isPost.year = fct_relevel(isPost.year,
                                       "pre", "post.2018","post.2019","post.2021"))
    
  
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
  
}

#Create imputed dataset to use in frequentist survival models
imp<-miceadds::datalist2mids(imputed.data)
imp.prepost<-miceadds::datalist2mids(imputed.data.prepost)

###########################################################
## STEP 2: Run downstream analyses such as regression  ###
###########################################################
#This step propagates the uncertainty from step 1 through subsequent analyses such as regressions

data = imputed.data[[1]] #use one iteration to perform model checks.

### Test the long-term effects of Hurricane Maria on grooming###

#Groom network density
mdl.groomPrePost.BC<-lmer( std.dens~ 1+ isPost.year + (1|group), data=density.all)
summary(mdl.groomPrePost.BC)
#performance::check_model(mdl.groomPrePost.BC)

#Groom strength
mdl.strength.groomPrePost.BC <- with(imp.prepost, lmer(std.groom.strength~ 1+ isPost.year +age+ sex +(1|group) + (1|id)))
summary(mice::pool(mdl.strength.groomPrePost.BC ))

# #check model performance
# mdl.strength.groomPrePost.iter<-lmer(std.groom.strength~ 1+ isPost.year +age+ sex +
#                                         (1|group) + (1|id),
#                                       data=data)
# performance::check_model(mdl.strength.groomPrePost.iter)
# performance::check_collinearity(mdl.strength.groomPrePost.iter)

#Groom degree
mdl.degree.groomPrePost.BC <- with(imp, lmer(std.groom.degree~ 1+ isPost.year + sex +age+
                                           (1|group) + (1|id)))
summary(mice::pool(mdl.degree.groomPrePost.BC))

# #check model performance
# mdl.degree.groomPrePost.iter<-lmer(std.groom.degree~ 1+ isPost.year + sex +
#                                     (1|group) + (1|id), data=data)
# performance::check_model(mdl.degree.groomPrePost.iter)
# performance::check_collinearity(mdl.degree.groomPrePost.iter)


setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
save(mdl.groomPrePost.BC, mdl.strength.groomPrePost.BC, mdl.degree.groomPrePost.BC, file = "GroomMdlOutput_PrePost_bigCayo.RData")
#load( "GroomMdlOutput_PrePost_bigCayo.RData")


