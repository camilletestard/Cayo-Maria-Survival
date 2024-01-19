#bison_grooming_survival_prePost.R
#1. Model grooming networks from before and after the hurricane based on scan data 
#and extract change in grooming pre-to-post using bison.
#2. Create imputed dataset
#3. Run mixed effect survival models
#C. Testard October 2022

#Load libraries
#bison package
library(bisonR)
library(dplyr)
library(brms)

#Survival analysis
library(survival)
library(survminer)
library(simsurv)
library(coxme)
library(marginaleffects)

#For plotting
library(ggplot2)
library(igraph)

#multiple imputation
library(mice)
library(broom.mixed)
library(eha)
library(ehahelper)

#Set seed for reproducibility
set.seed(1234)

#Load data
data_path = "~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/"
load(paste0(data_path,"grooming_data.RData"))

edgelist.all$groupyear = paste0(edgelist.all$group, edgelist.all$year)

#Set group year list
group = c("V","V","V","KK","KK")
years = c(2015, 2016, 2017, 2015, 2017)
groupyears = c("V2015","V2016","V2017","KK2015","KK2017")

#Initialize lists
gy=1; node_strength_change = list(); node_degree_change = list(); node_ids=list()

#Set parameters
num_iter = 100
edge_thresh=0.0001

for (gy in 1:length(groupyears)){ #for years
  
  ##############################################################
  ## STEP 1: Fit network for binary data  (i.e., proximity) ###
  ##############################################################
  # This step fits an edge weight model using conjugate priors, 
  # capturing uncertainty in social network edges
  
  priors <- get_default_priors("binary_conjugate")#define priors
  priors$edge <- "beta(0.1,0.1)" # set less flat priors
  #prior_check(priors, "binary_conjugate")

  #Load pre-hurricane network
  el.pre = edgelist.all[edgelist.all$groupyear==groupyears[gy],]
  el.pre$year=factor(el.pre$year)
  
  #Load corresponding post-hurricane network
  el.post = edgelist.all[edgelist.all$group==group[gy] & edgelist.all$year==2018,]
  el.post$year=factor(el.post$year)
  
  
  #Fit pre-hurricane network with bison
  fit.el.pre <- bison_model(
    (count | total_obseff) ~ dyad(ID1, ID2),
    data=el.pre,
    model_type = "binary_conjugate", #Count conjugate for aggression data 
    priors=priors
  )
  #plot_predictions(fit.el.pre, num_draws=20)
  #plot_predictions(fit.el, num_draws=20)
  #Left plot: The predictions from the model are shown in blue and the real data
  #are shown in black. Ideally the blue lines should be distributed around the
  #black line, indicating the real data are among the possible predictions of the model.
  #Right plot: comparison of predictions against point estimates of edge weights
  #We're happy with current predictions
  
  
  #Fit post-hurricane network with bison
  fit.el.post <- bison_model(
    (count | total_obseff) ~ dyad(ID1, ID2),
    data=el.post,
    model_type = "binary_conjugate", #Count conjugate for aggression data 
    priors=priors
  )
  #plot_predictions(fit.el.post, num_draws=20)
  
  #Check model fit
  # #Summary of edge weights and their credible intervals
  # summary(fit.el)
  # 
  # #Visualize network with uncertainty estimates
  # plot_network(fit.el, lwd=5)

  #Draw n edgelists from the posterior of the network
  samples.post<-draw_edgelist_samples(fit.el.post, num_draws=num_iter)
  samples.post[,c(3:ncol(samples.post))]<-plogis(as.matrix(samples.post[,c(3:ncol(samples.post))])) #Convert back from log scale
  samples.pre<-draw_edgelist_samples(fit.el.pre, num_draws=num_iter)
  samples.pre[,c(3:ncol(samples.pre))]<-plogis(as.matrix(samples.pre[,c(3:ncol(samples.pre))])) #Convert back from log scale
  
  #Match nodes pre and post
  nodes.pre = names(fit.el.pre$node_to_idx)
  nodes.post = names(fit.el.post$node_to_idx)
  nodes.prepost = intersect(nodes.pre, nodes.post)
  
  strength_change_all=data.frame(matrix(NA, ncol = length(nodes.prepost), nrow=0)); 
  degree_change_all=data.frame(matrix(NA, ncol = length(nodes.prepost), nrow=0)); 
  i=1
  for(i in 1:num_iter){
    
    #For pre-network
    weightedEL<-samples.pre[,c(1:2,2+i)]
    weightedEL[weightedEL[,3]<edge_thresh,3]=0
    adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
    data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
    m=as.matrix(data) # coerces the data set as a matrix
    am.g=graph.adjacency(m,mode= "undirected",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
    igraph_strength_pre<-strength(am.g); igraph_strength_pre<-igraph_strength_pre[match(nodes.prepost,names(igraph_strength_pre))]
    igraph_degree_pre<-degree(am.g); igraph_degree_pre<-igraph_degree_pre[match(nodes.prepost,names(igraph_degree_pre))]
    
    
    #For post-network
    weightedEL<-samples.post[,c(1:2,2+i)]
    weightedEL[weightedEL[,3]<edge_thresh,3]=0
    adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
    data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
    m=as.matrix(data) # coerces the data set as a matrix
    am.g=graph.adjacency(m,mode= "undirected",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
    igraph_strength_post<-strength(am.g); igraph_strength_post<-igraph_strength_post[match(nodes.prepost,names(igraph_strength_post))]
    igraph_degree_post<-degree(am.g); igraph_degree_post<-igraph_degree_post[match(nodes.prepost,names(igraph_degree_post))]
    
    strength_change = igraph_strength_post-igraph_strength_pre
    degree_change = igraph_degree_post-igraph_degree_pre
    
    strength_change_all=rbind(strength_change_all, strength_change)
    degree_change_all=rbind(degree_change_all, degree_change)
    print(i)
  }
  names(degree_change_all)=nodes.prepost; names(strength_change_all)=nodes.prepost
  
 
  node_ids[[groupyears[gy]]] <- nodes.prepost
  node_strength_change[[groupyears[gy]]] <- strength_change_all
  node_degree_change[[groupyears[gy]]] <- degree_change_all
  
  print(groupyears[gy])
  
}

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
save(node_ids, node_strength_change, node_degree_change, file = "GroomChange.RData")


######################################
## STEP 2: Create imputed data set ###
######################################

#Load data with required info to run a survival model.
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load('GroomChange.RData')
load('Survival_Adults.RData') 
SurvivalData$groupyearspre = paste0(SurvivalData$group,SurvivalData$year.prehurr)

# Extract change in grooming from each draw to create imputed data set:
group = c("V","V","V","KK","KK")
years = c(2015, 2016, 2017, 2015, 2017)
groupyears = c("V2015","V2016","V2017","KK2015","KK2017")
imputed.data=list(); i=1; gy=1;num_iter = 20
for (i in 1:num_iter){
  
  data<-SurvivalData[,c("id","year","group","sex",
                             "Age_entry.days","Age_event.days",
                             "Survival","days.in.study")]
  # data$id = as.factor(data$id)
  data$sex = as.factor(data$sex)
  data$group = as.factor(data$group)
  data$year=as.factor(data$year)
  data$id.year = paste(data$id, data$year ,sep='.')
  data$days.in.study=as.numeric(data$days.in.study)
  data$Age_entry.years=as.numeric(data$Age_entry.days)/365
  #data$Survival=as.factor(data$Survival)
  
  nodes.change.all=data.frame()
  for (gy in 1:length(groupyears)){
    
    nodes = node_ids[[groupyears[gy]]]
    groom_change_strength = node_strength_change[[groupyears[gy]]]
    groom_change_degree = node_degree_change[[groupyears[gy]]]
    nodes.change = data.frame(id.year = paste(nodes,years[gy],sep='.'))
    nodes.change$d.strength = as.numeric(groom_change_strength[i,])
    nodes.change$d.degree = as.numeric(groom_change_degree[i,])
    nodes.change.all=rbind(nodes.change.all, nodes.change)
  }
  
  data.final<-merge(data, nodes.change.all)
  data.final<-data.final[data.final$group!="F",]
  data.final$group = droplevels(data.final$group)
  data.final$year = droplevels(data.final$year)
  data.final$id=as.numeric(as.factor(data.final$id))
  # data.final$id = droplevels(data.final$id)
  data.final$std.d.strength<-(data.final$d.strength-mean(data.final$d.strength))/sd(data.final$d.strength)
  data.final$std.d.degree<-(data.final$d.degree-mean(data.final$d.degree))/sd(data.final$d.degree)
  
  if(any(is.na(data.final)|data.final==Inf)){
    print("NAs or Inf here")
    print(i)
  }
  
  imputed.data[[i]]=data.final
}

imp<-miceadds::datalist2mids(imputed.data, progress=T)

################################################
## STEP 3: Run downstream survival analyses ###
################################################
#This step propagates the uncertainty from step 1 through 
#subsequent survival models

degree.dgroom <- with(imp, coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
                                          1+ std.d.degree + sex+
                                          (1|year)+(1|id)+(1|group)) )
mdl.pool.degree<-summary(mice::pool(degree.dgroom))
est = mdl.pool.degree$estimate[1]; print(est) 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975)); print(CI)
HR = exp(est); print(HR)
HR_CI = exp(CI); print(HR_CI)

strength.dgroom <- with(imp, coxme(Surv(Age_entry.days, Age_event.days, Survival)~ 
                                          1+ std.d.strength + sex+
                                          (1|year) +(1|id)+(1|group)))
mdl.pool.strength<-summary(mice::pool(strength.dgroom))
est = mdl.pool.strength$estimate[1]; print(est) 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975)); print(CI)
HR = exp(est); print(HR)
HR_CI = exp(CI); print(HR_CI)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
save(degree.dgroom,strength.dgroom, file = "MdlOutput_ChangeGrooming.RData")
