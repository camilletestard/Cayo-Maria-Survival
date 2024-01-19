#bison_aggression_metrics.R

#Model aggression networks with bison. 
# 2018 data was only collected through scans, here we model 2018 aggression
#data with a binary model and all other years using a count model.

#C. Testard October 2022


#Load libraries
#bison package
library(bisonR)
library(dplyr)
library(brms)
library(igraph)

#Load data
data_path = "~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/"
load(paste0(data_path,"aggression_data.RData"))

edgelist.all$groupyear = paste0(edgelist.all$group, edgelist.all$year)

#Set group year list
group = c("F","V","KK","V","F","F","KK","V","V","KK","S","V","F","V")
years = c(2015,2015,2015,
          2016,2016,2017,2017,2017,
          2018, 2018,2019, 2019,2021,2021)
groupyears = c("F2015","V2015","KK2015",
               "V2016","F2016","F2017",
               "KK2017","V2017","V2018","KK2018",
               "S2019","V2019","F2021","V2021")

#Initialize lists
gy=1; node_ids=list();
node_strength_all = list(); density_all=list(); node_degree_all = list();

#Set parameters
num_iter = 100
edge_thresh=0.0001

for (gy in 1:length(groupyears)){
  
  ##############################################################
  ## STEP 1: Fit network for aggression data ###
  ##############################################################
  # This step fits an edge weight model using conjugate priors, 
  # capturing uncertainty in social network edges
  
  #Load observed aggression network
  el = edgelist.all[edgelist.all$groupyear==groupyears[gy],]
  el$weight = el$count/el$total_obs_time
  el$year=factor(el$year)
  unqids = unique(c(el$ID1, el$ID2))
  el$ID1<-factor(el$ID1, levels=unqids)
  el$ID2<-factor(el$ID2, levels=unqids)
  
  
  if (groupyears[gy]=="V2018"|groupyears[gy]=="KK2018"){ #For 2018 
    #Use binary model in bison (data = scans; at each sample was the monkey aggressive? Y or N)
    
    priors <- get_default_priors("binary_conjugate")#define priors
    priors$edge <- "beta(0.1,0.1)" # set less flat priors
    #prior_check(priors, "binary_conjugate")
    
    
    #Fit post-hurricane network with bison
    fit.el <- bison_model(
      (count | total_obs_time) ~ dyad(ID1, ID2),
      data=el,
      model_type = "binary_conjugate", #Count conjugate for aggression data 
      priors=priors,
      directed=T
    )
    #plot_predictions(fit.el, num_draws=20)
    
  }else{ #If any other year
    #Use count model in bison (data = focal follows. How many aggression events in X hours?)
    
  # priors <- get_default_priors("count")#define priors
  priors <- get_default_priors("count_conjugate")#define priors
  priors$edge <- "gamma(0.1,0.1)" # set less flat priors
  prior_check(priors, "count_conjugate")

  fit.el <- bison_model(
    (count | total_obs_time) ~ dyad(ID1, ID2),
    data=el,
    model_type = "count_conjugate", #Count conjugate for aggression data 
    priors=priors,
    directed=T
  )
    
  }
  #Check model fit
  #plot_predictions(fit.el, num_draws=20)

  # #Summary of edge weights and their credible intervals
  # summary(fit.el)
  # 
  # #Visualize network with uncertainty estimates
  # plot_network(fit.el, lwd=5)

  #Draw n edgelists from the posterior of the network
  samples.post<-draw_edgelist_samples(fit.el, num_draws=num_iter)
  samples.post[,c(3:ncol(samples.post))]<-plogis(as.matrix(samples.post[,c(3:ncol(samples.post))])) #Convert back from log scale
  #posterior.el[[groupyears[gy]]] <- samples.post #save
  
  #Extract node names
  nodes = names(fit.el$node_to_idx)
  
  #Draw network metrics from posterior (global, nodal and edge_weight)
  igraph_strength_all=data.frame(matrix(NA, ncol = length(nodes), nrow=0));
  igraph_degree_all=data.frame(matrix(NA, ncol = length(nodes), nrow=0)); i=1
  igraph_density_all=vector()
  for(i in 1:num_iter){
    weightedEL<-samples.post[,c(1:2,2+i)]
    weightedEL[is.na(weightedEL[,3]),3]=0
    weightedEL[weightedEL[,3]<edge_thresh,3]=0

    #Create adjacency matrix from edgelist
    adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
    data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
    #read adjacency matrix into igraph
    m=as.matrix(data) # coerces the data set as a matrix
    am.g=graph.adjacency(m,mode= "directed",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
    
    #Extract network metric
    igraph_strength<-strength(am.g); #scaled.strength<- (igraph_strength - min(igraph_strength)) / (max(igraph_strength) - min(igraph_strength))
    igraph_degree<-degree(am.g); #scaled.degree<- (igraph_degree - min(igraph_degree)) / (max(igraph_degree) - min(igraph_degree))
    igraph_density<-edge_density(am.g);
    
    #Save over all network instantiation
    igraph_strength_all=rbind(igraph_strength_all, igraph_strength)
    igraph_degree_all=rbind(igraph_degree_all, igraph_degree)
    igraph_density_all=rbind(igraph_density_all, igraph_density)
    
    print(i)
  }
  names(igraph_strength_all)=nodes; names(igraph_degree_all)=nodes; #Assign node id
  
  #Save
  node_ids[[groupyears[gy]]] <- nodes
  node_strength_all[[groupyears[gy]]] <- igraph_strength_all
  node_degree_all[[groupyears[gy]]] <- igraph_degree_all
  density_all[[groupyears[gy]]] <- igraph_density_all
  
  # #Draw network metrics from posterior (global, nodal and edge_weight)
  # node_ids[[groupyears[gy]]] <- names(fit.el$node_to_idx)
  # node_strength_all[[groupyears[gy]]] <- extract_metric(fit.el, "node_strength", num_draws =num_iter) #Output size: num_draws X num_nodes
  # density_all[[groupyears[gy]]] <-extract_metric(fit.el, "global_density", num_draws =num_iter)
  
  print(groupyears[gy])
  
}

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
save(node_ids, density_all, node_strength_all, num_iter, edge_thresh, file = "BisonAggression.RData")

