#bison_aggression_metrics.R

#Model aggression networks with bison. 
# 2018 data was only collected through scans, here we model 2018 aggression
#data with a binary model and all other years using a count model.

#C. Testard October 2022


#Load libraries
#bison package
library(bisonR)
library(dplyr)

#network analysis
library(igraph)

#Load data
data_path = "~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/"
load(paste0(data_path,"aggression_data.RData"))

edgelist.all$groupyear = paste0(edgelist.all$group, edgelist.all$year)

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","S","V","F","V","TT","V","F")
years = c(2013, 2013, 2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2019, 2019,2021,2021,2022,2022,2022)
groupyears = c("F2013","KK2013","F2014","HH2014","F2015","V2015","R2015","KK2015",
               "R2016","V2016","F2016","HH2016","F2017","KK2017","V2017",
               "S2019","V2019","F2021","V2021","TT2022","V2022","F2022")

#Initialize lists
gy=1; node_ids=list();
node_strength_all = list(); node_INstrength_all = list(); node_OUTstrength_all = list();
density_all=list(); node_degree_all = list();

#Set parameters
num_iter = 100
edge_thresh=0.0001

for (gy in 1:length(groupyears)){
  
  ##############################################################
  ## STEP 1: Fit network for aggression data ###
  ##############################################################
  # This step fits an edge weight model using count conjugate priors, 
  # capturing uncertainty in social network edges
  # Use count model in bison (data = focal follows. How many aggression events in X hours?)
  
  #Load observed aggression network
  el = edgelist.all[edgelist.all$groupyear==groupyears[gy],]
  el$weight = el$count/el$total_obs_time
  el$year=factor(el$year)
  unqids = unique(c(el$ID1, el$ID2))
  el$ID1<-factor(el$ID1, levels=unqids)
  el$ID2<-factor(el$ID2, levels=unqids)

  # priors <- get_default_priors("count")#define priors
  priors <- get_default_priors("count_conjugate")#define priors
  priors$edge <- "gamma(0.1,0.1)" # set less flat priors
  #prior_check(priors, "count_conjugate")

  fit.el <- bison_model(
    (count | total_obs_time) ~ dyad(ID1, ID2),
    data=el,
    model_type = "count_conjugate", #Count conjugate for aggression data 
    priors=priors,
    directed=T
  )
    
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
  igraph_INstrength_all=data.frame(matrix(NA, ncol = length(nodes), nrow=0));
  igraph_OUTstrength_all=data.frame(matrix(NA, ncol = length(nodes), nrow=0));
  igraph_degree_all=data.frame(matrix(NA, ncol = length(nodes), nrow=0)); i=1
  igraph_density_all=vector()
  
  for(i in 1:num_iter){
    weightedEL<-samples.post[,c(1:2,2+i)]
    weightedEL[weightedEL[,3]<edge_thresh,3]=0

    #Create adjacency matrix from edgelist
    adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
    data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
    #read adjacency matrix into igraph
    m=as.matrix(data) # coerces the data set as a matrix
    am.g=graph.adjacency(m,mode= "directed",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
    
    #Extract network metric
    igraph_INstrength<- strength(am.g, mode="in")
    igraph_OUTstrength<- strength(am.g, mode="out")
    igraph_strength<-strength(am.g); #node strength
    igraph_degree<-degree(am.g); #node degree
    igraph_density<-edge_density(am.g);#network density
    
    #Save over all network instantiation
    igraph_INstrength_all=rbind(igraph_INstrength_all, igraph_INstrength)
    igraph_OUTstrength_all=rbind(igraph_OUTstrength_all, igraph_OUTstrength)
    igraph_strength_all=rbind(igraph_strength_all, igraph_strength)
    igraph_degree_all=rbind(igraph_degree_all, igraph_degree)
    igraph_density_all=rbind(igraph_density_all, igraph_density)
    
    print(i)
  }
  names(igraph_strength_all)=nodes; 
  names(igraph_degree_all)=nodes; #Assign node id
  names(igraph_OUTstrength_all)=nodes; 
  names(igraph_INstrength_all)=nodes;
  
  #Save
  node_ids[[groupyears[gy]]] <- nodes
  node_INstrength_all[[groupyears[gy]]] <- igraph_INstrength_all
  node_OUTstrength_all[[groupyears[gy]]] <- igraph_OUTstrength_all
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
save(node_ids, density_all, node_INstrength_all, node_OUTstrength_all, node_strength_all, node_degree_all, num_iter, edge_thresh, file = "BisonAggression_no2018.RData")

