#bison_groomingScans_metrics.R

#Model grooming networks with bison. 
#Since for 2018 data was only collected through scans, 
#we us grooming binary data from scans for all years.

#C. Testard October 2022

#Load libraries
#bison package
library(bisonR)
library(dplyr)

#network analysis
library(igraph)

#Set seed for reproducibility
set.seed(1234)

#Load data
data_path = "~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/"
load(paste0(data_path,"grooming_data_scans.RData"))

edgelist.all$groupyear = paste0(edgelist.all$group, edgelist.all$year)

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V",
          "V","KK","S","V","F","V","TT","V","F")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
groupyears = paste0(group,years)

#Initialize lists
gy=1; node_strength_all = list(); node_degree_all = list(); density_all = list(); node_ids=list()
edge_thresh=0.0001

#Set parameters
num_iter = 100

for (gy in 1:length(groupyears)){ #for years
  
  ###########################################
  ## STEP 1: Fit network for binary data  ###
  ###########################################
  # This step fits an edge weight model using conjugate priors, 
  # capturing uncertainty in social network edges


  #Load observed network for specific group and year
  el = edgelist.all[edgelist.all$groupyear==groupyears[gy],]
  el$weight = el$count/el$total_obseff
  el$year=factor(el$year)
  unqids = unique(c(el$ID1, el$ID2))
  el$ID1<-factor(el$ID1, levels=unqids)
  el$ID2<-factor(el$ID2, levels=unqids)
  
  
  priors <- get_default_priors("binary_conjugate")#define priors
  priors$edge <- "beta(0.1,0.1)" # set less flat priors
  #prior_check(priors, "binary_conjugate")
  
  
  #Fit grooming network with bison
  fit.el <- bison_model(
    (count | total_obseff) ~ dyad(ID1, ID2),
    data=el,
    model_type = "binary_conjugate", #Binary conjugate for grooming captured in scan data 
    priors=priors,
    directed=T
  )
  #plot_predictions(fit.el, num_draws=20)
  
  #Check model fit
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
  
  for(i in 1:num_iter){#For all network instantiations
    weightedEL<-samples.post[,c(1:2,2+i)]#extract edge list from posterior
    weightedEL[weightedEL[,3]<edge_thresh,3]=0 #when edge strength is below a certain threshold,
                                               #consider that edge to be 0
    #weightedEL[which(el$weight==0),3]=0 #set observed 0 interaction to a real 0.
    
    #Create adjacency matrix from edgelist
    adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
    data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
    #read adjacency matrix into igraph
    m=as.matrix(data) # coerces the data set as a matrix
    am.g=graph.adjacency(m,mode= "directed",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
    
    #Extract metrics from network instantiation i
    igraph_strength<-strength(am.g); #node strength
    igraph_degree<-degree(am.g); #node degree
    igraph_density<-edge_density(am.g); #network density
    
    #Save over all network instantiations
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
  
  print(groupyears[gy])
  
}

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
save(node_ids, node_degree_all, node_strength_all, density_all, num_iter, edge_thresh, file = "BisonGroomingScans.RData")

