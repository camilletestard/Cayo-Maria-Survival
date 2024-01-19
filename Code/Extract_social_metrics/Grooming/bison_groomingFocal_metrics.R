#bison_groomingFocal_metrics.R

#Model grooming networks from focal follows with bison. 
#Note: duration model is not yet optimized, so we have to use count of grooming bouts instead.
#Since for 2018 data was only collected through scans, 
#we us grooming count data from focal follows for all years except 2018.

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
load(paste0(data_path,"grooming_data_Focals.RData"))

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
  
   #Set priors
  priors <- get_default_priors("count_conjugate")#define priors
  priors$edge <- "gamma(0.1,0.1)" # set less flat priors
  #prior_check(priors, "count_conjugate")

  #fit count model
  fit.el <- bison_model(
    (count | total_obseff) ~ dyad(ID1, ID2),
    data=el,
    model_type = "count_conjugate", #Count conjugate for grooming data
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
  for(i in 1:num_iter){
    weightedEL<-samples.post[,c(1:2,2+i)]
    weightedEL[weightedEL[,3]<edge_thresh,3]=0
    #weightedEL[which(el$weight==0),3]=0
    
    #Create adjacency matrix from edgelist
    adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
    data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
    #read adjacency matrix into igraph
    m=as.matrix(data) # coerces the data set as a matrix
    am.g=graph.adjacency(m,mode= "directed",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
    igraph_strength<-strength(am.g); #scaled.strength<- (igraph_strength - min(igraph_strength)) / (max(igraph_strength) - min(igraph_strength))
    igraph_degree<-degree(am.g); #scaled.degree<- (igraph_degree - min(igraph_degree)) / (max(igraph_degree) - min(igraph_degree))
    igraph_density<-edge_density(am.g);
    
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
save(node_ids, node_degree_all, node_strength_all,density_all,num_iter,edge_thresh, file = "BisonGroomingFocal.RData")

