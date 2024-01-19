#Model proximity networks for Cayo data with bison and run downstream regression
#C. Testard October 2022

#Load libraries
#For plotting
library(ggplot2)
library(igraph)

#Load data
data_path = "~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/"
load(paste0(data_path,"proximity_data.RData"))

edgelist.all$groupyear = paste0(edgelist.all$group, edgelist.all$year)

#Set group year list
group = c("F","S","F","F","F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V")
years = c(2010,2011,2011,2012,2013,2013,2014,
          2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022)
groupyears = paste0(group,years)
gy=5
for (gy in 1:length(groupyears)){ #for all group years
  
#Load network edgelist
el = edgelist.all[edgelist.all$groupyear==groupyears[gy],]
el$year=factor(el$year)
el$weight = el$count/el$total_samples
weightedEL<-el[,c("ID1","ID2","weight")]

#Create adjacency matrix from edgelist
adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
#read adjacency matrix into igraph
m=as.matrix(data) # coerces the data set as a matrix
am.g=graph.adjacency(m,mode= "undirected",weighted=T)

l <- layout.fruchterman.reingold(am.g, repulserad=vcount(am.g)^5,
                                 area=vcount(am.g)^3)
#changes size of labels of vertices
V(am.g)$label.cex <- 0.2

V(am.g)$degree=igraph::degree(am.g)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/SocialNetworks/Proximity/")

pdf(paste0("ProximityNetwork ",groupyears[gy],".pdf"), 
     width=10, height=8)
plot.igraph(am.g, layout=layout.sphere,
            #vertex.label=V(am.g)$name, vertex.label.font=2,
            vertex.size=10,
            edge.color="grey20", 
            edge.width=E(am.g)$weight*20,edge.arrow.size = 0.5,edge.curved=0.5,
            main = groupyears[gy])
dev.off()

}