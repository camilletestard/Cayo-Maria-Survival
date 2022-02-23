###Social network analysis and sociograms###
rm(list=ls())
library(igraph)
raw.data=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE) #Grooming matrix finished
m1=as.matrix(raw.data)
monkey.matrix=graph.adjacency(m1,mode="undirected",weighted="NULL")
monkey.matrix
library(sna)
library(tnet)
#Binary degree (inDegree)
Bindeg1<-apply(raw.data,2,function(a)sum(a>0))
Bindeg1
#binary outdegree
Boutdeg<-apply(raw.data,1,function(a)sum(a>0))
Boutdeg

###SOCIOGRAMS###
###Group F###
raw.dataF=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE) ##Group F sociogram file##
m1=as.matrix(raw.dataF)
monkey.matrix=graph.adjacency(m1,mode="undirected",weighted="NULL")
monkey.matrix
#Colour nodes by sex
#read attributes file for subjects
att=read.csv(file.choose()) ##Group F attribute file#
att

#link up attributes file with network
V(monkey.matrix)$Sex=as.character(att$Sex[match(V(monkey.matrix)$name,att$Animal_ID)])
V(monkey.matrix)$Sex

V(monkey.matrix)$Survival=as.character(att$Survival[match(V(monkey.matrix)$name,att$Animal_ID)])
V(monkey.matrix)$Survival

#set colour of sex
V(monkey.matrix)$color=V(monkey.matrix)$Survival #assign the "survival" attribute as the vertex colour
V(monkey.matrix)$color=gsub("0","palegreen3",V(monkey.matrix)$color) 
V(monkey.matrix)$color=gsub("1","red",V(monkey.matrix)$color) 

V(monkey.matrix)$shape=V(monkey.matrix)$Sex
V(monkey.matrix)$shape=gsub("1","circle",V(monkey.matrix)$shape)
V(monkey.matrix)$shape=gsub("2","square",V(monkey.matrix)$shape)


sociogram1 <- plot.igraph(monkey.matrix, vertex.size=6,
                          edge.width=1,
                          edge.color="black", edge.arrow.size = 0.5)
##Group KK##
raw.dataKK=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE) ##Group KK sociogram file##
m2=as.matrix(raw.dataKK)
monkey.matrixKK=graph.adjacency(m2,mode="undirected",weighted="NULL")
monkey.matrixKK

attKK=read.csv(file.choose())  ##Group KK attribute file##
attKK

#link up attributes file with network
V(monkey.matrixKK)$Sex=as.character(attKK$Sex[match(V(monkey.matrixKK)$name,attKK$Animal_ID)])
V(monkey.matrixKK)$Sex

V(monkey.matrixKK)$Survival=as.character(attKK$Survival[match(V(monkey.matrixKK)$name,attKK$Animal_ID)])
V(monkey.matrix)$Survival

#set colour of sex
V(monkey.matrixKK)$color=V(monkey.matrixKK)$Survival #assign the "survival" attribute as the vertex colour
V(monkey.matrixKK)$color=gsub("0","palegreen3",V(monkey.matrixKK)$color) 
V(monkey.matrixKK)$color=gsub("1","red",V(monkey.matrixKK)$color) 

V(monkey.matrixKK)$shape=V(monkey.matrixKK)$Sex
V(monkey.matrixKK)$shape=gsub("1","circle",V(monkey.matrixKK)$shape)
V(monkey.matrixKK)$shape=gsub("2","square",V(monkey.matrixKK)$shape)


sociogram2 <- plot.igraph(monkey.matrixKK, vertex.size=6,
                          edge.width=1,
                          edge.color="black", edge.arrow.size = 0.5)
###Group V###

raw.dataV=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE) ##Group V sociogram file##
m3=as.matrix(raw.dataV)
monkey.matrixV=graph.adjacency(m3,mode="undirected",weighted="NULL")
monkey.matrixV

attV=read.csv(file.choose()) ##Group V attribute file##
attV

#link up attributes file with network
V(monkey.matrixV)$Sex=as.character(attV$Sex[match(V(monkey.matrixV)$name,attV$Animal_ID)])
V(monkey.matrixV)$Sex

V(monkey.matrixV)$Survival=as.character(attV$Survival[match(V(monkey.matrixV)$name,attV$Animal_ID)])
V(monkey.matrixV)$Survival

#set colour of sex
V(monkey.matrixV)$color=V(monkey.matrixV)$Survival #assign the "survival" attribute as the vertex colour
V(monkey.matrixV)$color=gsub("0","palegreen3",V(monkey.matrixV)$color) 
V(monkey.matrixV)$color=gsub("1","red",V(monkey.matrixV)$color) 

V(monkey.matrixV)$shape=V(monkey.matrixV)$Sex
V(monkey.matrixV)$shape=gsub("1","circle",V(monkey.matrixV)$shape)
V(monkey.matrixV)$shape=gsub("2","square",V(monkey.matrixV)$shape)


sociogram3 <- plot.igraph(monkey.matrixV, vertex.size=6,
                          edge.width=1,
                          edge.color="black", edge.arrow.size = 0.5)

