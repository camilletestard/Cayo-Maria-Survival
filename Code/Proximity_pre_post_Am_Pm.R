setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")

load("proximity_data_AM.RData"); AM.data = edgelist.all; AM.data$weights = AM.data$count/AM.data$total_samples
load("proximity_data_PM.RData"); PM.data = edgelist.all; PM.data$weights = PM.data$count/PM.data$total_samples

mat=matrix(nrow=2,ncol=2)
AM.data$weights[is.na(AM.data$weights)]=0
mat[1,1]=mean(AM.data$weights[AM.data$isPost=="pre"])
mat[2,1]=mean(AM.data$weights[AM.data$isPost=="post"])

PM.data$weights[is.na(PM.data$weights)]=0
mat[1,2]=mean(PM.data$weights[PM.data$isPost=="pre"])
mat[2,2]=mean(PM.data$weights[PM.data$isPost=="post"])

plot(density(AM.data$weights[AM.data$isPost=="pre"]))


heatmap(mat)