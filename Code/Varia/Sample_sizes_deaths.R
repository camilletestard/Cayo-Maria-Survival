library("VennDiagram")

#Load data
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/')
load("SocialCapital.ALL.RData")

#Get the number of years each individual is sampled
t<-table(SocialCapital.ALL$id, SocialCapital.ALL$year)
t[t>1]=1
numYears_perID = rowSums(t)
length(which(numYears_perID==1))
length(which(numYears_perID==2))
length(which(numYears_perID==3))
length(which(numYears_perID==4))
length(which(numYears_perID==5))
plot(density(numYears_perID))
mean(rowSums(t))
sd(rowSums(t))

#Find the number of individuals sampled pre- and post-hurricane
t2<-table(SocialCapital.ALL$id, SocialCapital.ALL$isPost)
t2[t2>1]=1
length(which(t2[,2]==1)) #pre-hurricane
length(which(t2[,1]==1)) #post-hurricane
length(which(t2[,2]==1 & t2[,1]==0)) #pre-hurricane only
length(which(t2[,2]==0 & t2[,1]==1))#post-hurricane only
length(which(t2[,2]==1 & t2[,1]==1)) #pre and post-hurricane


#Get number of years sampled pre-hurricane and post-hurricane for IDs sampled in both periods
id_pre_post = which(t2[,2]==1 & t2[,1]==1)
length(which(rowSums(t[id_pre_post,1:5])>1))
length(which(rowSums(t[id_pre_post,6:9])>1))


#Get death numbers: 
load("Survival_Adults_TimeVarying_allgroups.RData")
length(unique(SurvivalData.ALL$id))

t3<-table(SurvivalData.ALL$id, SurvivalData.ALL$YearOfDeath)
t3[t3>1]=1
Death_Per_Year = colSums(t3)
sum(Death_Per_Year[1:5]) #pre-hurricane
sum(Death_Per_Year[6:10])#post-hurricane

#Venn diagram of IDs pre 
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')
set1 = names(which(t2[,2]==1))
set2 = names(which(t2[,1]==1))
