#Get_SampleSizes.R

################################################################
# Get number of individuals & hours observed per year and group

setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')

# group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V","F")
# years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
#           2016,2016,2016,2016,2017,2017,2017,
#           2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
group = c("F","S","F","F","F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V")
years = c(2010,2011,2011,2012,2013,2013,2014,
          2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022)
groupyears = paste0(group, years)

gy=1
all_MetaData = data.frame(matrix(ncol=8)); names(all_MetaData)=c("id","sex","age","hrs.focalfollowed","group","year","num.scans","isPost")
mean_hours_obs = vector(); mean_scans=vector()
sd_hours_obs = vector(); sd_scans=vector()

for (gy in 1:length(groupyears)){ #for all group & years
  
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
  meta_data = meta_data[,c("id","sex","age","hrs.focalfollowed")]
  meta_data$group = group[gy]; meta_data$year=years[gy];
  
  if(years[gy]==2018){
    prox_data = read.csv(paste("Group",groupyears[gy],"_scansamples_FULL_CLEANED.txt", sep = ""))
    scans = as.data.frame(table(prox_data$subject.ID))
  }else{
    prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = "")) #Load scan data}
    scans = as.data.frame(table(prox_data$focal.monkey))
  }
  
  meta_data$num.scans = scans$Freq[match(meta_data$id, scans$Var1)];
  
  if(years[gy]<2018){ meta_data$isPost =0}else{meta_data$isPost = 1}
  
  mean_hours_obs[gy]=mean(meta_data$hrs.focalfollowed)
  sd_hours_obs[gy]=sd(meta_data$hrs.focalfollowed)
  mean_scans[gy]=mean(meta_data$num.scans)
  sd_scans[gy]=sd(meta_data$num.scans)
  all_MetaData = rbind(all_MetaData, meta_data)
  
  
}

#Total # IDs
length(unique(all_MetaData$id))-1

#Get mean and sd observation effort per id, per group and year
hrs_obs = data.frame(groupyears, mean_hours_obs, sd_hours_obs)
scans_obs = data.frame(groupyears, mean_scans, sd_scans)

#mean and sd observation effort across all groups and years
mean(all_MetaData$hrs.focalfollowed, na.rm=T); sd(all_MetaData$hrs.focalfollowed, na.rm=T)
mean(all_MetaData$num.scans, na.rm=T); sd(all_MetaData$num.scans, na.rm=T)

#mean and sd observation effort for all non-hurricane years
mean(all_MetaData$hrs.focalfollowed[all_MetaData$year!=2018], na.rm=T); sd(all_MetaData$hrs.focalfollowed[all_MetaData$year!=2018], na.rm=T)
mean(all_MetaData$num.scans[all_MetaData$year!=2018], na.rm=T); sd(all_MetaData$num.scans[all_MetaData$year!=2018], na.rm=T)

mean(all_MetaData$num.scans[all_MetaData$year==2018], na.rm=T); sd(all_MetaData$num.scans[all_MetaData$year==2018], na.rm=T)

#Number of individuals per group and year
data=table(all_MetaData$group, all_MetaData$year)
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')
write.csv(data, "Individuals_per_year.csv")

# #Number of *unique* individuals in each group and year
# data = subset(all_MetaData,!duplicated(all_MetaData$id))
# table(data$group)
# table(data$year)

#Number of years each individual was observed
data=table(all_MetaData$id)
mean(data); sd(data)




# id.isPost = table(all_MetaData$id, all_MetaData$isPost)
# id_pre = row.names(as.data.frame(which(id.isPost[,1]>0))); id_post = row.names(as.data.frame(which(id.isPost[,2]>0)))
# id.PreAndPost = as.data.frame(row.names(as.data.frame(which(id.isPost[,1]>0 & id.isPost[,2]>0)))); names(id.PreAndPost)="id"
# id.PreAndPost$group = all_MetaData$group[match(id.PreAndPost$id, all_MetaData$id)]
# table(id.PreAndPost$group)

########################3############################
#Extract sample size for behavior data

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load("BisonProximity.RData")

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022)
groupyears = c("F2013","KK2013","F2014","HH2014","F2015","V2015","R2015","KK2015",
               "R2016","V2016","F2016","HH2016","F2017","KK2017","V2017",
               "V2018","KK2018","S2019","V2019","F2021","V2021","TT2022","V2022")

# Extract change in proximity from each draw to create imputed data set:
i=1; gy=1;
data.all<-data.frame(); density.iter=data.frame(matrix(NA, nrow=num_iter)); names(density.iter)="dens"
density.all<-data.frame();

for (gy in 1:length(groupyears)){
  
  data_path<-'~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/'
  data = read.csv(paste0(data_path,"Group",groupyears[gy],"_GroupByYear.txt")) #load meta data
  
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
  data$prox.strength<-as.numeric(strength[match(data$id, node.id)])
  data$prox.degree<-as.numeric(degree[match(data$id, node.id)])
  data.final<-data[,c("id","sex","age","group","year","isPost","isPost.year", "id.year","prox.strength", "prox.degree")]
  
  data.all<-rbind(data.all, data.final)
  
  density.iter$dens=density_all[[groupyears[gy]]]
  density.iter$group=group[gy]
  density.iter$year=years[gy]
  density.iter$isPost = ifelse(years[gy]<2018, "pre","post")
  density.iter$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
  
  density.all<-rbind(density.all, density.iter)
}

length(unique(data.all$id))

# #############################################################################
#Get deaths considered for testing the survival effect of 
# pre- and post-hurricane social capital (using time-varying mixed effect survival model)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load('Survival_Adults_TimeVarying_allgroups.RData') 
load("BisonProximity.RData")

#Set group year list
group = c("V","V","V","V","KK","S","F","TT")
years = c(2018, 2019, 2021,2022, 2018, 2019, 2021,2022)
# group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V")
# years = c(2013, 2013,2014,2014,2015,2015,2015,2015,2016,2016,2016,2016,2017,2017,2017)
groupyears = paste0(group, years)

data<-SurvivalData.ALL[,c("id","id.year","period","year","group","sex",
                          "Age_entry.days","Age_event.days",
                          "Survival","days.in.study","percofsex.dominanted")]
data$percofsex.dominanted=as.numeric(scale(data$percofsex.dominanted))
data$id = as.factor(data$id)
data$sex[data$sex=="FALSE"]="F"; data$sex = as.factor(data$sex); 
data$group = as.factor(data$group)
data$year=as.factor(data$year)
data$period=as.factor(data$period)
data$id.year = paste(data$id, data$year,sep='.')
data$days.in.study=as.numeric(data$days.in.study)
data$Age_entry.years=as.numeric(data$Age_entry.days)/365
#data$Survival=as.factor(data$Survival)

proximity.all=data.frame(); i=1
for (gy in 1:length(groupyears)){
  
  nodes = node_ids[[groupyears[gy]]]
  strengthID = node_strength_all[[groupyears[gy]]]
  degreeID = node_degree_all[[groupyears[gy]]]
  proximity = data.frame(id.year = paste(nodes,years[gy],sep='.'))
  proximity$node_strength = as.numeric(strengthID[i,])
  proximity$degree = as.numeric(degreeID[i,])
  proximity.all=rbind(proximity.all, proximity)
}

data.final<-merge(data, proximity.all)

SampleSize = subset(data.final,!duplicated(data.final$id))
dead.animals = data.final[data.final$Survival==1,]; length(unique(dead.animals$id))


# #############################################################################
#Get deaths considered for testing the survival effect of post-hurricane social capital

#Load demographic data and behavioral data from bison networks
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load('Survival_Adults_TimeVarying_allgroups.RData') 
load("BisonProximity.RData")

#Set group year list
group = c("V","V","V","KK","S","F")
years = c(2018, 2019, 2021, 2018, 2019, 2021)
# group = c("KK","S","F")
# years = c(2018, 2019, 2021)
# group = c("V","V","V")
# years = c(2018, 2019, 2021)
groupyears = paste0(group,years)

i=1; gy=1;
data<-SurvivalData.ALL[,c("id","period","year","group","sex",
                          "Age_entry.days","Age_event.days",
                          "Survival","days.in.study")]
#data$percofsex.dominanted=scale(data$percofsex.dominanted)
#data$percentrank=scale(data$percentrank)
data$id = as.factor(data$id)
data$sex = as.factor(data$sex)
data$group = as.factor(data$group)
data$year=as.factor(data$year)
data$period=as.factor(data$period)
data$id.year = paste(data$id, data$year,sep='.')
data$days.in.study=as.numeric(data$days.in.study)
data$Age_entry.years=as.numeric(data$Age_entry.days)/365
#data$Survival=as.factor(data$Survival)

proximity.all=data.frame()
for (gy in 1:length(groupyears)){
  
  nodes = node_ids[[groupyears[gy]]]
  strengthID = node_strength_all[[groupyears[gy]]]
  degreeID = node_degree_all[[groupyears[gy]]]
  proximity = data.frame(id.year = paste(nodes,years[gy],sep='.'))
  proximity$node_strength = as.numeric(strengthID[i,])
  proximity$degree = as.numeric(degreeID[i,])
  proximity.all=rbind(proximity.all, proximity)
}

data.final<-merge(data, proximity.all)
length(unique(data.final$id))

dead.animals = data.final[data.final$Survival==1,]
length(unique(dead.animals$id))

  
# #############################################################################
#Get deaths considered for testing the survival effect of social change pre-to-post-hurricane

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load('ProxChange.RData')
load('Survival_Adults.RData') 
SurvivalData$groupyearspre = paste0(SurvivalData$group,SurvivalData$year.prehurr)

# Extract change in proximity from each draw to create imputed data set:
group = c("V","V","V","KK","KK")
years = c(2015, 2016, 2017, 2015, 2017)
groupyears = c("V2015","V2016","V2017","KK2015","KK2017")
i=1; gy=1;

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
  prox_change_strength = node_strength_change[[groupyears[gy]]]
  prox_change_degree = node_degree_change[[groupyears[gy]]]
  nodes.change = data.frame(id.year = paste(nodes,years[gy],sep='.'))
  nodes.change$d.strength = as.numeric(prox_change_strength[i,])
  nodes.change$d.degree = as.numeric(prox_change_degree[i,])
  nodes.change.all=rbind(nodes.change.all, nodes.change)
}

data.final<-merge(data, nodes.change.all)
SampleSize = subset(data.final,!duplicated(data.final$id))
sum(SampleSize$Survival)