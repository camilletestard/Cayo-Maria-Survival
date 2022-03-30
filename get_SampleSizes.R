#Get_SampleSizes.R

setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data All Cleaned/BehavioralDataFiles')

group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","S","V","F","V")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2019, 2019,2021,2021)
groupyears = c("F2013","KK2013","F2014","HH2014","F2015","V2015","R2015","KK2015",
               "R2016","V2016","F2016","HH2016","F2017","KK2017","V2017",
               "S2019","V2019","F2021","V2021") 
gy=1
all_MetaData = data.frame(matrix(ncol=8)); names(all_MetaData)=c("id","sex","age","hrs.focalfollowed","group","year","num.scans","isPost")

for (gy in 1:length(groupyears)){ #for all group & years
  
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")) #load meta data
  meta_data = meta_data[meta_data$hrs.focalfollowed>1,c("id","sex","age","hrs.focalfollowed")]
  meta_data$group = group[gy]; meta_data$year=years[gy];
  
  prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = "")) #Load scan data
  scans = as.data.frame(table(prox_data$focal.monkey))
  meta_data$num.scans = scans$Freq[match(meta_data$id, scans$Var1)];
  if(years[gy]<2018){ meta_data$isPost =0}else{meta_data$isPost = 1}
  
  all_MetaData = rbind(all_MetaData, meta_data)
  
}

id.isPost = table(all_MetaData$id, all_MetaData$isPost)
id.PreAndPost = as.data.frame(row.names(as.data.frame(which(id.isPost[,1]>0 & id.isPost[,2]>0)))); names(id.PreAndPost)="id"
id.PreAndPost$group = all_MetaData$group[match(id.PreAndPost$id, all_MetaData$id)]
table(id.PreAndPost$group)


