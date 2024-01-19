#Generate temporary group by year files for 2022 data

group = c("TT", "V")
years = c(2022,2022)
groupyears = c("TT2022", "V2022")
gy=1
for (gy in 1:length(groupyears)){

setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = ""))
focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = "")) #load meta data
example_meta_data = read.csv("GroupV2017_GroupByYear.txt") #load meta data
demographics = read.csv("CayoDemo_2021.txt", sep = '\t')

focal.id.scans = unique(prox_data$focal.monkey)
focal.id = unique(focal_data$focal_id)
unqIDs = unique(focal.id.scans, focal.id)

meta_data<-data.frame(matrix(NA,    # Create empty data frame
                  nrow = length(unqIDs),
                  ncol = ncol(example_meta_data)))
names(meta_data) = names(example_meta_data)

#Assign demographic variables
meta_data$id = unqIDs
meta_data[,c("sex", "year.of.birth")] = demographics[match( unqIDs,demographics$AnimalID),c("Sex","DOB")]
meta_data$sex = ifelse(meta_data$sex=="FEMALE","F","M")
meta_data$year.of.birth = as.Date(meta_data$year.of.birth, "%m/%d/%y")
meta_data$age = as.numeric(round((as.Date("2022-08-31") -meta_data$year.of.birth)/365.25))

#Assign observation effort
id=1
for (id in 1:length(unqIDs)){
  
  meta_data$hrs.focalfollowed[id] = length(unique(focal_data$observation_name[focal_data$focal_id==unqIDs[id]]))*10/60
  
}

write.csv( meta_data, paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))

}