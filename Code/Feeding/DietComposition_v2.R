#Diet composition

group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V",
          "S","V","F","V","V", "TT", "F")
years = c(2013,2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,
          2017,2017,2017,
          2019, 2019,2021,2021, 2022, 2022, 2022)
groupyears = paste0(group, years)

gy=1; plant_perc=data.frame(groupyears, group, years);  plant_perc$plantPerc = NA; focal_data_all=data.frame()
for (gy in 1:length(groupyears)){ #for all group & years
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  #Load data
  setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = "")); meta_data$idcode=NULL #load meta data
  meta_data = meta_data[meta_data$hrs.focalfollowed>0,]

  focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = "")) 
  focal_data$year = years[gy]
  focal_data_all = rbind(focal_data_all, focal_data)
}
  
  focal_data_all$food_type[focal_data_all$food_type=="Water"]="water"
  focal_data_all$food_type[focal_data_all$food_type=="feedwater"]="water"
  focal_data_all$food_type[focal_data_all$food_type=="Plant"]="plant"
  focal_data_all$food_type[focal_data_all$food_type=="Plants"]="plant"
  focal_data_all$food_type[focal_data_all$food_type=="feedplant"]="plant"
  
  feeding_data = focal_data_all[focal_data_all$behaviour=="Feed" & focal_data_all$food_type!="water",]
length(which(feeding_data$food_type=="plant"))/nrow(feeding_data)
  
prehurr=length(which(feeding_data$food_type[feeding_data$year<2018]=="plant"))/nrow(feeding_data[feeding_data$year<2018,])
posthurr=length(which(feeding_data$food_type[feeding_data$year>2017]=="plant"))/nrow(feeding_data[feeding_data$year>2017,])

ggplot(plant_perc, aes(x=years, y = plantPerc, color = group))+
  geom_point(size=3, alpha=0.7)+
  theme_light(base_size = 14)+
  xline(xintercept = )
ggsave("CayoMacaqueDiet.pdf")
