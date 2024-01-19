#Simulating ability  scan vs. focal

#NOTE: mean # scans per day for V2018 is 268, KK2018 is 270
#mean # focals in a day for V2015 is 20

group = c("F","V","R","KK",
          "R","V","F","HH",
          "F","KK","V","V","KK",
          "S","V","F","V")
years = c(2015,2015,2015,2015,
          2016,2016,2016,2016,
          2017,2017,2017,2018,2018,
          2019,2019,2021,2021)
groupyears = c("F2015","V2015","R2015","KK2015",
               "R2016","V2016","F2016","HH2016",
               "F2017","KK2017","V2017","V2018","KK2018",
               "S2019","V2019","F2021","V2021") 
gy=2; mean.scans.per.day=vector(); mean.focal.per.day=vector()
for (gy in 1:length(groupyears)){ #for all group & years
  
  if (years[gy]==2018) {
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    scans2018= read.csv(paste("Group",groupyears[gy],"_scansamples_FULL_CLEANED.csv", sep = ""))
    scans.per.day = as.data.frame(table(scans2018$date))
    mean.scans.per.day = cbind(mean.scans.per.day, mean(scans.per.day$Freq))
  }else{
    
    #Load data
    setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles')
    focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = ""))
    mean.focal.per.day = cbind(mean.focal.per.day, length(unique(focal_data$observation_name))/length(unique(focal_data$date)))
  }
}

mean(mean.focal.per.day)
mean(mean.scans.per.day)

#################################################################################

simulations = 1000
n_hours = 6 #number of hours in the day
time_in_s = n_hours*60*60 #number of seconds in a day
n_agg_events = 50 #number of aggressive events in the day
event_duration = 20 #aggression event duration in sec
focal_duration_hrs = 1/6 #time of focal in hours (i.e. 10min)
focal_duration_s = focal_duration_hrs*3600 #focal huration in seconds
n_focals = 10 # number of focals in a day
total_focal_h = n_focals*focal_duration_hrs #focal observation time in hours
total_focal_s = n_focals*focal_duration_s #focal observation time in seconds

scan_duration = 5#round(focal_duration_s/num_scans) #duration of a scan observations equivalent to the amount of focal
num_scans = total_focal_s/scan_duration #260 #round(99.61+294.75*total_focal_h) #estimate # scans from the focal hours observed (formula based on the actual data)

num_agg_events_focal=numeric(simulations)
num_agg_events_scan=numeric(simulations)
for (i in 1:simulations){
  
  event_times = sample((event_duration):(time_in_s-event_duration), n_agg_events)
  focal_times = sample((focal_duration_s):(time_in_s-focal_duration_s), n_focals)
  scan_times = sample((scan_duration):(time_in_s-round(event_duration)), num_scans)
  
  aggression = numeric(time_in_s); 
  for (e in 1:length(event_times)){
    aggression[(event_times[e]-event_duration/2+1):(event_times[e]+event_duration/2)]=e
  }
  
  focals=numeric(time_in_s); 
  for (f in 1:length(focal_times)){
    focals[(focal_times[f]-focal_duration_s/2+1):(focal_times[f]+focal_duration_s/2)]=f
  }
  
  scans=numeric(time_in_s)
  for (s in 1:length(scan_times)){
    scans[(scan_times[s]-scan_duration/2+1):(scan_times[s]+scan_duration/2)]=s
  }
  
  num_agg_events_focal[i] = length(unique(aggression[focals!=0]))
  num_agg_events_scan[i] = length(unique(aggression[scans!=0]))
}

mean(num_agg_events_focal)
mean(num_agg_events_scan)
