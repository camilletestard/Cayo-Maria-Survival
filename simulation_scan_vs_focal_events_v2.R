#Simulating ability to detect short aggression bouts in scans vs. focals
#Testing the how focal sampling vs scan sampling can capture short aggressive bouts/actions

#NOTE: median # scan observations per day in 2018 is 300
#median # focals in a day pre-hurricane is 20

#Set parameters
n_simulations = 1000
n_id=10
n_hours = 7 #number of hours in the day
time_in_s = n_hours*60*60 #number of seconds in a day
n_agg_events = 50 #number of aggressive events in the day
event_duration = 20 #aggression event duration in sec
focal_duration_hrs = 1/6 #time of focal in hours (i.e. 10min)
focal_duration_s = focal_duration_hrs*3600 #focal duration in seconds
n_focals = 20 # number of focals in a day
total_focal_h = n_focals*focal_duration_hrs #focal observation time in hours
total_focal_s = n_focals*focal_duration_s #focal observation time in seconds

#Create aggression edgelist
all.pairs=data.frame(t(combn(1:n_id,2)))
all.pairs$pair.id=1:nrow(all.pairs)
agg_el = all.pairs[sample(nrow(all.pairs),n_agg_events, replace=T),c(1,2)]
agg_el_pairID = all.pairs$pair.id[sample(nrow(all.pairs),n_agg_events, replace=T)]

#Create focal list
focal_ID = sample(1:n_id, n_focals, replace=T)

#Model the individual nature of the scan and focal sampling.
#Randomly assign id missing in scan (maybe assume that all individuals are visible)

scan_duration = 10 #duration of a scan observations in sec 
num_scans = total_focal_s/scan_duration #equivalent # scans than the focal hours observed 

#initiate # of aggressive events caught during focals and scans
num_agg_events_focal=numeric(n_simulations) #focals
num_agg_events_scan=numeric(n_simulations) #scans
i=1
for (i in 1:n_simulations){ #for all simulation iterations
  
  #randomly assign mid-point of aggressive events, focal follows and scans during the day
  event_times = sample((event_duration):(time_in_s-event_duration), n_agg_events) #aggressive events
  focal_times = sample((focal_duration_s):(time_in_s-focal_duration_s), n_focals) #focal follows
  scan_times = sample((scan_duration):(time_in_s-round(event_duration)), num_scans) #scans
  
  #Assign time around mid-point according to set duration
  aggression = numeric(time_in_s); 
  for (e in 1:length(event_times)){ #for all aggression events
    aggression_id[(event_times[e]-event_duration/2+1):(event_times[e]+event_duration/2)]=agg_el_pairID[e]
    aggression_event[(event_times[e]-event_duration/2+1):(event_times[e]+event_duration/2)]=e
  }
  
  focals=numeric(time_in_s); 
  for (f in 1:length(focal_times)){ #for all focals
    focals[(focal_times[f]-focal_duration_s/2+1):(focal_times[f]+focal_duration_s/2)]=f
  }
  
  scans=numeric(time_in_s)
  for (s in 1:length(scan_times)){ #for all scans
    scans[(scan_times[s]-scan_duration/2+1):(scan_times[s]+scan_duration/2)]=s
  }
  
  #Extract unique number of aggressive events caught during focals and scans
  num_agg_events_focal[i] = length(unique(aggression[focals!=0]))-1
  num_agg_events_scan[i] = length(unique(aggression[scans!=0]))-1
}

mean(num_agg_events_focal) #mean number of unique aggression events captured during focals
mean(num_agg_events_scan) #mean number of unique aggression events captured during scans
