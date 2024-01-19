#Simulating ability to detect short aggression bouts in scans vs. focals
#Testing the how focal sampling vs scan sampling can capture short aggressive bouts/actions
library(ggplot2)

#Set parameters
n_simulations = 1000
n_id=50
n_hours = 7 #number of hours in the day
total_time_s = n_hours*60*60 #number of seconds in a day
n_events = 200 #number of aggressive events in the day
event_duration = c(2, 4, 6, 8, 20, 30, 100) #aggression event duration in sec
#!!!!!! IMPORTANT NOTE: because of the limited duration of the observation period, there is an 
#upper limit to the length of bout before they start overlapping
focal_duration_hrs = 1/6 #time of focal in hours (i.e. 10min)
focal_duration_s = focal_duration_hrs*3600 #focal duration in seconds
n_focals = 20 # number of focals in a day
total_obs_s = n_focals*focal_duration_s #focal observation time in seconds
scan_duration = 10 #duration of a scan observations in sec 
num_scans = total_obs_s/scan_duration #equivalent # scans than the focal hours observed 

#SANITY CHECKS
total_obs_s < total_time_s
max(event_duration)*n_events < total_time_s

#Create aggression edgelist
all.pairs=data.frame(t(combn(1:n_id,2)))
all.pairs$pair.id=1:nrow(all.pairs)
rand.edges = sample(nrow(all.pairs),n_events, replace=T)
agg_el = all.pairs[rand.edges,c(1,2)]

#Set the sweeping parameter
sweep.param=event_duration

#initiate # of aggressive events caught during focals and scans
num_agg_events_focal=matrix(0,length(sweep.param), n_simulations) #focals
num_agg_events_scan=matrix(0,length(sweep.param), n_simulations) #scans
df.focal.list=list(); df.scan.list=list()
i=1; sweep=1
for (sweep in 1:length(sweep.param)){
  for (i in 1:n_simulations){ #for all simulation iterations
    
    #Create focal list
    if(n_id>n_focals){
      focal.list = sample(1:n_id,n_focals)
    }else{
    focal.list = c(sample(1:n_id,n_id),sample(1:n_id,n_id))}
    
    #Create equivalent scan list
    scan.list = sample(1:n_id,num_scans,replace = T)
    
    #randomly assign mid-point of aggressive events, focal follows and scans during the day
    event_times = sample(seq.int((event_duration[sweep]),(total_time_s-event_duration[sweep]), event_duration[sweep]), n_events) #aggressive events
    focal_times = sample(seq.int((focal_duration_s),(total_time_s-focal_duration_s),focal_duration_s), n_focals) #focal follows
    scan_times = sample(seq.int((scan_duration),(total_time_s-scan_duration),scan_duration), num_scans) #scans
    
    #Assign time around mid-point according to set duration
    aggression_id1 = numeric(total_time_s); 
    aggression_id2 = numeric(total_time_s); 
    aggression_event = numeric(total_time_s); 
    for (e in 1:length(event_times)){ #for all aggression events
      aggression_id1[(event_times[e]-event_duration[sweep]/2+1):(event_times[e]+event_duration[sweep]/2)]=agg_el[e,1]
      aggression_id2[(event_times[e]-event_duration[sweep]/2+1):(event_times[e]+event_duration[sweep]/2)]=agg_el[e,2]
      aggression_event[(event_times[e]-event_duration[sweep]/2+1):(event_times[e]+event_duration[sweep]/2)]=e
    }
    
    focals=numeric(total_time_s); 
    for (f in 1:length(focal_times)){ #for all focals
      focals[(focal_times[f]-focal_duration_s/2+1):(focal_times[f]+focal_duration_s/2)]=focal.list[f]
    }
    
    scans=numeric(total_time_s)
    for (s in 1:length(scan_times)){ #for all scans
      scans[(scan_times[s]-scan_duration/2+1):(scan_times[s]+scan_duration/2)]=scan.list[s]
    }
    
    #Sanity check: make sure the number of unique event_times is as intended
    if(length(which(aggression_event!=0))!=n_events*event_duration[sweep]|
       length(which(focals!=0))!=total_obs_s|
       length(which(scans!=0))!=total_obs_s){
      stop()
    }
    
    df.focal.list[[i]]=data.frame(aggression_id1, aggression_id2,aggression_event, focals)
    df.focal=df.focal.list[[i]]; df.focal=df.focal[df.focal$focals!=0,]
    df.scan.list[[i]]=data.frame(aggression_id1, aggression_id2,aggression_event, scans)
    df.scan=df.scan.list[[i]]; df.scan=df.scan[df.scan$scans!=0,]
    
    #Extract unique number of aggressive events caught during focals and scans
    agg.focal=df.focal$aggression_event[which(df.focal$aggression_id1==df.focal$focals | df.focal$aggression_id2==df.focal$focals)]
    num_agg_events_focal[sweep,i] = length(unique(agg.focal))
    
    agg.scan=df.scan$aggression_event[which(df.scan$aggression_id1==df.scan$scans | df.scan$aggression_id2==df.scan$scans)]
    num_agg_events_scan[sweep,i] = length(unique(agg.scan))
  }
}

#Plot results
focal_mean = rowMeans(num_agg_events_focal) #mean number of unique aggression events captured during focals
scan_mean = rowMeans(num_agg_events_scan) #mean number of unique aggression events captured during scans
x<-sweep.param
df<-data.frame(x,focal_mean, scan_mean)

colors <- c("Scan" = "blue", "Focal" = "red")
ggplot(df, aes(x))+                  # basic graphical object
  geom_line(aes(y=focal_mean, color = "Focal"), size = 1.5) +  # first layer
  geom_line(aes(y=scan_mean, color = "Scan"), size = 1.5) + # second layer
  labs(x = 'Length of behavior bout (s)',
       y = 'Number of behavior bouts detected',
       color="Legend")+scale_color_manual(values=colors)+
  theme_light(base_size = 15)
