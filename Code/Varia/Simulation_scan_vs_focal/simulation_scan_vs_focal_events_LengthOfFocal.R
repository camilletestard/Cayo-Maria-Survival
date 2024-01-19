#Simulating ability to detect short aggression bouts in scans vs. focals
#Testing the how focal sampling vs scan sampling can capture short aggressive bouts/actions

#Set parameters
n_simulations = 100
n_id=50
n_hours = 7 #number of hours in the day
time_in_s = n_hours*60*60 #number of seconds in a day
total_focal_h = 3.33 #focal observation time in hours (=20 10min focals a day)
total_focal_s = 3.33*3600 #focal observation time in seconds

n_agg_events = 500 #number of aggressive events in the day
event_duration = 10 #aggression event duration in sec
focal_duration_hrs = c(1/4, 1/6, 1/12) #time of focal in hours (i.e. 10min)
focal_duration_s = focal_duration_hrs*3600 #focal duration in seconds
n_focals = round(total_focal_h/focal_duration_hrs) # number of focals in a day
scan_duration = 10 #duration of a scan observations in sec 
num_scans = total_focal_s/scan_duration #equivalent # scans than the focal hours observed 

#Create aggression edgelist
all.pairs=data.frame(t(combn(1:n_id,2)))
all.pairs$pair.id=1:nrow(all.pairs)
rand.edges = sample(nrow(all.pairs),n_agg_events, replace=T)
agg_el = all.pairs[rand.edges,c(1,2)]
agg_el_pairID = all.pairs$pair.id[rand.edges]

#Set the sweeping parameter
sweep.param=focal_duration_s

#initiate # of aggressive events caught during focals and scans
num_agg_events_focal=matrix(0,length(sweep.param), n_simulations) #focals
num_agg_events_scan=matrix(0,length(sweep.param), n_simulations) #scans
i=1

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
    event_times = sample((event_duration):event_duration:(time_in_s-event_duration), n_agg_events) #aggressive events
    focal_times = sample((focal_duration_s[sweep]):focal_duration_s[sweep]:(time_in_s-focal_duration_s[sweep]), n_focals) #focal follows
    scan_times = sample((scan_duration):scan_duration:(time_in_s-round(scan_duration)), num_scans) #scans
    
    #check to make sure the number of unique event_times is as intended
    length(unique(event_times))==n_agg_events
    length(unique(focal_times))==n_focals
    length(unique(scan_times))==num_scans
    
    #Assign time around mid-point according to set duration
    aggression_id1 = numeric(time_in_s); 
    aggression_id2 = numeric(time_in_s); 
    aggression_event = numeric(time_in_s); 
    for (e in 1:length(event_times)){ #for all aggression events
      aggression_id1[(event_times[e]-event_duration[sweep]/2+1):(event_times[e]+event_duration[sweep]/2)]=agg_el[e,1]
      aggression_id2[(event_times[e]-event_duration[sweep]/2+1):(event_times[e]+event_duration[sweep]/2)]=agg_el[e,2]
      aggression_event[(event_times[e]-event_duration[sweep]/2+1):(event_times[e]+event_duration[sweep]/2)]=e
    }
    
    focals=numeric(time_in_s); 
    for (f in 1:length(focal_times)){ #for all focals
      focals[(focal_times[f]-focal_duration_s[sweep]/2+1):(focal_times[f]+focal_duration_s[sweep]/2)]=focal.list[f]
    }
    
    scans=numeric(time_in_s)
    for (s in 1:length(scan_times)){ #for all scans
      scans[(scan_times[s]-scan_duration/2+1):(scan_times[s]+scan_duration/2)]=scan.list[s]
    }
    
    df.focal=data.frame(aggression_id1, aggression_id2,aggression_event, focals)
    df.focal=df.focal[df.focal$focals!=0,]
    df.scan=data.frame(aggression_id1, aggression_id2,aggression_event, scans)
    df.scan=df.scan[df.scan$scans!=0,]
    
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
  labs(x = 'Length of focal (s)',
       y = 'Number of behavior bouts detected',
       color="Legend")+scale_color_manual(values=colors)+
  theme_classic(base_size = 15)
