#Simulating ability to detect short aggression bouts in scans vs. focals
#Testing the how focal sampling vs scan sampling can capture short aggressive bouts/actions

#NOTE: median # scan observations per day in 2018 is 300
#median # focals in a day pre-hurricane is 20
library(ggplot2)

#Set parameters
n_simulations = 1000
n_hours = 7 #number of hours in the day
time_in_s = n_hours*60*60 #number of seconds in a day
n_agg_events = 10 #number of aggressive events in the day
event_duration = c(3, 5, 10, 20, 30, 50, 100, 200) #aggression event duration in sec
focal_duration_hrs = 1/12 #time of focal in hours (i.e. 10min)
focal_duration_s = focal_duration_hrs*3600 #focal duration in seconds
n_focals = 20 # number of focals in a day
total_focal_h = n_focals*focal_duration_hrs #focal observation time in hours
total_focal_s = n_focals*focal_duration_s #focal observation time in seconds

scan_duration = 10 #duration of a scan observations in sec 
num_scans = total_focal_s/scan_duration #equivalent # scans than the focal hours observed 

#initiate # of aggressive events caught during focals and scans
num_agg_events_focal=matrix(0,length(event_duration), n_simulations) #focals
num_agg_events_scan=matrix(0,length(event_duration), n_simulations) #scans
i=1

for (dur in 1:length(event_duration)){
  for (i in 1:n_simulations){ #for all simulation iterations
    
    #randomly assign mid-point of aggressive events, focal follows and scans during the day
    event_times = sample((event_duration[dur]):(time_in_s-event_duration[dur]), n_agg_events) #aggressive events
    focal_times = sample((focal_duration_s):(time_in_s-focal_duration_s), n_focals) #focal follows
    scan_times = sample((scan_duration):(time_in_s-round(scan_duration)), num_scans) #scans
    
    #Assign time around mid-point according to set duration
    aggression = numeric(time_in_s); 
    for (e in 1:length(event_times)){ #for all aggression events
      aggression[(event_times[e]-event_duration[dur]/2+1):(event_times[e]+event_duration[dur]/2)]=e
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
    num_agg_events_focal[dur,i] = length(unique(aggression[focals!=0]))
    num_agg_events_scan[dur,i] = length(unique(aggression[scans!=0]))
  }
}

#Plot data
focal_mean = rowMeans(num_agg_events_focal) #mean number of unique aggression events captured during focals
scan_mean = rowMeans(num_agg_events_scan) #mean number of unique aggression events captured during scans
x<-event_duration
df<-data.frame(x,focal_mean, scan_mean)

colors <- c("Scan" = "blue", "Focal" = "red")
ggplot(df, aes(x))+                  # basic graphical object
  geom_line(aes(y=focal_mean, color = "Focal"), size = 1.5) +  # first layer
  geom_line(aes(y=scan_mean, color = "Scan"), size = 1.5) + # second layer
  labs(x = 'Length of aggression bout (s)',
       y = 'Number of aggression bout detected',
       color="Legend")+scale_color_manual(values=colors)+
  theme_classic(base_size = 15)
