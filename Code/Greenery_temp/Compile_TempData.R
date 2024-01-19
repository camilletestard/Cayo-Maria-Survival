#Compile_TempData.R
#This script compiles all individual .csv files from the Google Drive folder
#as well as the meta data for the temperature loggers.
#Jan 2023 C. Testard

# load libraries --------
library(readr)
library(tidyverse)
library(googledrive)
library(lubridate)
library(hms) 
library(ggplot2)

#Load meta data
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Temperature/')
marina_data = read.csv("cayo_therm_data_2018-2020.csv",sep=',')
# marina_data$year = year(marina_data$date); table(marina_data$year)
# marina_data$month = month(marina_data$date);table(marina_data$month)
# marina_data$hour = substr(marina_data$time,1,2);table(marina_data$hour)

meta_data=read.csv("Thermochron_deployment_locations_2018_2019.csv",sep = ';')
meta_data$device_address[nchar(meta_data$device_address)>16]= #Adjust misnamed temp loggers
  substr(meta_data$device_address[nchar(meta_data$device_address)>16],2,17)
#remove duplicates
meta_data=meta_data[!duplicated(meta_data$device_address),]

#Load and format all individual temp records csv files
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Temperature/Data')
csv_files <- dir(pattern='*.csv$', recursive = T)

fi=1; all_files=data.frame()
for (fi in 1:length(csv_files)){
  #Read csv file
  file=read.csv(csv_files[fi],skip = 14, header = T,sep = ',')
  file$file.id=fi
  
  #Extract therm number:
  file$therm.id=substr(csv_files[fi],nchar(csv_files[fi])-14,nchar(csv_files[fi])-13)
  
  #Extract device address to merge with meta data later
  device_address=read.csv(csv_files[fi]); device_address=device_address[1,1]; device_address=substr(device_address,nchar(device_address)-15,nchar(device_address))
  file$device_address = device_address
  
  #Bind file with the rest of the data
  all_files = rbind(all_files, file)
  
  print(fi)
}

#Format dataframe
names(all_files)=c("date.time","unit","value","file.id","therm.id","device_address")
all_files$date.time = as.POSIXct(all_files$date.time, format = "%m/%d/%y %I:%M:%S %p")
#IMPORTANTE NOTE: date doesnt parse properly for a certain amount of dates
# test=as.POSIXct(all_files$date.time, format = "%m/%d/%y %H:%M:%S")
# test=strptime(all_files$date.time, "%m/%d/%y %H:%M:%S")
# all_files[which(is.na(test)),]
# all_files[which(!is.na(test)),]

all_files$date = as.Date(all_files$date.time)
all_files$year = year(all_files$date.time)
#fix 2000 years. they are all 2021 files
all_files$year[all_files$year==2000]=2021
all_files$month = month(all_files$date.time)
all_files$time <- as_hms(all_files$date.time)
all_files$am.pm <-"night"; 
all_files$am.pm[all_files$time>as_hms("6:00:00") & all_files$time<as_hms("10:00:00")]="AM"
all_files$am.pm[all_files$time>as_hms("10:00:00") & all_files$time<as_hms("17:00:00")]="PM"

#Check device missing or name mismatch
device_names=unique(all_files$device_address)
device_names[is.na(match(device_names, meta_data$device_address))] #devices with temp data but no meta data
meta_data$device_address[is.na(match(meta_data$device_address, device_names))]#devices with no temp data but with meta data
#IMPORTANTE NOTE: Need to update for 2022 change in location / new loggers

#Merge location and vegetation status with temp data
temp_data=all_files
temp_data[,c("location","vegetation")] = meta_data[match(all_files$device_address,meta_data$device_address), c("location","vegetation")]
#temp_data[,c("location","vegetation")] = as.factor(temp_data[,c("location","vegetation")])
temp_data$gross_location = "Big cayo"; 
temp_data$gross_location[temp_data$location=="SCU"|temp_data$location=="SCL"]="Small cayo"

#Format temp data for plotting
#Remove missing data
temp_data_final = temp_data[rowSums(is.na(temp_data)) == 0, ] 
#Remove 2022 data for now (location may have changed)
temp_data_final = temp_data_final[temp_data_final$year<2022,]
#Remove night time data
temp_data_final = temp_data_final[temp_data_final$am.pm!="night",]
#Lump de-vegetated areas
temp_data_final$is.shaded=ifelse(temp_data_final$vegetation == "Vegetated","Shaded","Exposed")


#Check data compared to Marina's
table(temp_data_final$year)
table(temp_data_final$month)
table(marina_data$year)

#Plot temp. by vegetation type and location
tempPlot<-ggplot(temp_data_final, aes(x=vegetation,y=value))+
  geom_violin()+
  theme_classic(base_size = 12)+
  facet_grid(~gross_location)
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Temperature_plot.pdf",tempPlot)
aggregate(value ~ vegetation + year, data =temp_data_final, FUN = mean, na.rm = TRUE)
aggregate(value ~ vegetation + gross_location, data =temp_data_final, FUN = mean, na.rm = TRUE)

#Average difference between shaded and non shaded areas
aggregate(temp_data_final$value, list(temp_data_final$is.shaded), FUN=mean)
t.test(temp_data_final$value[temp_data_final$is.shaded=="Shaded"], temp_data_final$value[temp_data_final$is.shaded=="Exposed"])

#Find how much of the time during the day temperatures exceed 40C
temp_data_day_exposed = temp_data_final[temp_data_final$is.shaded=="Exposed" &
                                          temp_data_final$time > as_hms("10:00:00"),]
length(which(temp_data_day_exposed$value>40))/nrow(temp_data_day_exposed)

#Plot temperature by time of day
temp_data_exposed = temp_data_final[temp_data_final$is.shaded=="Exposed",]
plot(temp_data_exposed$time, temp_data_exposed$value)
temp_data_exposed$time = as.POSIXct(temp_data_exposed$time, format="%H:%M:%S")
ggplot(temp_data_exposed, aes(x=time, y=value))+
  geom_smooth()+
  scale_x_datetime(date_breaks = "hour")+
  #geom_hline(yintercept = 40, linetype = "dashed", colour = "black")+
  theme_classic(base_size = 12)
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Temperature_by_time_of_day.pdf")

#Plot temperature by time of year
temp_data_exposed$is.summer="summer"
temp_data_exposed$is.summer[temp_data_exposed$month<5 | temp_data_exposed$month>9]="winter"
ggplot(temp_data_exposed, aes(x=am.pm, y=value))+
  geom_violin()+
  geom_boxplot(width=.1)+
  facet_grid(~is.summer)+
  #geom_hline(yintercept = 40, linetype = "dashed", colour = "black")+
  theme_light(base_size = 12)

temp_data_exposed$month=as.factor(temp_data_exposed$month)
ggplot(temp_data_exposed, aes(x=month, y=value))+
  geom_violin()+
  geom_boxplot(width=.1)+
  facet_grid(~am.pm)+
  #geom_hline(yintercept = 40, linetype = "dashed", colour = "black")+
  theme_light(base_size = 12)
aggregate(temp_data_exposed$value, list(temp_data_exposed$is.summer), FUN=median)



#Plot temp. by vegetation type and time of day
ggplot(temp_data_final, aes(x=is.shaded,y=value))+
  geom_boxplot()+
  theme_classic(base_size = 12)+
  facet_grid(~am.pm)
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Temperature_plot_byTime.pdf")
aggregate(value ~ is.shaded+ am.pm, data =temp_data_final, FUN = median, na.rm = TRUE)

# 
# #Check marina's data
# ggplot(marina_data, aes(x=state,y=Value))+
#   geom_violin()+
#   theme_classic(base_size = 12)
# aggregate(marina_data$Value, list(marina_data$state), FUN=mean)
# aggregate(marina_data$Value, list(marina_data$state), FUN=median)
