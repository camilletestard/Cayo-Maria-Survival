library(lubridate)
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/')
data = read.delim('CayoDemo_2021.txt',sep='\t')

data$Death <- mdy(as.character(data$Death))
length(which(as.Date(data$Death)>"2017-09-20" & as.Date(data$Death)<"2017-10-20"))
