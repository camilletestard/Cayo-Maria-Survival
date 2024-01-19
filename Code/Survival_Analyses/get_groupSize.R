#Generate input data for survival models.
#Feburary 2022, Camille Testard

library(stringr)
library(igraph)
library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#Load scan data and population info
setwd("~/Dropbox (Penn)/CayoBehavior/Data/Census")
census = xlsx::read.xlsx("2021-12-16-CENSO-FINAL.xlsx", 1)

group = c("F","KK","F","F","V","KK","V","F","F","KK","V","V","KK","S","V","F","V")
years = c(2013, 2013,2014,2015,2015,2015,
          2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021)
groupyears = c("F2013","KK2013","F2014",
               "F2015","V2015","KK2015",
               "V2016","F2016",
               "F2017","KK2017","V2017",
               "V2018","KK2018","S2019","V2019","F2021","V2021")

gy=1; Demographic=data.frame(matrix(NA, nrow=length(groupyears), ncol=6)); names(Demographic)=c("groupyear","group","year","groupsize","groupsize_JuvOrAdult","numF"); 
for (gy in 1:length(groupyears)){ #for all group & years
  
  Demographic$groupyear[gy] = groupyears[gy]
  Demographic$group[gy] = group[gy]
  Demographic$year[gy] = years[gy]
  census_gy = census[census$Natal.Group==group[gy],]
  date_thresh = paste0(years[gy],"-01-01")
  census_gy$age = (as.Date(date_thresh) - census_gy$DOB)/365
  census_gy$isJuvOrAdult = ifelse(census_gy$age>3,1,0)
  census_gy$Status[census_gy$DateTransfer>date_thresh]="IN CS"
  census_gy$Status[census_gy$DOD>date_thresh]="IN CS"
  
  Demographic$groupsize[gy] = length(which(census_gy$Status=="IN CS"))
  Demographic$groupsize_JuvOrAdult[gy] = length(which(census_gy$Status=="IN CS" & census_gy$isJuvOrAdult==1))
  Demographic$numF[gy] = length(which(census_gy$Sex=="FEMALE" & census_gy$Status=="IN CS"))
  Demographic$numF_JuvOrAdult[gy] = length(which(census_gy$Sex=="FEMALE" & census_gy$Status=="IN CS" & census_gy$isJuvOrAdult==1))
  Demographic$propF[gy] = Demographic$numF[gy]/Demographic$groupsize[gy]
  Demographic$propF_JuvOrAdult[gy] =  Demographic$numF_JuvOrAdult[gy]/Demographic$groupsize_JuvOrAdult[gy]
  Demographic$sexratio[gy] = Demographic$numF[gy]/(Demographic$groupsize[gy]-Demographic$numF[gy])
  Demographic$sexratio_JuvOrAdult[gy] = Demographic$numF_JuvOrAdult[gy]/(Demographic$groupsize_JuvOrAdult[gy]-Demographic$numF_JuvOrAdult[gy])
  
}

Demographic$location = ifelse(Demographic$group=="V","Small Cayo","Big Cayo")
Demographic$home.range.size = 3; Demographic$home.range.size[Demographic$group=="F"]=7; Demographic$home.range.size[Demographic$group=="KK"]=2; Demographic$home.range.size[Demographic$group==F]=2
Demographic$pop.density = Demographic$groupsize/Demographic$home.range.size
Demographic$pop.density.JuvOrAdult = Demographic$groupsize_JuvOrAdult/Demographic$home.range.size
Demographic$year = as.factor(Demographic$year)

ggplot(Demographic, aes(x=year, y=groupsize, color=group))+
  geom_point(size=4, alpha=0.7)+
  theme_classic(base_size = 14)+
  ylab("Group size (all ages)")

ggplot(Demographic, aes(x=year, y=groupsize_JuvOrAdult, color=group))+
  geom_point(size=4, alpha=0.7)+
  theme_classic(base_size = 14)+
  ylab("Group Size (>3yo)")

ggplot(Demographic, aes(x=year, y=pop.density, color=group))+
  geom_point(size=3, alpha=0.7)+
  theme_classic(base_size = 14)+
  ylab("Population per hectar (all ages)")

ggplot(Demographic, aes(x=year, y=pop.density.JuvOrAdult, color=group))+
  geom_point(size=3, alpha=0.7)+
  theme_classic(base_size = 14)+
  ylab("Population per hectar (>3yo)")

#Save file for pre-hurricane sociality
save(Demographic,file ="~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/Survival_Adults.RData")
