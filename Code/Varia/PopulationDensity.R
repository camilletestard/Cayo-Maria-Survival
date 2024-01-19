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

#Update census to consider deaths in 2022
death_update2022 =xlsx::read.xlsx("Deaths2022.xlsx",1)
death_update2022$Status = "DEAD"
id=1
for (id in 1:nrow(death_update2022)){
  idx=which(census$AnimalID == death_update2022$AnimalID[id])
  if(!purrr::is_empty(idx)){
    census[idx,c("DOD","Status")]=death_update2022[id, c("DOD","Status")]
  }
}


#pedigree = read.csv("/PEDIGREE_2021.txt", sep = '\t')
rm(list = setdiff(ls(), "census"))


#Set study period dates (consider each year separately)
dates = c("2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-12-31",
              "2018-12-31","2019-12-31","2020-12-31","2021-12-31","2022-12-31")

yr=1; PopSize=vector()
for (yr in 1:length(dates)){
  
PopDens = census
PopDens$DOD[is.na(PopDens$DOD)] = PopDens$DateTransfer[is.na(PopDens$DOD)]
PopDens = PopDens[census$DOB<dates[yr],]
PopDens$Status[PopDens$DOD>dates[yr]] ="IN CS"
PopSize[yr]=length(which(PopDens$Status=="IN CS"))


}

mean(PopSize[1:5])
mean(PopSize[6:10])
