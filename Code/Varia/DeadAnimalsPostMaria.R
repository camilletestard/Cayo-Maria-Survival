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


#Set study start and end dates
study.start.date = as.Date("2017-09-17")
study.end.date = as.Date("2018-09-15")

#Get death rates during study period
deaths.post.maria = census[which(census$DOD>study.start.date & census$DOD<study.end.date),]

#Calculate age
deaths.post.maria$age.days = deaths.post.maria$DOD - deaths.post.maria$DOB
deaths.post.maria$age.year =deaths.post.maria$age.days/365

#Get age distribution
age.at.death =as.numeric(deaths.post.maria$age.year)
hist(age.at.death, breaks =10)
length(which(age.at.death>6))

deaths.post.maria.adults = deaths.post.maria[deaths.post.maria$age.year>6,]
table(deaths.post.maria.adults$Natal.Group)