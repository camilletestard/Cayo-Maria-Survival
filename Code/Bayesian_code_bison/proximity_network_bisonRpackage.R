# BiSON code for testing change in sociality across time.
#Proximity, grooming and aggression

#Load functions
setwd("~/Documents/GitHub/Cayo-Maria-Survival/code/Bayesian_code_bison/")
source("scripts/functions_GlobalNetworkMetrics.R")

#Load libraries
library(bisonR)
library(dplyr)
library(ggplot2)

#For groups and years:
group= "V"
year=2016
groupyear = paste(group,year,sep="")

######################################################
## Extract proximity data - Create aggregated dataframe
######################################################

#Load cayo data
dataPath = '~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/'
prox_data = read.csv(paste(dataPath,"Group",groupyear,"_ProximityGroups.txt", sep=""))
meta_data = read.csv(paste(dataPath,"Group",groupyear,"_GroupByYear.txt", sep=""))

if (groupyear == "V2019"){ #quick fix for now
  prox_idx = !is.na(prox_data$partners.activity..sequential.) #find indices where there are individuals in proximity
  #add focal monkey in proximity column
  prox_data$in.proximity[prox_idx]= paste(prox_data$focal.monkey[prox_idx], 
                                          prox_data$in.proximity[prox_idx],sep=",")
}

#Format data with aggregate format
# Output the Master Edgelist of all possible pairs given the unique IDs.
unqIDs = meta_data$id#[meta_data$focalcutoff_met=="Y"]
edgelist = calcMasterEL(unqIDs);
df_obs_agg  = calcEdgeList(prox_data, edgelist);
names(df_obs_agg)=c("ID1", "ID2", "dyad_id","count")

#extract the number of unique IDs
unique_names <- unique(c(df_obs_agg$ID1, df_obs_agg$ID2))
nr_ind <- length(unique_names)
nr_dyads <- nr_ind*(nr_ind-1)/2 # -1 to remove self-interactions e.g. AA & /2 because undirected so AB = BA

#Get observation effort for each dyad
numscans = as.data.frame(table(prox_data$focal.monkey))

df_obs_agg$ID1_obseff_duration = meta_data$hrs.focalfollowed[match(df_obs_agg$ID1, meta_data$id)]
df_obs_agg$ID1_obseff_samples = numscans$Freq[match(df_obs_agg$ID1, numscans$Var1)]
df_obs_agg$ID2_obseff_duration = meta_data$hrs.focalfollowed[match(df_obs_agg$ID2, meta_data$id)] 
df_obs_agg$ID2_obseff_samples = numscans$Freq[match(df_obs_agg$ID2, numscans$Var1)]
df_obs_agg$total_obs_time = df_obs_agg$ID1_obseff_duration  + df_obs_agg$ID2_obseff_duration 
df_obs_agg$total_samples = df_obs_agg$ID1_obseff_samples + df_obs_agg$ID2_obseff_samples

## Add id qualifiers
df_obs_agg$ID1 = factor(df_obs_agg$ID1, levels = unique_names); df_obs_agg$ID2 = factor(df_obs_agg$ID2, levels = unique_names); 
df_obs_agg$ID1_id = as.integer(df_obs_agg$ID1); df_obs_agg$ID2_id = as.integer(df_obs_agg$ID2)
df_obs_agg$dyad_id = factor(df_obs_agg$dyad_id, levels=df_obs_agg$dyad_id)
#sex
df_obs_agg$ID1_sex = meta_data$sex[match(df_obs_agg$ID1, meta_data$id)]
df_obs_agg$ID2_sex = meta_data$sex[match(df_obs_agg$ID2, meta_data$id)]
#rank
df_obs_agg$ID1_rank = meta_data$ordinal.rank[match(df_obs_agg$ID1, meta_data$id)]
df_obs_agg$ID2_rank = meta_data$ordinal.rank[match(df_obs_agg$ID2, meta_data$id)]
#age
df_obs_agg$ID1_age = meta_data$age[match(df_obs_agg$ID1, meta_data$id)]
df_obs_agg$ID2_age = meta_data$age[match(df_obs_agg$ID2, meta_data$id)]
#group, year, Hurricane status
df_obs_agg$group = group; df_obs_agg$year = year; df_obs_agg$isPost = "post"

head(df_obs_agg)

save(df_obs_agg,file = "Proximity_df.RData")

######################################################
## Fit edge model
######################################################
library(bisonR)

setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/')
load("Proximity_df.RData")

#Set priors
priors <- get_default_priors("binary")
priors

#Fit model
fit_edge <- edge_model(
  (count | total_samples) ~ dyad(ID1, ID2), 
  data=df_obs_agg, 
  data_type="binary", 
  priors=priors
)

#Check model fit
plot_trace(fit_edge)

#Check posterior
plot_predictions(fit_edge, num_draws=100)

summary(fit_edge)







