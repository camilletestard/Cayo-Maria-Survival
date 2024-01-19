#Who dies after the hurricane

#############################################
# Pre-hurricane sociality 
#############################################

### Load data ###
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data')
load('SocialCapital_Adults.RData'); data = SocialCapital.ALL
length(which(data$Survival==1))

data=data[data$Survival==1,]
data=data[match(unique(data$id), data$id),]

hist(as.numeric(data$Age_event.days)/360)
hist(data$percentrank); table(data$ordrank)
table(data$sex)

#############################################
# Change in sociality pre-to-post hurricane
#############################################
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data')
load('SocialCapital_changeP_Adults.RData'); data = full.data

data=data[data$Survival==1,]
data=data[match(unique(data$id), data$id),]

hist(as.numeric(data$Age_event.days)/360)
hist(data$percentrank); table(data$ordrank)
table(data$sex)

