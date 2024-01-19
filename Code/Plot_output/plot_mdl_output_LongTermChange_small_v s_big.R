library(mice)
library(broom.mixed)
library(eha)
library(ehahelper)
library(ggplot2)
library(forcats)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/") #Set working directory

#Load model outputs testing long-term change in sociality post Maria
#Small cayo
load("GroomMdlOutput_PrePost_smallCayo.RData")#Longitudinal change in grooming
load("ProxMdlOutput_PrePost_smallCayo.RData")#Longitudinal change in proximity
load("AggMdlOutput_PrePost_smallCayo.RData")#Longitudinal change in aggression

#Big cayo 
load("GroomMdlOutput_PrePost_bigCayo.RData")#Longitudinal change in grooming
load("ProxMdlOutput_PrePost_bigCayo.RData")#Longitudinal change in proximity
load("AggMdlOutput_PrePost_bigCayo.RData")#Longitudinal change in aggression

data_combined = data.frame()

###################
### SMALL CAYO ###
###################

#Proximity
mdl.pool.strength.prox<-summary(mice::pool(mdl.strength.proxPrePost.SC ))
est = mdl.pool.strength.prox$estimate
se = mdl.pool.strength.prox$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:5,]); data[nrow(data)+1,]= NA; names(data)=c("estimate","lower","upper")
data$location="small cayo"; data$behavior = "proximity"; data$metric = "Standardized nodal strength"; data$year = c(2018,2019,2021,2022,2020)
data_combined= rbind(data_combined,data)

#Aggression
mdl.pool.strength.agg<-summary(mice::pool(mdl.strength.aggPrePost.SC ))
est = mdl.pool.strength.agg$estimate
se = mdl.pool.strength.agg$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:4,]); data[nrow(data)+1,]= NA; names(data)=c("estimate","lower","upper")
data$location="small cayo";data$behavior = "aggression"; data$metric = "Standardized nodal strength"; data$year = c(2019,2021,2022,2020)
data_combined= rbind(data_combined,data)

#Grooming
mdl.pool.strength.groom<-summary(mice::pool(mdl.strength.groomPrePost.SC ))
est = mdl.pool.strength.groom$estimate
se = mdl.pool.strength.groom$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:5,]); data[nrow(data)+1,]= NA; names(data)=c("estimate","lower","upper")
data$location="small cayo";data$behavior = "grooming"; data$metric = "Standardized nodal strength"; data$year = c(2018,2019,2021,2022,2020)
data_combined= rbind(data_combined,data)

#################
### BIG CAYO ###
#################

#Proximity
mdl.pool.strength.prox<-summary(mice::pool(mdl.strength.proxPrePost.BC ))
est = mdl.pool.strength.prox$estimate
se = mdl.pool.strength.prox$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:5,]); data[nrow(data)+1,]= NA; names(data)=c("estimate","lower","upper")
data$location="big cayo"; data$behavior = "proximity"; data$metric = "Standardized nodal strength"; data$year = c(2018,2019,2021,2022,2020)
data_combined= rbind(data_combined,data)

#Aggression
mdl.pool.strength.agg<-summary(mice::pool(mdl.strength.aggPrePost.BC ))
est = mdl.pool.strength.agg$estimate
se = mdl.pool.strength.agg$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:4,]); data[nrow(data)+1,]= NA; names(data)=c("estimate","lower","upper")
data$location="big cayo";data$behavior = "aggression"; data$metric = "Standardized nodal strength"; data$year = c(2019,2021,2022,2020)
data_combined= rbind(data_combined,data)

#Grooming
mdl.pool.strength.groom<-summary(mice::pool(mdl.strength.groomPrePost.BC ))
est = mdl.pool.strength.groom$estimate
se = mdl.pool.strength.groom$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:5,]); data[nrow(data)+1,]= NA; names(data)=c("estimate","lower","upper")
data$location="big cayo";data$behavior = "grooming"; data$metric = "Standardized nodal strength"; data$year = c(2018,2019,2021,2022,2020)
data_combined= rbind(data_combined,data)

##################
# 4. Format output
##################
data_combined$year = as.factor(data_combined$year)
data_combined = data_combined %>%
  mutate(behavior = fct_relevel(behavior,
                                  "proximity","grooming","aggression"
  ))

#Plot output
ggplot(data_combined, aes(x=year,y=estimate)) +        # ggplot2 plot with confidence intervals
  #geom_point(size=0.75, shape=21, fill="blue") +
  geom_hline(yintercept =0,colour="black",linetype = "longdash",size=0.25)+
  geom_pointrange(aes(ymin = lower, ymax = upper), size=0.1, color="blue", fill="white", shape=22)+ 
  facet_grid(location~behavior)+
  #ylim(-1.5, 1.5)+
  #coord_flip()+
  theme_light()
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Estimates_LongTermProximityChange_byIsland.pdf")
