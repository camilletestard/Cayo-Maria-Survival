library(mice)
library(broom.mixed)
library(eha)
library(ehahelper)
library(ggplot2)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/") #Set working directory

#Load model outputs testing long-term change in sociality post Maria
load("GroomMdlOutput_PrePost_bigCayo.RData")#Longitudinal change in grooming
load("ProxMdlOutput_PrePost_bigCayo.RData")#Longitudinal change in proximity
load("AggMdlOutput_PrePost_bigCayo.RData")#Longitudinal change in aggression

######################
# 1. Network density
######################

data_combined = data.frame()
# 
# #Proximity
# mdl.density.prox.pool = summary(mdl.proxPrePost)
# output = mdl.density.prox.pool$coefficients; output = output[2:4,]
# est = as.numeric(output[,1])
# se = as.numeric(output[,2])
# data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
# data=data.frame(data); names(data)=c("estimate","lower","upper")
# data$behavior = "proximity"; data$metric = "Standardized network density"; data$year = c(2018,2019,2021)
# data_combined= rbind(data_combined,data)
# 
# 
# #Aggression
# mdl.density.agg.pool = summary(mdl.aggPrePost)
# output = mdl.density.agg.pool$coefficients; output = output[2:3,]
# est = as.numeric(output[,1])
# se = as.numeric(output[,2])
# data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
# data=data.frame(data); names(data)=c("estimate","lower","upper")
# data$behavior = "aggression"; data$metric = "Standardized network density"; data$year = c(2019,2021)
# data_combined= rbind(data_combined,data)
# 
# #Grooming
# mdl.density.groom.pool = summary(mdl.groomPrePost)
# output = mdl.density.groom.pool$coefficients; output = output[2:4,]
# est = as.numeric(output[,1])
# se = as.numeric(output[,2])
# data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
# data=data.frame(data); names(data)=c("estimate","lower","upper")
# data$behavior = "grooming"; data$metric = "Standardized network density"; data$year = c(2018,2019,2021)
# data_combined= rbind(data_combined,data)


#####################
# 2. Nodal strength 
#####################

#Proximity
mdl.pool.strength.prox<-summary(mice::pool(mdl.strength.proxPrePost ))
est = mdl.pool.strength.prox$estimate
se = mdl.pool.strength.prox$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:4,]); names(data)=c("estimate","lower","upper")
data$behavior = "proximity"; data$metric = "Standardized nodal strength"; data$year = c(2018,2019,2021)
data_combined= rbind(data_combined,data)

#Aggression
mdl.pool.strength.agg<-summary(mice::pool(mdl.strength.aggPrePost ))
est = mdl.pool.strength.agg$estimate
se = mdl.pool.strength.agg$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:3,]); names(data)=c("estimate","lower","upper")
data$behavior = "aggression"; data$metric = "Standardized nodal strength"; data$year = c(2019,2021)
data_combined= rbind(data_combined,data)

#Grooming
mdl.pool.strength.groom<-summary(mice::pool(mdl.strength.groomPrePost ))
est = mdl.pool.strength.groom$estimate
se = mdl.pool.strength.groom$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:4,]); names(data)=c("estimate","lower","upper")
data$behavior = "grooming"; data$metric = "Standardized nodal strength"; data$year = c(2018,2019,2021)
data_combined= rbind(data_combined,data)

###################
# 3. Nodal degree 
###################

#Proximity
mdl.pool.degree.prox<-summary(mice::pool(mdl.degree.proxPrePost ))
est = mdl.pool.degree.prox$estimate
se = mdl.pool.degree.prox$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:4,]); names(data)=c("estimate","lower","upper")
data$behavior = "proximity"; data$metric = "Standardized nodal degree"; data$year = c(2018,2019,2021)
data_combined= rbind(data_combined,data)

#Aggression
mdl.pool.degree.agg<-summary(mice::pool(mdl.degree.aggPrePost ))
est = mdl.pool.degree.agg$estimate
se = mdl.pool.degree.agg$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:3,]); names(data)=c("estimate","lower","upper")
data$behavior = "aggression"; data$metric = "Standardized nodal degree"; data$year = c(2019,2021)
data_combined= rbind(data_combined,data)

#Grooming
mdl.pool.degree.groom<-summary(mice::pool(mdl.degree.groomPrePost ))
est = mdl.pool.degree.groom$estimate
se = mdl.pool.degree.groom$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(data[2:4,]); names(data)=c("estimate","lower","upper")
data$behavior = "grooming"; data$metric = "Standardized nodal degree"; data$year = c(2018,2019,2021)
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
  facet_grid(behavior~ metric)+
  #ylim(-1.5, 1.5)+
  #coord_flip()+
  theme_light()
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Estimates_LongTermChange.pdf")
