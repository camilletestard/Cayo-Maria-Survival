library(mice)
library(broom.mixed)
library(eha)
library(ehahelper)
library(ggplot2)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/") #Set working directory

#Load model outputs testing long-term change in sociality post Maria
load("ProxMdlOutput_PrePost_PM.RData")#Longitudinal change in proximity, only considering PM data
load("ProxMdlOutput_PrePost_AM.RData")#Longitudinal change in proximity, only considering AM data


data_combined = data.frame()


#####################
# 1. Nodal strength 
#####################

#AM
mdl.pool.strength.prox<-summary(mice::pool(mdl.strength.proxPrePostAM ))
est = mdl.pool.strength.prox$estimate
se = mdl.pool.strength.prox$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(estimate = data[2,1], lower = data[2,2], upper= data[2,3]);
data$behavior = "proximityAM"; data$metric = "Standardized nodal strength";
data_combined= rbind(data_combined,data)

#PM
mdl.pool.strength.prox<-summary(mice::pool(mdl.strength.proxPrePostPM ))
est = mdl.pool.strength.prox$estimate
se = mdl.pool.strength.prox$std.error
data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
data=data.frame(estimate = data[2,1], lower = data[2,2], upper= data[2,3]);
data$behavior = "proximityPM"; data$metric = "Standardized nodal strength"; 
data_combined= rbind(data_combined,data)


# ###################
# # 2. Nodal degree
# ###################
# 
# #Proximity AM
# mdl.pool.degree.prox<-summary(mice::pool(mdl.degree.proxPrePostAM ))
# est = mdl.pool.degree.prox$estimate
# se = mdl.pool.degree.prox$std.error
# data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
# data=data.frame(estimate = data[2,1], lower = data[2,2], upper= data[2,3]);
# data$behavior = "proximityAM"; data$metric = "Standardized nodal degree"; 
# data_combined= rbind(data_combined,data)
# 
# #Proximity PM
# mdl.pool.degree.prox<-summary(mice::pool(mdl.degree.proxPrePostPM ))
# est = mdl.pool.degree.prox$estimate
# se = mdl.pool.degree.prox$std.error
# data=cbind(est, est-se*qnorm(0.975), est+se*qnorm(0.975))
# data=data.frame(estimate = data[2,1], lower = data[2,2], upper= data[2,3]);
# data$behavior = "proximityPM"; data$metric = "Standardized nodal degree"
# data_combined= rbind(data_combined,data)


##################
# 2. Plot output
##################

ggplot(data_combined, aes(x=behavior, y=estimate)) +        # ggplot2 plot with confidence intervals
  geom_point(size=0.75, shape=21) +
  geom_hline(yintercept =0,colour="black",linetype = "longdash",size=0.25)+
  geom_pointrange(aes(ymin = lower, ymax = upper), size=0.1, shape=22)+
  theme_light()
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Estimates_ProximityLongTermChange_AMvsPM.pdf")
