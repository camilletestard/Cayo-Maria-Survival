library(mice)
library(broom.mixed)
library(eha)
library(ehahelper)
library(ggplot2)

#load model outputs
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/") #Set working directory

#Load model outputs testing effect of post-hurricane sociality on survival 
load("ProxMdlOutput_PostHurr.RData") #Survival model for the effect of proximity post-hurricane
load("GroomMdlOutput_PostHurr.RData")#Survival model for the effect of grooming post-hurricane

#Load model outputs testing effect of pre-hurricane sociality on survival 
load("ProxMdlOutput_PreHurr.RData")#Survival model for the effect of proximity pre-hurricane
load("GroomMdlOutput_PreHurr.RData")#Survival model for the effect of grooming pre-hurricane

#Load model outputs testing effect of change in sociality after Maria on survival 
load("MdlOutput_ChangeGrooming.RData")#Survival model for the effect of short term change in grooming pre-to-post-hurricane
load("MdlOutput_ChangeProximity.RData")#Survival model for the effect of short term change in proximity pre-to-post-hurricane


##################
#### PROXIMITY ###
##################

mdl_output_proximity<-data.frame(matrix(NA, nrow=5, ncol=6))
names(mdl_output_proximity)=c("metric","behavior","model","estimate","lower","upper")

#Proximity degree prehurr
mdl.pool.degree<-summary(mice::pool(degree.prox.pre))
est = mdl.pool.degree$estimate[1] 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_proximity[1,]=c("degree","prox","prehurr",est,CI[1], CI[2])

#Proximity strength prehurr
mdl.pool.strength<-summary(mice::pool(strength.prox.pre))
est = mdl.pool.strength$estimate[1] 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_proximity[2,]=c("strength","prox","prehurr",est,CI[1], CI[2])

#Proximity degree posthurr
mdl.pool.degree<-summary(mice::pool(degree.prox.post))
est = mdl.pool.degree$estimate[1] 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_proximity[3,]=c("degree","prox","posthurr",est,CI[1], CI[2])

#Proximity strength posthurr
mdl.pool.strength<-summary(mice::pool(strength.prox.post))
est = mdl.pool.strength$estimate[1] 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_proximity[4,]=c("strength","prox","posthurr",est,CI[1], CI[2])

#Change in proximity
mdl.pool.strength<-summary(mice::pool(strength.dprox))
est = mdl.pool.strength$estimate[1] 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_proximity[5,]=c("strength","prox","prePost",est,CI[1], CI[2])


mdl_output_proximity$full.label<-paste(mdl_output_proximity$metric,mdl_output_proximity$behavior,mdl_output_proximity$model, sep=".")
mdl_output_proximity$estimate=as.numeric(mdl_output_proximity$estimate)
mdl_output_proximity$lower=as.numeric(mdl_output_proximity$lower)
mdl_output_proximity$upper=as.numeric(mdl_output_proximity$upper)

  mdl_output_proximity$full.label=as.factor(mdl_output_proximity$full.label)
  mdl_output_proximity = mdl_output_proximity %>%
    mutate(full.label = fct_relevel(full.label,
                                    "strength.prox.posthurr","degree.prox.posthurr", 
                                    "strength.prox.prePost",
                                    "strength.prox.prehurr","degree.prox.prehurr"
                                ))
  
  
  ggplot(mdl_output_proximity, aes(x=full.label,y=estimate)) +        # ggplot2 plot with confidence intervals
    geom_point(size=3, shape=21, fill="blue") +
    geom_hline(yintercept =0,colour="red",linetype = "longdash",size=1)+
    geom_errorbar(aes(ymin = lower, ymax = upper), colour="black", width=.1)+
    ylim(-3, 3)+
    coord_flip()+theme_light()
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
#ggsave("Proximity_survival.pdf")


##################
#### GROOMING ####
##################

mdl_output_groom<-data.frame(matrix(NA, nrow=5, ncol=6))
names(mdl_output_groom)=c("metric","behavior","model","estimate","lower","upper")

#Groom degree prehurr
mdl.pool.degree<-summary(mice::pool(degree.groom.pre))
est = mdl.pool.degree$estimate[1] 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_groom[1,]=c("degree","groom","prehurr",est,CI[1], CI[2])

#Groom strength prehurr
mdl.pool.strength<-summary(mice::pool(strength.groom.pre))
est = mdl.pool.strength$estimate[1] 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_groom[2,]=c("strength","groom","prehurr",est,CI[1], CI[2])

#Groomdegree posthurr
mdl.pool.degree<-summary(mice::pool(degree.groom.post))
est = mdl.pool.degree$estimate[1] 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_groom[3,]=c("degree","groom","posthurr",est,CI[1], CI[2])

#Groom strength posthurr
mdl.pool.strength<-summary(mice::pool(strength.groom.post))
est = mdl.pool.strength$estimate[1] 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_groom[4,]=c("strength","groom","posthurr",est,CI[1], CI[2])

#Groom degree prepost
mdl.pool.degree<-summary(mice::pool(degree.dgroom))
est = mdl.pool.degree$estimate[1] 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output_groom[5,]=c("degree","groom","prepost",est,CI[1], CI[2])

mdl_output_groom$full.label<-paste(mdl_output_groom$metric,mdl_output_groom$behavior,mdl_output_groom$model, sep=".")
mdl_output_groom$estimate=as.numeric(mdl_output_groom$estimate)
mdl_output_groom$lower=as.numeric(mdl_output_groom$lower)
mdl_output_groom$upper=as.numeric(mdl_output_groom$upper)

mdl_output_groom$full.label=as.factor(mdl_output_groom$full.label)
mdl_output_groom = mdl_output_groom %>%
  mutate(full.label = fct_relevel(full.label,
                                  "degree.groom.posthurr", "strength.groom.posthurr",
                                  "degree.groom.prepost",
                                  "degree.groom.prehurr", "strength.groom.prehurr"))


ggplot(mdl_output_groom, aes(x=full.label,y=estimate)) +        # ggplot2 plot with confidence intervals
  geom_point(size=3, shape=21, fill="blue") +
  geom_hline(yintercept =0,colour="red",linetype = "longdash",size=1)+
  geom_errorbar(aes(ymin = lower, ymax = upper), colour="black", width=.1)+ 
  ylim(-3, 3)+
  coord_flip()+theme_light()
#ggsave("Grooming_survival.pdf")
