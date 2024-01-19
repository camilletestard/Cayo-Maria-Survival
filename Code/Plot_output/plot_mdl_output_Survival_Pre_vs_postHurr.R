library(mice)
library(broom.mixed)
library(eha)
library(ehahelper)
library(ggplot2)

#load model outputs
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/") #Set working directory

#Load model outputs testing effect of post-hurricane sociality on survival 
load("ProxMdlOutput_PostHurr_allgroups.RData") #Survival model for the effect of proximity post-hurricane

#Load model outputs testing effect of pre-hurricane sociality on survival 
load("ProxMdlOutput_PreHurr_allgroups.RData")#Survival model for the effect of proximity pre-hurricane


mdl_output<-data.frame(matrix(NA, nrow=4, ncol=5))
names(mdl_output)=c("metric","isPost","estimate","lower","upper")

#Proximity degree prehurr
mdl.pool.degree<-summary(mice::pool(degree.prox.pre))
est = mdl.pool.degree$estimate[1] 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output[1,]=c("degree","prehurr",est,CI[1], CI[2])

#Proximity strength prehurr
mdl.pool.strength<-summary(mice::pool(strength.prox.pre))
est = mdl.pool.strength$estimate[1] 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output[2,]=c("strength","prehurr",est,CI[1], CI[2])

# #Proximity status prehurr
# mdl.pool.status<-summary(mice::pool(status.pre))
# est = mdl.pool.status$estimate[1] 
# se = mdl.pool.status$std.error[1]
# CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
# HR = exp(est)
# HR_CI = exp(CI)
# mdl_output[3,]=c("status","prehurr",est,CI[1], CI[2])

#Proximity degree posthurr
mdl.pool.degree<-summary(mice::pool(degree.prox.post))
est = mdl.pool.degree$estimate[1] 
se = mdl.pool.degree$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output[3,]=c("degree","posthurr",est,CI[1], CI[2])

#Proximity strength posthurr
mdl.pool.strength<-summary(mice::pool(strength.prox.post))
est = mdl.pool.strength$estimate[1] 
se = mdl.pool.strength$std.error[1]
CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
HR = exp(est)
HR_CI = exp(CI)
mdl_output[4,]=c("strength","posthurr",est,CI[1], CI[2])

# #Proximity status posthurr
# mdl.pool.status<-summary(mice::pool(status.post))
# est = mdl.pool.status$estimate[1] 
# se = mdl.pool.status$std.error[1]
# CI = c(est-se*qnorm(0.975), est+se*qnorm(0.975))
# HR = exp(est)
# HR_CI = exp(CI)
# mdl_output[6,]=c("status","posthurr",est,CI[1], CI[2])


mdl_output$full.label<-paste(mdl_output$metric,mdl_output$isPost, sep=".")
mdl_output$estimate=as.numeric(mdl_output$estimate)
mdl_output$lower=as.numeric(mdl_output$lower)
mdl_output$upper=as.numeric(mdl_output$upper)

  mdl_output$full.label=as.factor(mdl_output$full.label)
  mdl_output = mdl_output %>%
    mutate(full.label = fct_relevel(full.label,
                                    "strength.posthurr","degree.posthurr",
                                    "strength.prehurr","degree.prehurr",
                                ))
  # mutate(full.label = fct_relevel(full.label,
  #                                 "strength.posthurr","degree.posthurr", "status.posthurr",
  #                                 "strength.prehurr","degree.prehurr", "status.prehurr"
  # ))
  
  
  ggplot(mdl_output, aes(x=full.label,y=estimate)) +        # ggplot2 plot with confidence intervals
    geom_point(size=3, shape=21, fill="blue") +
    geom_hline(yintercept =0,colour="red",linetype = "longdash",size=1)+
    geom_errorbar(aes(ymin = lower, ymax = upper), colour="black", width=.1)+
    ylim(-1, 1)+
    coord_flip()+theme_light()
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Proximity_survival_pre_vs_post.pdf")

