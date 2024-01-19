#Adult_Survival_ChangePrePost_allYearsPre.R
#This script runs survival models on adults to test whether change in sociality
#pre-to-post hurricane predicts survival up to 4 years after hurricane Maria.
#Input: SocialCapital_changeP_Adults.RData
#Camille Testard Feb. 2022, adapted from Chloe Shergold code.

#Load libraries:
library(survival)
library(survminer)
library(coxme)
library(ggplot2)
library(matrixStats)
library(gridExtra) 
library(graphics)
library(vioplot)
library(reshape2)

#############################################
# Change in sociality pre-to-post hurricane
#############################################
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data')
load('SocialCapital_changeP_Adults.RData') #input dataframe generated previously.

#Initialize variables
fit.groom.hr=data.frame(matrix(ncol=3)); names(fit.groom.hr)=c("dpSocial","groupV","sexF")
fit.prox.hr=data.frame(matrix(ncol=3)); names(fit.prox.hr)=c("dpAcc","groupV","sexF")

# full.data$Survival=as.factor(full.data$Survival)
# ggplot(full.data,aes(x=Survival, y=dpAcc))+
#   geom_violin()
# ggplot(full.data,aes(x=Survival, y=dpSocial))+
#   geom_violin()


i=1; max_iter = max(full.data$iter)
for (i in 1:max_iter){
  data= full.data[full.data$iter==i,];
  #data= full.data[full.data$iter<51,];
  
    
  #Format the data
  data<-within(data,{
    Survival<-as.factor(Survival)
    sex<-factor(sex, levels = c("M", "F"))
    ordrank<-factor(ordrank, levels = c("L", "M", "H"))
    group<-as.factor(group)  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
    Age_entry.days<-as.numeric(Age_entry.days)
    Age_event.days<-as.numeric(Age_event.days)
    days.in.study<-as.numeric(days.in.study)
    num_obs<-scale(as.numeric(num_obs))
    dpAcc<- 100*dpAcc
    dpSocial<- 100*dpSocial
    year.prehurr<-as.factor(year.prehurr)
    id<-as.factor(id)
    iter<-as.factor(iter)
  })
  
  data$dpAcc.cat <- cut(data$dpAcc,
                        breaks=as.numeric(quantile(data$dpAcc,probs = seq(0, 1, 0.33))),
                        labels=c('Small', 'Medium', 'High'))
  data$dpSocial.cat <- cut(data$dpSocial,
                           breaks=as.numeric(quantile(data$dpSocial,probs = seq(0, 1, 0.33))),
                           labels=c('Small', 'Medium', 'High'))

  #Fit the survival models
  try(fitsocial.groom<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~dpSocial +group +sex +(1|id) +(1|year.prehurr),data=data)) #Runs a cox PH model with age as the time scale.
  #summary(fitsocial.groom)
  # cz <- cox.zph(fitsocial.groom)
  # print(cz)
  
  try(fitsocial.prox<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~dpAcc +group +sex +(1|id) +(1|year.prehurr),data=data)) #Runs a cox PH model with age as the time scale.
  #summary(fitsocial.prox)
  # cz <- cox.zph(fitsocial.prox)
  # print(cz)
  
  
  #Combine data from all iterations
  #if (all(fixef(fitsocial.groom))<3){
  fit.groom.hr = rbind(fit.groom.hr, fixef(fitsocial.groom))
  fit.prox.hr = rbind(fit.prox.hr, fixef(fitsocial.prox))
  #}
  #confint(fitsocial.prox); confint(fitsocial.groom)
  print(i)
  
}

save(fit.groom.hr, fit.prox.hr,file ="~/Documents/GitHub/Cayo-Maria-Survival/R.Data/SurvivalAdults_PrePost.RData")
load("~/Documents/GitHub/Cayo-Maria-Survival/R.Data/SurvivalAdults_PrePost.RData")

### PLOT SURVIVAL ###

#Remove NAs
idx.na.groom = which(is.na(fit.groom.hr),arr.ind=T)
fit.groom.hr = fit.groom.hr[unique(-idx.na.groom),]

idx.na.prox = which(is.na(fit.prox.hr),arr.ind=T)
fit.prox.hr = fit.prox.hr[unique(-idx.na.prox),]

#Remove extreme outliers due to model misfit of model
idx.outlier.groom = which(abs(fit.groom.hr) > 3, arr.ind = T)
fit.groom.hr=fit.groom.hr[-idx.outlier.groom[,1],]
fit.groom.hr.exp=exp(fit.groom.hr)
fit.groom.hr.plot = melt(fit.groom.hr.exp)

idx.outlier.prox = which(abs(fit.prox.hr) > 3, arr.ind = T)
fit.prox.hr=fit.prox.hr[-idx.outlier.prox[,1],]
fit.prox.hr.exp = exp(fit.prox.hr)
fit.prox.hr.plot =  melt(fit.prox.hr.exp)

hist(fit.prox.hr.exp$dpAcc, 50, xlab = "Hazard ratio for change in proximity", main = "Histogram of Hazard Ratios for change in proximity")
hist(fit.groom.hr.exp$dpSocial, 65, xlab = "Hazard ratio for change in grooming", main = "Histogram of Hazard Ratios for change in grooming")
median(fit.prox.hr.exp$dpAcc, na.rm=T); quantile(exp(fit.prox.hr$dpAcc),probs = c(0.025, 0.975), na.rm = T)
median(fit.groom.hr.exp$dpSocial, na.rm=T); quantile(exp(fit.groom.hr$dpSocial),probs = c(0.025, 0.975), na.rm = T)

ggplot(fit.prox.hr.plot, aes(x=variable, y=value, color=variable))+
  geom_violin()+
  geom_hline(yintercept =1)+
  #geom_jitter(width=0.15, alpha=0.5)+
  labs(x='Hazard ratios') +
  theme_classic(base_size = 15)+ theme(legend.position = "none")+
  title(main="Hazard ratios for change in p(proximity) models")+
  theme(axis.text.y = element_text(angle = 30))+
  coord_flip()
Means = colMeans2(as.matrix(exp(fit.prox.hr))); Means = round(Means,3)
CI = colQuantiles(as.matrix(exp(fit.prox.hr)), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc<-tableGrob(Estimates);t.Acc<-grid.arrange(t.Acc, top="p(Prox) Model (Group KK)");
#write.csv(Estimates,"pprox.KK.csv")


  
# vioplot(exp(fit.prox.hr.plot$dpAcc),exp(fit.prox.hr.plot$sexF),exp(fit.prox.hr.plot$`dpAcc:sexF`),
#         exp(fit.prox.hr.plot$ordrankM),exp(fit.prox.hr.plot$ordrankH),exp(fit.prox.hr.plot$num_obs),
#         at=c(1,2,3,4,5,6),#outline=FALSE,
#         horizontal =T, main="Hazard ratios for change in p(proximity) models",
#         cex.axis =0.6,cex.main =1,cex.names =1,
#         na.rm=T,
#         coef = 1,
#         names=c("dp(prox)","sexF","dp(prox):sexF","ordrankM","ordankH", "NumObs."))
# segments(1,0.2,1,10.8, col = "Red", lty=5, lwd=2)

ggplot(fit.groom.hr.plot, aes(x=variable, y=value, color=variable))+
  geom_violin()+
  geom_hline(yintercept =1)+
  #geom_jitter(width=0.15, alpha=0.5)+
  labs(x='Hazard ratios') +
  theme_classic(base_size = 15)+ theme(legend.position = "none")+
  title(main="Hazard ratios for change in p(groom) models")+
  theme(axis.text.y = element_text(angle = 30))+
  coord_flip()
Means = colMeans2(as.matrix(exp(fit.groom.hr))); Means = round(Means,3)
CI = colQuantiles(as.matrix(exp(fit.groom.hr)), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.groom<-tableGrob(Estimates);t.groom<-grid.arrange(t.groom, top="p(groom) Model (Group KK)");

# vioplot(exp(fit.groom.hr.plot$dpSocial),exp(fit.groom.hr.plot$sexF),exp(fit.groom.hr.plot$`dpSocial:sexF`),
#         exp(fit.groom.hr.plot$ordrankM),exp(fit.groom.hr.plot$ordrankH),exp(fit.groom.hr.plot$num_obs),
#         at=c(1,2,3,4,5,6),#outline=FALSE,
#         horizontal =T, main="Hazard ratios for change in p(grooming) models",
#         cex.axis =0.6,cex.main =1,cex.names =1,
#         na.rm=T,
#         coef = 0.1,
#         names=c("dp(groom)","sexF","dp(groom):sex","ordrankM","ordrankH", "NumObs."))
# segments(1,0.2,1,10.8, col = "Red", lty=5, lwd=2)

ggpubr::ggarrange(prox.plot,groom.plot,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# ### Survival curves ###
# 
# # Plot the baseline survival function
# ggsurvplot(survfit(fitsocial.prox), color = "#2E9FDF",
#            ggtheme = theme_minimal(), data=data)
# 
# data$dpAcc.factor = 1
# data$dpAcc.factor[data$dpAcc>median(data$dpAcc)]=2;
# data$dpAcc.factor = as.factor(data$dpAcc.factor)
# 
# fit2=survfit(Surv(Age_entry.days, Age_event.days, Survival)~dpAcc.factor,data=data)
# ggsurvplot(fit2,
#            data = data,
#            size = 1,                 # change line size
#            palette =
#              c("#E7B800", "#2E9FDF"),# custom color palettes
#            conf.int = TRUE,          # Add confidence interval
#            pval = TRUE,              # Add p-value
#            risk.table = FALSE,        # Add risk table
#            risk.table.col = "strata",# Risk table color by groups
#            legend.labs =
#              c("Below average #partners", "Above average #partners"),    # Change legend labels
#            risk.table.height = 0.25,# Useful to change when you have multiple groups
#            xlim=c(2500,8000),
#            xlab="Age in days",
#            break.time.by = 1000,
#            ggtheme = theme_bw()
# )
