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
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
load('SocialCapital_changeP_Adults.RData') #input dataframe generated previously.

#Initialize variables
fit.groom.hr=data.frame(matrix(ncol=6)); names(fit.groom.hr)=c("dpSocial","sexF","ordrankM","ordrankH","num_obs","dpSocial:sexF")
fit.prox.hr=data.frame(matrix(ncol=6)); names(fit.prox.hr)=c("dpAcc","sexF","ordrankM","ordrankH","num_obs","dpAcc:sexF")


i=1; max_iter = max(full.data$iter)
for (i in 1:max_iter){
  data= full.data[full.data$iter==i,];
  
  # #Plot the raw data
  # setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results')
  # dpAcc.hist<-ggplot(data,aes(x=dpAcc))+
  #   geom_histogram()+ theme_classic(base_size = 15)+
  #   geom_vline(xintercept = 0, linetype="dashed", color = "red",size=2)+
  #   xlab('Change in p(proximity) pre-to-post hurricane')
  # ggsave("dpAcc.hist.png")
  
  # dpSocial.hist<-ggplot(data,aes(x=dpSocial))+
  #   geom_histogram()+ theme_classic(base_size = 15)+
  #   geom_vline(xintercept = 0, linetype="dashed", color = "red",size=2 )+
  #   xlab('Change in p(grooming) pre-to-post hurricane')
  # ggsave("dpSocial.hist.png")
  # mean(data$dpSocial)
  # 
  # ggplot(data,aes(x=dpAcc, y=dpSocial))+
  #   geom_point()+ xlab('Change in p(proximity)')+ ylab('Change in p(grooming)')+
  #   geom_smooth(method='lm',formula= y~x)+ theme_classic(base_size = 15)
  
  # Plot 
  ggplot(data, aes(x=as.factor(Survival), y=dpAcc))+
    geom_violin()+ xlab('Death Status')+ylab('Change in p(proximity)')+
    geom_boxplot(width=0.25)+ theme_classic(base_size = 15)
  # ggplot(data, aes(x=as.factor(Survival), y=dpSocial)+
  #   geom_violin()+xlab('Death Status')+ylab('Change in p(grooming)')+
  #   geom_boxplot(width=0.25)+ theme_classic(base_size = 15)
  # 
  
  #Format the data
  data<-within(data,{
    sex<-factor(sex,labels=c("M","F"))
    ordrank<-factor(ordrank, labels=c("L","M","H"))
    group<-factor(group,labels=c("V","KK"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
    Age_entry.days<-as.numeric(Age_entry.days)
    Age_event.days<-as.numeric(Age_event.days)
    days.in.study<-as.numeric(days.in.study)
    num_obs<-scale(as.numeric(num_obs))
    dpAcc<- 100*dpAcc
    dpSocial<- 100*dpSocial
    year.prehurr<-as.factor(year.prehurr)
    id<-as.factor(id)
    #Survival<-as.factor(Survival)
  })
  length(which(data$Survival==1))/nrow(data)
  table(data$id, data$year.prehurr)
  
  #Fit the survival models
  try(fitsocial.groom<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~dpSocial*sex+ ordrank+ num_obs+ (1|year.prehurr)+(1|id) ,data=data)) #Runs a cox PH model with age as the time scale.
  summary(fitsocial.groom)
  #cz <- cox.zph(fitsocial.groom)
  #print(cz)
  
  try(fitsocial.prox<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~dpAcc*sex+ ordrank + num_obs +(1|year.prehurr)+(1|id),data=data)) #Runs a cox PH model with age as the time scale.
  summary(fitsocial.prox)
  #cz <- cox.zph(fitsocial.prox)
  #print(cz)
  
  #plot difference in p(proximity) when alive vs. dead
  # data$death.status="Alive"; data$death.status[data$Survival==1]="Dead"
  # ggplot(data, aes(x=death.status, y=dpAcc))+
  #   geom_violin()+
  #   geom_boxplot(width=0.5)+ theme_classic(base_size = 16)+
  #   ylab('Change in proximity pre-to-post hurricane')+ xlab('')
  # mean(data$dpAcc[data$Survival==0]); mean(data$dpAcc[data$Survival==1])
  # sum(data$Survival[data$sex=='F']);sum(data$Survival[data$sex=='M'])
  
  #Combine data from all iterations
  fit.groom.hr = rbind(fit.groom.hr, fixef(fitsocial.groom))
  fit.prox.hr = rbind(fit.prox.hr, fixef(fitsocial.prox))
  #confint(fitsocial.prox); confint(fitsocial.groom)
  print(i)
  
}
# fit.groom.hr[which(fit.groom.hr>2 | fit.groom.hr<(-2),arr.ind=T)]=NA
# fit.groom.hr$sexF[exp(fit.groom.hr$sexF)>1]=NA
# fit.groom.hr$ordrankH[exp(fit.groom.hr$ordrankH)>1]=NA
idx.groom = which(is.na(fit.groom.hr),arr.ind=T)
fit.groom.hr.plot = melt(exp(fit.groom.hr[unique(-idx.groom[,1]),]))

idx.prox = which(is.na(fit.prox.hr),arr.ind=T)
fit.prox.hr.plot =  melt(exp(fit.prox.hr[unique(-idx.prox[,1]),]))

hist(exp(fit.prox.hr$dpAcc), 50, xlab = "Hazard ratio for change in proximity", main = "Histogram of Hazard Ratios for change in proximity")
hist(exp(fit.groom.hr$dpSocial), 50, xlab = "Hazard ratio for change in grooming", main = "Histogram of Hazard Ratios for change in proximity")
mean(exp(fit.prox.hr$dpAcc), na.rm=T); quantile(exp(fit.prox.hr$dpAcc),probs = c(0.025, 0.975), na.rm = T)
mean(exp(fit.groom.hr$dpSocial), na.rm=T); quantile(exp(fit.groom.hr$dpSocial),probs = c(0.025, 0.975), na.rm = T)

prox.plot<-ggplot(fit.prox.hr.plot, aes(x=variable, y=value, color=variable))+
  geom_violin()+
  geom_hline(yintercept =1)+
  #geom_jitter(width=0.15, alpha=0.5)+
  labs(x='Hazard ratios') +
  theme_classic(base_size = 15)+ theme(legend.position = "none")+
  title(main="Hazard ratios for change in p(proximity) models")+
  theme(axis.text.y = element_text(angle = 30))+
  coord_flip()
  
# vioplot(exp(fit.prox.hr.plot$dpAcc),exp(fit.prox.hr.plot$sexF),exp(fit.prox.hr.plot$`dpAcc:sexF`),
#         exp(fit.prox.hr.plot$ordrankM),exp(fit.prox.hr.plot$ordrankH),exp(fit.prox.hr.plot$num_obs),
#         at=c(1,2,3,4,5,6),#outline=FALSE,
#         horizontal =T, main="Hazard ratios for change in p(proximity) models",
#         cex.axis =0.6,cex.main =1,cex.names =1,
#         na.rm=T,
#         coef = 1,
#         names=c("dp(prox)","sexF","dp(prox):sexF","ordrankM","ordankH", "NumObs."))
# segments(1,0.2,1,10.8, col = "Red", lty=5, lwd=2)

groom.plot<-ggplot(fit.groom.hr.plot, aes(x=variable, y=value, color=variable))+
  geom_violin()+
  geom_hline(yintercept =1)+
  #geom_jitter(width=0.15, alpha=0.5)+
  labs(x='Hazard ratios') +
  theme_classic(base_size = 15)+ theme(legend.position = "none")+
  title(main="Hazard ratios for change in p(groom) models")+
  theme(axis.text.y = element_text(angle = 30))+
  coord_flip()

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
