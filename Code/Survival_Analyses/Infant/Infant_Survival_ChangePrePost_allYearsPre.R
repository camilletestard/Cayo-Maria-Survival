#Infant_Survival_ChangePrePost_allYearsPre.R
#This script runs survival models on infants to test whether change in sociality
#pre-to-post hurricane predicts infant survival up to 4 years after hurricane Maria.
#Input: SocialCapital_changeP_Infants.RData
#Camille Testard Feb. 2022, adapted from Chloe Shergold code.

#Load libraries:
library(survival)
library(survminer)
library(coxme)

#############################################
# Change in sociality pre-to-post hurricane
#############################################
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
load('SocialCapital_changeP_infants.RData') #input dataframe generated previously.
#data=full.data

#Initialize variables
fit.groom.hr=data.frame(matrix(ncol=6)); names(fit.groom.hr)=c("mom.dpSocial","sexF","mom.ordrankM","mom.ordrankH","num_obs")
fit.prox.hr=data.frame(matrix(ncol=6)); names(fit.prox.hr)=c("mom.dpAcc","sexF","mom.ordrankM","mom.ordrankH","num_obs")

i=1
for (i in 1:max(full.data$iter)){
  data= full.data[full.data$iter==i,];
  
  data<-within(data,{
    sex<-factor(sex, levels = c("MALE", "FEMALE"))
    mom.ordrank<-factor(mom.ordrank, levels = c("L", "M", "H"))
    group<-as.factor(group)  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
    Age_entry.days<-as.numeric(Age_entry.days)
    Age_event.days<-as.numeric(Age_event.days)
    days.in.study<-as.numeric(days.in.study)
    num_obs<-scale(as.numeric(num_obs))
    mom.dpAcc<- 100*mom.dpAcc
    mom.dpSocial<- 100*mom.dpSocial
    year.prehurr<-as.factor(year.prehurr)
    id<-as.factor(id)
    iter<-as.factor(iter)
    #Survival<-as.factor(Survival)
  })
  length(which(data$Survival==1))/nrow(data)
  
  fitsocial.groom<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~mom.dpSocial+sex+mom.ordrank+num_obs+ (1|year.prehurr)+(1|id),data=data) #Runs a cox PH model with age as the time scale.
  summary(fitsocial.groom)
  cz <- cox.zph(fitsocial.groom) #Check model assumptions
  #print(cz)
  
  fitsocial.prox<-coxme(Surv(Age_entry.days, Age_event.days, Survival)~mom.dpAcc+sex+mom.ordrank+ num_obs+(1|year.prehurr)+(1|id),data=data) #Runs a cox PH model with age as the time scale.
  summary(fitsocial.prox)
  #cz <- cox.zph(fitsocial.prox)
  #print(cz)

  if (all(fixef(fitsocial.groom))<4){
  fit.groom.hr = rbind(fit.groom.hr, fixef(fitsocial.groom))
  fit.prox.hr = rbind(fit.prox.hr, fixef(fitsocial.prox))}
  
  print(i)
  
}

#Remove NAs
idx.na.groom = which(is.na(fit.groom.hr),arr.ind=T)
fit.groom.hr = fit.groom.hr[unique(-idx.na.groom),]

idx.na.prox = which(is.na(fit.prox.hr),arr.ind=T)
fit.prox.hr = fit.prox.hr[unique(-idx.na.prox),]

#Remove extreme outliers due to model misfit
outlier_thresh = 3 #more than 3 sd away from mean
ZscoreGroom <- apply(fit.groom.hr, 2, function(x) (x - mean(x)) / sd(x))
idx.outlier.groom = which(abs(ZscoreGroom) > 3, arr.ind = T)
fit.groom.hr.exp=exp(fit.groom.hr[unique(-idx.outlier.groom[,1]),])
fit.groom.hr.plot = melt(fit.groom.hr.exp)

ZscoreProx <- apply(fit.prox.hr, 2, function(x) (x - mean(x)) / sd(x))
idx.outlier.prox = which(abs(ZscoreProx) > 3, arr.ind = T)
fit.prox.hr.exp = exp(fit.prox.hr[unique(-idx.outlier.prox[,1]),])
fit.prox.hr.plot =  melt(fit.prox.hr.exp)

hist(fit.prox.hr.exp$mom.dpAcc, 50, xlab = "Hazard ratio for change in proximity", main = "Histogram of Hazard Ratios for change in proximity")
hist(fit.groom.hr.exp$mom.dpSocial, 50, xlab = "Hazard ratio for change in grooming", main = "Histogram of Hazard Ratios for change in grooming")
mean(fit.prox.hr.exp$mom.dpAcc, na.rm=T); quantile(exp(fit.prox.hr$mom.dpAcc),probs = c(0.025, 0.975), na.rm = T)
mean(fit.groom.hr.exp$mom.dpSocial, na.rm=T); quantile(exp(fit.groom.hr$mom.dpSocial),probs = c(0.025, 0.975), na.rm = T)

prox.plot<-ggplot(fit.prox.hr.plot, aes(x=variable, y=value, color=variable))+
  geom_violin()+
  geom_hline(yintercept =1)+
  #geom_jitter(width=0.15, alpha=0.5)+
  labs(x='Hazard ratios') +
  theme_classic(base_size = 15)+ theme(legend.position = "none")+
  title(main="Hazard ratios for change in p(proximity) models")+
  theme(axis.text.y = element_text(angle = 30))+
  coord_flip()