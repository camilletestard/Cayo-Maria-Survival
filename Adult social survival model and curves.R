###Final Cox PH model, social integration sole covariate###
data<-read.csv(file.choose()) ##Adult data finished##
library(survival)
library(survminer)
scale=365.25
fitsocial<-coxph(Surv(Age_entry.days./Scale, Age_event.days./Scale, Survival==2)~Social_integration,data=data) #Runs a cox PH model with age as the time scale.
summary(fitsocial)

###Survival curve###

dataScurve<-read.csv(file.choose()) ##social survival curve data##
fit2=survfit(Surv(Age_entry.days., Age_event.days., Survival==2)~Social_ord,data=dataScurve)
ggsurvplot(fit2,
           data = dataScurve,
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           pval = FALSE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs =
             c("Below average social integration", "Above average social integration"),    # Change legend labels
           risk.table.height = 0.25,# Useful to change when you have multiple groups
           xlim=c(2500,8000),
           xlab="Age in days",
           break.time.by = 1000,
           ggtheme = theme_bw()
)
###Cumulative hazard and cumulative event####
datahigh<-read.csv(file.choose()) ##High social data##
datalow<-read.csv(file.choose()) ##Low social data##
fith<-survfit(Surv(Age_entry.days., Age_event.days., Survival)~Social_ord, data = datahigh)
fitl<-survfit(Surv(Age_entry.days., Age_event.days., Survival)~Social_ord, data = datalow)
splots<-list()
splots[[1]]<-ggsurvplot(fith, data = datahigh, fun = "event", xlab = "Age in days", xlim = c(2500, 8000), legend.labs = "High social integration", legend = "bottom")
splots[[2]]<-ggsurvplot(fitl, data = datalow, fun = "event", xlab = "Age in days", xlim = c(2500, 8000), legend.labs = "Low social integration", legend = "bottom")
splots[[3]]<-ggsurvplot(fith, data = datahigh, fun = "cumhaz", xlab = "Age in days", xlim = c(2500, 8000), legend.labs = "High social integration", legend = "bottom")
splots[[4]]<-ggsurvplot(fitl, data = datalow, fun = "cumhaz", xlab = "Age in days", xlim = c(2500, 8000), legend.labs = "Low social integration", legend = "bottom")
arrange_ggsurvplots(splots,print=TRUE, ncol=2, nrow=2)

