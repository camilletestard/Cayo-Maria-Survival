###Infant final survival model###
Infdata<-read.csv(file.choose()) ###Infant data finished###

##Test for PH assumption##
Y=Surv(Infdata$Time, Infdata$Survival==1)
modelIn<-coxph(Y~Maternal_social, data =Infdata)
summary(modelIn)
test.ph<-cox.zph(modelIn,transform=rank)
test.ph

##Plotting schoenfeld residual graph##
fitIn<-coxph(Surv(Time, Survival)~ Maternal_social, data = Infdata)
ggcoxdiagnostics(fitIn, type = "schoenfeld", ox.scale = ("time"), hline = TRUE, hline.col = "red", hline.yintercept = 0, hline.lty = "solid", title = "Diagnostic plot")

##Extended Cox PH model##
Infdata<-read.csv(file.choose()) ###Infant data finished###
monkeys.cp=survSplit(Infdata,cut=Infdata$Time[Infdata$Survival==1],end="Time", event="Survival",start="start",id="id") #converts the dataset into a start stop format (multiple observations to allow covariate to change between observatoins)
monkeys.cp$logtsocial=monkeys.cp$Maternal_social*log(monkeys.cp$Time) #creats a time-varying covariate (logsocial) as the product of age and the natural log of time
monkey.model<-coxph(Surv(monkeys.cp$start,monkeys.cp$Time,monkeys.cp$Survival)~logtsocial+cluster(id),data=monkeys.cp) #runs an extended cox model using the parameters 'age', 'social' and 'logtage'(time-varying)
summary(monkey.model) #results for the infant social model

###Survival curve###
InfSdata<-read.csv(file.choose()) ##infant survival curve data##
fit1=survfit(Surv(Time, Survival==1)~Social_ord,data=InfSdata)
ggsurvplot(fit1,
           data = InfSdata,
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           pval = FALSE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs =
             c("Below average maternal social integration", "Above average maternal social integration"),    # Change legend labels
           risk.table.height = 0.25,# Useful to change when you have multiple groups
           xlim=c(0,365),
           xlab="Age in days",
           break.time.by = 50,
           ggtheme = theme_bw()
)

###Cumulative hazard and cumulative event####
datahigh<-read.csv(file.choose()) ##High social data##
datalow<-read.csv(file.choose()) ##Low social data##
fith<-survfit(Surv(Time, Survival==1)~Social_ord, data = datahigh)
fitl<-survfit(Surv(Time, Survival==1)~Social_ord, data = datalow)
splots<-list()
splots[[1]]<-ggsurvplot(fith, data = datahigh, fun = "event", xlab = "Age in days", xlim = c(0, 365), legend.labs = "High maternal social integration", legend = "bottom")
splots[[2]]<-ggsurvplot(fitl, data = datalow, fun = "event", xlab = "Age in days", xlim = c(0, 365), legend.labs = "Low maternal social integration", legend = "bottom")
splots[[3]]<-ggsurvplot(fith, data = datahigh, fun = "cumhaz", xlab = "Age in days", xlim = c(0, 365), legend.labs = "High maternal social integration", legend = "bottom")
splots[[4]]<-ggsurvplot(fitl, data = datalow, fun = "cumhaz", xlab = "Age in days", xlim = c(0, 365), legend.labs = "Low maternal social integration", legend = "bottom")
arrange_ggsurvplots(splots,print=TRUE, ncol=2, nrow=2)
