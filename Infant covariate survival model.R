##Infant survival model##
library(survival)
library(survminer)
Infdata<-read.csv(file.choose()) ##Infant data finished##
fit<-within(Infdata,{    
  Sex<-factor(Sex,labels=c("males","females"))
  Group_size<-factor(Group_size,labels=c("F","KK","V"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are) 
})  ###data is adult data finished###
model<-coxph( Surv(Time, Survival==1) ~ Sex + Maternal_rank + Group_size, data=fit)
summary (model) ##Results from the Cox PH model
ggforest(model,data=fit)


###Survival curve for additional covariate model###
###model with all covariates##
Infdata1<-read.csv(file.choose()) ##Infant data finished##
Y=Surv(Infdata1$Time, Infdata1$Survival==1)
res.coxi<-coxph(Y~Rank_ord+Sex+Group_size,data= Infdata1)
ggsurvplot(survfit(res.coxi),data = Infdata1, color="#2E9FDF", legend.labs= "Model with sex, group size, and mother rank ", xlab= "Age in days", xlim=c(0,365), ggtheme= theme_minimal())


###testing individual covariates###
sex_dfi<-with(Infdata1, 
              data.frame(Sex= c(1, 2),
                         Group_size = c(106,106),
                         Rank_ord= c(1,1)
              )
)

sex_df           
fitsi<-survfit(res.coxi,newdata = sex_dfi)
ggsurvplot(fitsi, data = Infdata1, xlim=c(0,365), xlab="Age in days", conf.int = TRUE, legend.labs=c("Males", "Females"), ggtheme = theme_minimal())

###Group model###
Group_dfi<-with(Infdata1,
                data.frame (Group_size= c(106, 111, 114),
                            Sex= c(1, 1, 1),
                            Rank_ord = c(1,1,1)
                )
)
fitGi<-survfit(res.coxi,newdata = Group_dfi)
ggsurvplot(fitGi, data = Infdata1, xlim=c(0,365), xlab="Age in days", conf.int = TRUE, legend.labs=c("Group KK", " Group V","Group F"), ggtheme = theme_minimal())

###Rank model###
Rank_dfi<-with(Infdata1,
               data.frame (Group_size= c(106, 106, 106),
                           Sex= c(1, 1, 1),
                           Rank_ord= c(1,2,3)
               )
)
fitRi<-survfit(res.coxi,newdata = Rank_dfi)
ggsurvplot(fitRi, data = Infdata1, xlim=c(0,365), xlab="Age in days", conf.int = TRUE, legend.labs=c("High maternal rank", "Average maternal rank","Low maternal rank"), ggtheme = theme_minimal())

##Plotting curves together##
splots<-list()
splots[[1]]<-ggsurvplot(fitsi, data = Infdata1, xlim=c(0,365), xlab="Age in days", conf.int = TRUE, legend.labs=c("Males", "Females"), ggtheme = theme_minimal())
splots[[2]]<-ggsurvplot(fitGi, data = Infdata1, xlim=c(0,365), xlab="Age in days", conf.int = TRUE, legend.labs=c("Group KK", " Group V","Group F"), ggtheme = theme_minimal())
splots[[3]]<-ggsurvplot(fitRi, data = Infdata1, xlim=c(0,365), xlab="Age in days", conf.int = TRUE, legend.labs=c("High maternal rank", "Average maternal rank","Low maternal rank"), ggtheme = theme_minimal())
arrange_ggsurvplots(splots,print=TRUE, ncol=1, nrow=3)
