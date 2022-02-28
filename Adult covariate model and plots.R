setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
#data.chloe<-read.csv(file.choose()) ##Adult data finished##
load('SocialCapital_Adults.RData')
data= SocialCapital.ALL[SocialCapital.ALL$group!="KK",]
length(which(data$Survival==1))/nrow(data)
Scale = 365.25

library(survival)
library(survminer)
library(coxme)

###Cox PH model with the covariates group size, sex, and rank###
#Format data
data<-within(data,{
  sex<-factor(sex,labels=c("M","F"))
  ordrank<-factor(ordrank, labels=c("L","M","H"))
  group<-factor(group,labels=c("F","V"))  ##Informs the model of the levels within the catagorical covariates (helps identify where the differences are)
  Age_entry.days<-as.numeric(Age_entry.days)
  Age_event.days<-as.numeric(Age_event.days)
})  ###data is adult data finished###

model<-coxph(Surv(Age_entry.days/Scale, Age_event.days/Scale, Survival) ~ sex + ordrank + group.size, data=data)
summary (model) ##Results from the Cox PH model
ggforest(model,data=data)

model2<-coxme(Surv(Age_entry.days/Scale, Age_event.days/Scale, Survival) ~ sex + ordrank + group.size + (1|year.prehurr), data=data)
summary (model2)
ggforest(model,data=data)

#Check assumptions
cz <- cox.zph(model2)
print(cz)
ggcoxzph(cz)
ggcoxdiagnostics(model2, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

###Survival curves for additional covariate model###
##Survival curve for model with the three covariates combined##
Plot1<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~ordrank+ sex+ group.size, data=data)
ggsurvplot(survfit(Plot1),data = data, color="#2E9FDF", legend.labs= "Model with sex, group size, and rank ", xlab= "Age in days", xlim=c(2500,8000), ggtheme= theme_minimal())

###modelling individual covariates###
res.coxr<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~ordrank+ sex+ group.size, data=data)##Same Cox PH model as above, levels not specified as done below)
##Sex model##
sex_df<-with(data,
             data.frame(sex= c("M","F"), #identifies the levels within the variable sex
                        group.size = c(447,447), #Holds the effects of Group size constant at lowest value for both sexes
                        ordrank= c("L","L")#Holds the effects of rank constant at lowest value for both sexes
             )
)

sex_df
fitsex<-survfit(res.coxr, newdata = sex_df)
ggsurvplot(fitsex, data = data, xlim=c(2500,8000), xlab="Age in days", conf.int = TRUE, legend ="bottom", legend.labs=c("Males", "Females"), ggtheme = theme_minimal())

###Group model###
Group_df<-with(data,
               data.frame (group.size= unique(data$group.size), #identifies levels within group size
                           sex= c("F","F"), #Holds the effect of sex constant across groups
                           ordrank= c("L","L") #Holds the effects of rank constant aross groups
               )
)
fitG<-survfit(res.coxr,newdata = Group_df)
ggsurvplot(fitG, data = data, xlim=c(2500,8000), xlab="Age in days", conf.int = TRUE, legend="bottom", legend.labs=c("Group V","Group F"), ggtheme = theme_minimal())

###Rank model###
Rank_df<-with(data,
              data.frame (group.size= c(447,447, 447),  #Holds the effects of Group size constant at lowest value for all ranks
                          sex= c("F","F","F"), #Holds the effect of sex constant across groups
                          ordrank= c("L","M","H") #identifies the levels within rank (high, median, low)
              )
)
fitR<-survfit(res.coxr,newdata = Rank_df)
ggsurvplot(fitR, data = data, xlim=c(2500,8000), xlab="Age in days", conf.int = TRUE, legend = "bottom", legend.labs=c("Low rank", "Middle rank","High rank"), ggtheme = theme_minimal())

##Plotting curves together##
splots<-list()
splots[[1]]<-ggsurvplot(fitsex, data = data, xlim=c(2500,8000), xlab="Age in days", conf.int = TRUE, legend ="bottom", legend.labs=c("Males", "Females"), ggtheme = theme_minimal())
splots[[2]]<-ggsurvplot(fitG, data = data, xlim=c(2500,8000), xlab="Age in days", conf.int = TRUE, legend="bottom", legend.labs=c("Group KK", " Group V","Group F"), ggtheme = theme_minimal())
splots[[3]]<-ggsurvplot(fitR, data = data, xlim=c(2500,8000), xlab="Age in days", conf.int = TRUE, legend = "bottom", legend.labs=c("High rank", "Median rank","Low rank"), ggtheme = theme_minimal())
arrange_ggsurvplots(splots,print=TRUE, ncol=1, nrow=3)
