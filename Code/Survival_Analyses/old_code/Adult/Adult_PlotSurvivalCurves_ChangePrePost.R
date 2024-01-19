#Adult_PlotSurvivalCurves_ChangePrePost.R

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

#Load data
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data')
load('SocialCapital_changeP_Adults.RData') #input dataframe generated previously.

i=1;  data= full.data[full.data$iter==i,];
#i=100;  data= full.data[full.data$iter<i,];
#data = data[data$year.prehurr==2017,]

#Format the data
data<-within(data,{
  group<-as.factor(group)  ##Informs the model of the levels within the categorical covariates (helps identify where the differences are)
  sex<-as.factor(sex)
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

###Plot the raw data ###
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results')
ggplot(data,aes(x=dpAcc))+
  geom_histogram()+ theme_classic(base_size = 15)+
  geom_vline(xintercept = 0, linetype="dashed", color = "red",size=2)+
  xlab('Change in p(proximity) pre-to-post hurricane')
ggsave("dpAcc.hist.png")

ggplot(data,aes(x=dpSocial))+
  geom_histogram()+ theme_classic(base_size = 15)+
  geom_vline(xintercept = 0, linetype="dashed", color = "red",size=2 )+
  xlab('Change in p(grooming) pre-to-post hurricane')
ggsave("dpSocial.hist.png")
mean(data$dpSocial)

ggplot(data,aes(x=dpAcc, y=dpSocial))+
  geom_point()+ xlab('Change in p(proximity)')+ ylab('Change in p(grooming)')+
  geom_smooth(method='lm',formula= y~x)+ theme_classic(base_size = 15)

# Plot raw survival difference according to change in p(social)
setwd('~/Documents/Github/Cayo-Maria-Survival/Results/')
ggplot(data, aes(x=as.factor(Survival), y=dpAcc))+
  geom_violin()+xlab('')+ylab('Change in p(proximity) pre-to-post hurricane')+
  geom_boxplot(width=0.25)+ 
  geom_jitter(width=0.1,size = 2, alpha=0.7)+
  theme_classic(base_size = 15)+ scale_x_discrete(labels= c("alive","dead"))
  #+facet_grid(~group)
ggsave("dpProx_Survival_Violin.png")
mean(data$dpAcc[data$Survival==0]); mean(data$dpAcc[data$Survival==1])

sum(data$Survival[data$sex=='F'])/length(which(data$sex=='F'));sum(data$Survival[data$sex=='M'])/length(which(data$sex=='M'))

sum(data$Survival[data$group=='KK'])/length(which(data$group=='KK'));sum(data$Survival[data$group=='V'])/length(which(data$group=='V'))

ggplot(data, aes(x=as.factor(Survival), y=dpSocial))+
  geom_violin()+xlab('')+ylab('Change in p(grooming)')+
  geom_boxplot(width=0.25)+ 
  geom_jitter(width=0.1,size = 2, alpha=0.7)+ 
  theme_classic(base_size = 15)+ scale_x_discrete(labels= c("alive","dead"))
ggsave("dpSocial_Survival_Violin.png")
mean(data$dpSocial[data$Survival==0]); mean(data$dpSocial[data$Survival==1])


####  Plot survival curves ####

#####################################################
### PROXIMITY ###
#####################################################
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/Survival_curves/")

surv.prox<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~dpAcc.cat + sex+ group,data=data)
summary(surv.prox)

##dpAcc model##
dpAcc_df<-with(data,
               data.frame(dpAcc.cat =c("Small", "Medium", "High"),
                          sex= c("F","F","F"), #identifies the levels within the variable sex
                          group = c("V","V","V")#, #Holds the effects of Group size constant at lowest value for both sexes
                          #ordrank= c("H","H","H"),#Holds the effects of rank constant at lowest value for both sexes
                          #year.prehurr = c("2016", "2016", "2016")
               )
)
dpAcc_df
fitAcc<-survfit(surv.prox, newdata = dpAcc_df)
ggsurvplot(fitAcc, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(proximity)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())


##sex model##
sex_df<-with(data,
             data.frame(dpAcc.cat =c("Small", "Small"),
                        sex= c("M","F"), #identifies the levels within the variable sex
                        group = c("V","V")#, #Holds the effects of Group size constant at lowest value for both sexes
                        #ordrank= c("L","L"),#Holds the effects of rank constant at lowest value for both sexes
                        #year.prehurr = c("2016", "2016")
             )
)
sex_df
fitSex<-survfit(surv.prox, newdata = sex_df)
ggsurvplot(fitSex, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Sex",legend.labs=c("M", "F"), ggtheme = theme_minimal())


###Group model###
group_df<-with(data,
               data.frame(dpAcc.cat =c("Small", "Small"),
                          sex= c("F","F"), #identifies the levels within the variable sex
                          group = c("V","KK"), #Holds the effects of Group size constant at lowest value for both sexes
                          ordrank= c("L","L"),#Holds the effects of rank constant at lowest value for both sexes
                          year.prehurr = c("2016", "2016")
               )
)
fitG<-survfit(surv.prox,newdata = group_df)
ggsurvplot(fitSex, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Group",legend.labs=c("V", "KK"), ggtheme = theme_minimal())


###Rank model###
# Rank_df<-with(data,
#               data.frame (dpAcc.cat =c("Small", "Small","Small"),
#                           sex= c("F","F","F"), #identifies the levels within the variable sex
#                           group = c("V","V","V"), #Holds the effect of sex constant across groups
#                           ordrank= c("L","M","H"), #identifies the levels within rank (high, median, low)
#                           year.prehurr = c("2016", "2016", "2016")
#               )
# )
# fitR<-survfit(surv.prox,newdata = Rank_df)

##Plotting curves together##
splots<-list()
splots[[1]]<-ggsurvplot(fitAcc, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(proximity)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())
splots[[2]]<-ggsurvplot(fitSex, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "sex", legend.labs=c("Male", "Female"), ggtheme = theme_minimal())
splots[[3]]<-ggsurvplot(fitG, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend="bottom", legend.title = "Group",legend.labs=c("V","KK"), ggtheme = theme_minimal())
#splots[[4]]<-ggsurvplot(fitR, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend = "bottom", legend.title = "Rank",legend.labs=c("Low", "Middle","High"), ggtheme = theme_minimal())
arrange_ggsurvplots(splots,print=TRUE, ncol=3, nrow=1, title="Survival effects of change in p(proximity)")
ggsave("dpAcc.SurvivalCurves.png")

#####################################################
### GROOMING ###
#####################################################

surv.groom<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~dpSocial.cat + sex+ ordrank+ group,data=data)
summary(surv.groom)

##dpSocialmodel##
dpSocial_df<-with(data,
               data.frame(dpSocial.cat =c("Small", "Medium", "High"),
                          sex= c("F","F","F"), #identifies the levels within the variable sex
                          group = c("V","V","V"), #Holds the effects of Group size constant at lowest value for both sexes
                          ordrank= c("L","L","L")#Holds the effects of rank constant at lowest value for both sexes
               )
)
dpSocial_df
fitSocial<-survfit(surv.groom, newdata = dpSocial_df)


##sex model##
sex_df<-with(data,
             data.frame(dpSocial.cat =c("Small", "Small"),
                        sex= c("M","F"), #identifies the levels within the variable sex
                        group = c("V","V"), #Holds the effects of Group size constant at lowest value for both sexes
                        ordrank= c("L","L")#Holds the effects of rank constant at lowest value for both sexes
             )
)
sex_df
fitSex<-survfit(surv.groom, newdata = sex_df)


###Group model###
group_df<-with(data,
               data.frame(dpSocial.cat =c("Small", "Small"),
                          sex= c("F","F"), #identifies the levels within the variable sex
                          group = c("V","KK"), #Holds the effects of Group size constant at lowest value for both sexes
                          ordrank= c("L","L")#Holds the effects of rank constant at lowest value for both sexes
               )
)
fitG<-survfit(surv.groom,newdata = group_df)

###Rank model###
Rank_df<-with(data,
              data.frame (dpSocial.cat =c("Small", "Small","Small"),
                          sex= c("F","F","F"), #identifies the levels within the variable sex
                          group = c("V","V","V"), #Holds the effect of sex constant across groups
                          ordrank= c("L","M","H") #identifies the levels within rank (high, median, low)
              )
)
fitR<-survfit(surv.groom,newdata = Rank_df)

##Plotting curves together##
splots<-list()
splots[[1]]<-ggsurvplot(fitSocial, data = data, xlim=c(2500,8500),xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(grooming)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())
splots[[2]]<-ggsurvplot(fitSex, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "sex", legend.labs=c("Male", "Female"), ggtheme = theme_minimal())
splots[[3]]<-ggsurvplot(fitG, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend="bottom", legend.title = "Group",legend.labs=c("V","KK"), ggtheme = theme_minimal())
splots[[4]]<-ggsurvplot(fitR, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend = "bottom", legend.title = "Rank",legend.labs=c("Low", "Middle","High"), ggtheme = theme_minimal())
arrange_ggsurvplots(splots,print=TRUE, ncol=2, nrow=2, title="Survival effects of change in p(grooming)")
ggsave("dpSocial.SurvivalCurves.png")

##Contrasting dpAcc and dpSocial##
splots<-list()
splots[[1]]<-ggsurvplot(fitSocial, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(grooming)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())
splots[[2]]<-ggsurvplot(fitAcc, data = data, xlim=c(2500,8500), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(proximity)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())
arrange_ggsurvplots(splots,print=TRUE, ncol=1, nrow=2, title="Survival effects of change in sociality pre-to-post hurricane")
ggsave("SurvivalCurve_CompareProxGroom.png")
