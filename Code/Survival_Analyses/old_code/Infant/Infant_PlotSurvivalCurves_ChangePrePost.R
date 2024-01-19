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
load('SocialCapital_changeP_infants.RData') #input dataframe generated previously.

i=1;  data= full.data[full.data$iter==i,];

#Format the data
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
data$mom.dpAcc.cat <- cut(data$mom.dpAcc,
                      breaks=as.numeric(quantile(data$mom.dpAcc,probs = seq(0, 1, 0.33))),
                      labels=c('Small', 'Medium', 'High'))
data$mom.dpSocial.cat <- cut(data$mom.dpSocial,
                         breaks=as.numeric(quantile(data$mom.dpSocial,probs = seq(0, 1, 0.33))),
                         labels=c('Small', 'Medium', 'High'))

# Plot raw survival rate difference
setwd('~/Documents/Github/Cayo-Maria-Survival/Results/')
ggplot(data, aes(x=as.factor(Survival), y=mom.dpAcc))+
  geom_violin()+xlab('')+ylab('Change in p(proximity) pre-to-post hurricane')+
  geom_boxplot(width=0.25)+ 
  theme_classic(base_size = 15)+ scale_x_discrete(labels= c("alive","dead"))
  #+facet_grid(~group)
ggsave("dpProx_Survival_Violin_infants.png")
mean(data$dpAcc[data$Survival==0]); mean(data$dpAcc[data$Survival==1])
sum(data$Survival[data$sex=='F'])/length(which(data$sex=='F'));sum(data$Survival[data$sex=='M'])/length(which(data$sex=='M'))

ggplot(data, aes(x=as.factor(Survival), y=mom.dpSocial))+
  geom_violin()+xlab('')+ylab('Change in p(grooming)')+
  geom_boxplot(width=0.25)+ 
  theme_classic(base_size = 15)+ scale_x_discrete(labels= c("alive","dead"))
ggsave("dpSocial_Survival_Violin.png")
mean(data$dpSocial[data$Survival==0]); mean(data$dpSocial[data$Survival==1])


####  Plot survival curves ####

#####################################################
### PROXIMITY ###
#####################################################

surv.prox<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~mom.dpAcc.cat + sex+ mom.ordrank+ group,data=data)
summary(surv.prox)
# cz <- cox.zph(surv.prox)
# print(cz)

##dpAcc model##
dpAcc_df<-with(data,
               data.frame(mom.dpAcc.cat =c("Small", "Medium", "High"),
                          sex= c("FEMALE","FEMALE","FEMALE"), #identifies the levels within the variable sex
                          group = c("V","V","V"), #Holds the effects of Group size constant at lowest value for both sexes
                          mom.ordrank= c("L","L","L")#Holds the effects of rank constant at lowest value for both sexes
               )
)
dpAcc_df
fitAcc<-survfit(surv.prox, newdata = dpAcc_df)

##sex model##
sex_df<-with(data,
             data.frame(mom.dpAcc.cat =c("Small", "Small"),
                        sex= c("MALE","FEMALE"), #identifies the levels within the variable sex
                        group = c("V","V"), #Holds the effects of Group size constant at lowest value for both sexes
                        mom.ordrank= c("L","L")#Holds the effects of rank constant at lowest value for both sexes
             )
)
sex_df
fitSex<-survfit(surv.prox, newdata = sex_df)


###Group model###
group_df<-with(data,
               data.frame(mom.dpAcc.cat =c("Small", "Small"),
                          sex= c("FEMALE","FEMALE"), #identifies the levels within the variable sex
                          group = c("V","KK"), #Holds the effects of Group size constant at lowest value for both sexes
                          mom.ordrank= c("L","L")#Holds the effects of rank constant at lowest value for both sexes
               )
)
fitG<-survfit(surv.prox,newdata = group_df)

###Rank model###
Rank_df<-with(data,
              data.frame (mom.dpAcc.cat =c("Small", "Small","Small"),
                          sex= c("FEMALE","FEMALE","FEMALE"), #identifies the levels within the variable sex
                          group = c("V","V","V"), #Holds the effect of sex constant across groups
                          mom.ordrank= c("L","M","H") #identifies the levels within rank (high, median, low)
              )
)
fitR<-survfit(surv.prox,newdata = Rank_df)

##Plotting curves together##
splots<-list()
splots[[1]]<-ggsurvplot(fitAcc, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(proximity)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())
splots[[2]]<-ggsurvplot(fitSex, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "sex", legend.labs=c("Male", "Female"), ggtheme = theme_minimal())
splots[[3]]<-ggsurvplot(fitG, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend="bottom", legend.title = "Group",legend.labs=c("V","KK"), ggtheme = theme_minimal())
splots[[4]]<-ggsurvplot(fitR, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend = "bottom", legend.title = "Rank",legend.labs=c("Low", "Middle","High"), ggtheme = theme_minimal())
arrange_ggsurvplots(splots,print=TRUE, ncol=2, nrow=2, title="Survival effects of change in p(proximity)")
ggsave("dpAcc.SurvivalCurves_infants.png")

#####################################################
### GROOMING ###
#####################################################

surv.groom<-coxph(Surv(Age_entry.days, Age_event.days, Survival)~mom.dpSocial.cat + sex+ mom.ordrank+ group,data=data)
summary(surv.groom)

##dpSocialmodel##
dpSocial_df<-with(data,
               data.frame(mom.dpSocial.cat =c("Small", "Medium", "High"),
                          sex= c("FEMALE","FEMALE","FEMALE"), #identifies the levels within the variable sex
                          group = c("V","V","V"), #Holds the effects of Group size constant at lowest value for both sexes
                          mom.ordrank= c("L","L","L")#Holds the effects of rank constant at lowest value for both sexes
               )
)
dpSocial_df
fitSocial<-survfit(surv.groom, newdata = dpSocial_df)


##sex model##
sex_df<-with(data,
             data.frame(mom.dpSocial.cat =c("Small", "Small"),
                        sex= c("MALE","FEMALE"), #identifies the levels within the variable sex
                        group = c("V","V"), #Holds the effects of Group size constant at lowest value for both sexes
                        mom.ordrank= c("L","L")#Holds the effects of rank constant at lowest value for both sexes
             )
)
sex_df
fitSex<-survfit(surv.groom, newdata = sex_df)


###Group model###
group_df<-with(data,
               data.frame(mom.dpSocial.cat =c("Small", "Small"),
                          sex= c("FEMALE","FEMALE"), #identifies the levels within the variable sex
                          group = c("V","KK"), #Holds the effects of Group size constant at lowest value for both sexes
                          mom.ordrank= c("L","L")#Holds the effects of rank constant at lowest value for both sexes
               )
)
fitG<-survfit(surv.groom,newdata = group_df)

###Rank model###
Rank_df<-with(data,
              data.frame (mom.dpSocial.cat =c("Small", "Small","Small"),
                          sex= c("FEMALE","FEMALE","FEMALE"), #identifies the levels within the variable sex
                          group = c("V","V","V"), #Holds the effect of sex constant across groups
                          mom.ordrank= c("L","M","H") #identifies the levels within rank (high, median, low)
              )
)
fitR<-survfit(surv.groom,newdata = Rank_df)

##Plotting curves together##
splots<-list()
splots[[1]]<-ggsurvplot(fitSocial, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(grooming)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())
splots[[2]]<-ggsurvplot(fitSex, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "sex", legend.labs=c("Male", "Female"), ggtheme = theme_minimal())
splots[[3]]<-ggsurvplot(fitG, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend="bottom", legend.title = "Group",legend.labs=c("V","KK"), ggtheme = theme_minimal())
splots[[4]]<-ggsurvplot(fitR, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend = "bottom", legend.title = "Rank",legend.labs=c("Low", "Middle","High"), ggtheme = theme_minimal())
arrange_ggsurvplots(splots,print=TRUE, ncol=2, nrow=2, title="Survival effects of change in p(grooming)")
ggsave("dpSocial.SurvivalCurves_infants.png")

##Contrasting dpAcc and dpSocial##
splots<-list()
splots[[1]]<-ggsurvplot(fitSocial, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(grooming)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())
splots[[2]]<-ggsurvplot(fitAcc, data = data, xlim=c(0,1600), xlab="Age in days", conf.int = T, legend ="bottom", legend.title = "Change in p(proximity)",legend.labs=c("Small", "Medium", "High"), ggtheme = theme_minimal())
arrange_ggsurvplots(splots,print=TRUE, ncol=1, nrow=2, title="Survival effects of change in sociality pre-to-post hurricane")
ggsave("SurvivalCurve_CompareProxGroom_infants.png")
