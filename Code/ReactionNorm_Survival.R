#ReactionNorm_Survival.R

#Model proximity network with bison - get a distribution of possible networks from the observed data
#Regression from imputed dataset to test the change in proximity over time
#C. Testard October 2022

#Load libraries
#data wrangling
library(dplyr)
library(forcats)

#Modelling
library(lme4)
library(broom.mixed)

#For plotting
library(ggplot2)
library(sjPlot)

#For modeling
library(MCMCglmm) 
library(tidyverse) 
library(broom) 
#library(nadiv) 
library(gridExtra) 
library(lattice)

library(coxme)
library(survival)
library(brms)

#Load data
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load('Survival_Adults_TimeVarying_allgroups.RData')   
load("BisonProximity.RData")

#Set group year list
group = c("F","KK","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V","F")
years = c(2013, 2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
groupyears = paste0(group,years)


######################################
## STEP 1: CREATE IMPUTED DATASSET ###
######################################

# Extract change in proximity from each draw to create imputed data set:
imputed.data=list(); imputed.data.prepost=list(); i=1; gy=1;
imputed.density=list();
for (i in 1:num_iter){
  
  data.all<-data.frame(); density.iter=data.frame(matrix(NA, nrow=num_iter)); names(density.iter)="dens"
  density.all<-data.frame();
  
  for (gy in 1:length(groupyears)){
    
    
    #Extract behavioral data
    data_path<-'~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/'
    data = read.csv(paste0(data_path,"Group",groupyears[gy],"_GroupByYear.txt")) #load meta data
    data = data[data$hrs.focalfollowed>0,]
    
    data$group = group[gy]
    data$year= years[gy]; 
    data$isPost = ifelse(years[gy]<2018, "pre","post")
    data$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
    data$id = as.factor(data$id)
    data$sex[data$sex=="FALSE"]="F"; data$sex = as.factor(data$sex);
    data$id.year = paste(data$id, data$year,sep='.')
    
    strength<-node_strength_all[[groupyears[gy]]][i,]
    degree<-node_degree_all[[groupyears[gy]]][i,]
    node.id<-node_ids[[groupyears[gy]]]
    data$prox.strength<-as.numeric(strength[match(data$id, node.id)])
    data$prox.degree<-as.numeric(degree[match(data$id, node.id)])
    
    #data.merged<-merge(data,data.survival[,c("id.year","Age_entry.days","Age_event.days",
    #                                    "Survival","period")], by="id.year")
    
    data.final<-data[,c("id","sex","age","group","year","isPost","isPost.year",
                               "id.year","prox.strength", "prox.degree")]
    
    data.all<-rbind(data.all, data.final)
    
    density.iter$dens=density_all[[groupyears[gy]]]
    density.iter$group=group[gy]
    density.iter$year=years[gy]
    density.iter$isPost = ifelse(years[gy]<2018, "pre","post")
    density.iter$isPost.year = ifelse(years[gy]<2018,"pre",paste(data$isPost, data$year,sep='.'))
    
    density.all<-rbind(density.all, density.iter)
  }
  
  #Add age and death status info
  SurvivalData.ALL$YearOfBirth = year(SurvivalData.ALL$DOB)
  SurvivalData.ALL$age_at_event= SurvivalData.ALL$DOD-SurvivalData.ALL$DOB
  SurvivalData.ALL$age_at_event[is.na(SurvivalData.ALL$age_at_event)] = as.Date("2023-01-01")-SurvivalData.ALL$DOB[is.na(SurvivalData.ALL$age_at_event)]
  SurvivalData.ALL$age_at_entry = as.Date("2017-09-17") - SurvivalData.ALL$DOB
  
  dead.ids = unique(SurvivalData.ALL$id[which(!is.na(SurvivalData.ALL$YearOfDeath))])
  data.all$status=0
  data.all$status[!is.na(match(data.all$id, dead.ids))]=1; #data.all$status=as.factor(data.all$status)
  
  data.all$age_at_event = as.numeric(SurvivalData.ALL$age_at_event[match(data.all$id, SurvivalData.ALL$id)])
  data.all$age_at_entry = as.numeric(SurvivalData.ALL$age_at_entry[match(data.all$id, SurvivalData.ALL$id)])
  data.all$age_at_entry[data.all$age_at_entry<0]=0
  data.all$time= data.all$age_at_event - data.all$age_at_entry
    
  #Standardize
  density.all$std.dens <-(density.all$dens-mean(density.all$dens))/sd(density.all$dens)
  data.all$std.prox.strength = (data.all$prox.strength-mean(data.all$prox.strength))/sd(data.all$prox.strength)
  data.all$std.prox.degree = (data.all$prox.degree-mean(data.all$prox.degree))/sd(data.all$prox.degree)
  
  #Create year factor
  data.all$year.factor = as.factor(data.all$year)
  
  #Adjust levels
  data.all$group<-as.factor(data.all$group)
  data.all = data.all %>%
    mutate(isPost = fct_relevel(isPost,
                                "pre", "post")) %>%
    mutate(isPost.year = fct_relevel(isPost.year,
                                     "pre", "post.2018","post.2019","post.2021", "post.2022"))
  #"post.2018","pre","post.2019","post.2021", "post.2022"))
  
  density.all = density.all %>%
    mutate(isPost = fct_relevel(isPost,
                                "pre", "post")) %>%
    mutate(isPost.year = fct_relevel(isPost.year,
                                     "pre", "post.2018","post.2019","post.2021", "post.2022"))
  #"post.2018","pre","post.2019","post.2021", "post.2022"))
  
  
  imputed.data[[i]]=data.all
  imputed.density[[i]]=density.all
  
  
  #If only consider IDs present both pre and post
  id.isPost = table(data.all$id, data.all$isPost)
  id.year = table(data.all$id, data.all$year); 
  id_pre = row.names(as.data.frame(which(id.isPost[,"pre"]>0))); id_post = row.names(as.data.frame(which(id.isPost[,"post"]>0)))
  id.PreAndPost = as.data.frame(row.names(as.data.frame(which(id.isPost[,"pre"]>0 & id.isPost[,"post"]>0)))); names(id.PreAndPost)="id"
  id.PreAndPost$group = data.all$group[match(id.PreAndPost$id, data.all$id)]
  table(id.PreAndPost$group)
  data.prepost = subset(data.all, id %in% id.PreAndPost$id)
  
  imputed.data.prepost[[i]]<-data.prepost
  
  print(i)
  
}

# #Create imputed dataset to use in frequentist survival models
# imp<-miceadds::datalist2mids(imputed.data)
# imp.prepost<-miceadds::datalist2mids(imputed.data.prepost)

#Load data
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
#save(imputed.data.prepost, imputed.data, file="ReactionNormAnalysis_data.RData")
load("ReactionNormAnalysis_data.RData")

#Format data
data = imputed.data.prepost[[1]]; 
data$status=as.factor(data$status)
data$status.num=as.numeric(data$status)-1
data$sex.num = 0; data$sex.num[data$sex=="F"]=1
data$group.num=as.numeric(data$group)
data$isPost.num=as.numeric(data$isPost)


#Extract reaction norm by taking the random effect
model<-lmer(std.prox.strength~  1+ isPost +status+ scale(age)+ sex + (1|group) + (1+ isPost|id), data=data)
plot(model) 
qqnorm(residuals(model))
summary(model)

#Compute BLUPs for reaction norm
random_effects <- ranef(model)
fixed_effects <- fixef(model)
reaction_norms <- fixed_effects["isPostpost"]+ random_effects[["id"]]$'(Intercept)' + random_effects[["id"]]$isPostpost
df_rn<-data.frame(id=rownames(random_effects[["id"]]), 
                  rn = reaction_norms);
data$reaction_norm = df_rn$rn[match(data$id, df_rn$id)]


#Plot reaction norm by survival status
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")

augment(model)%>% select(id,group,sex,isPost,status,.fitted,std.prox.strength)%>% 
  gather(type,std.prox.strength, `.fitted`:std.prox.strength)%>% 
  ggplot(.,aes(x=isPost,y=std.prox.strength,group=id))+ 
  geom_line(alpha=0.3)+ 
  geom_point(alpha=0.3)+
  theme_light()+ facet_grid(~type)
ggsave("ReactionNorm_randomRegression.pdf")

df=augment(model)%>% select(id,group,sex,isPost,status,.fitted,std.prox.strength)%>%
  gather(type,std.prox.strength, `.fitted`:std.prox.strength)
df.fitted = df[df$type==".fitted",]
df_agg=aggregate(std.prox.strength ~ id + isPost +status, data=df.fitted, mean)

dfs <- df_agg %>%
  group_split(isPost)
df_pre = dfs[[1]]; df_post = dfs[[2]]; 
df_diff = df_pre[,c("id","status")]; df_diff$slope = df_post$std.prox.strength - df_pre$std.prox.strength

ggplot(df_diff, aes(x=slope, fill=status))+
  geom_histogram(alpha=0.5, color="black")+
  theme_light()
ggsave("ReactionNormDist_by_survival.pdf")

#Test the difference between the reaction norm distribution of individuals who survived
#and those who did not.
ks.test(df_diff$slope[df_diff$status==1], df_diff$slope[df_diff$status==0])

ggplot(df_diff, aes(x=status, y=slope, color=status))+
  geom_violin()+
  geom_boxplot(width=0.2)+
  theme_light()
ggsave("ReactionNormBoxPlot_by_survival.pdf")

###############################################
##### Model effect of slopes on survival #####
###############################################

#Have only one value per id for survival
data <- data %>%
  group_by(id) %>%
  mutate(status.num.nas = ifelse(row_number() == 1, status.num, NA))

#### Input reaction norm BLUP into survival model ####
data_surv <- data[,c("id","time","status","status.num","reaction_norm","sex","group")]
data_surv <- data_surv %>% distinct(id, .keep_all = TRUE)
data_surv$pre_level <- scale(df_pre$std.prox.strength[match(data_surv$id, df_pre$id)])
cor.test(data_surv$pre_level, data_surv$reaction_norm)

data_surv_low = data_surv[data_surv$pre_level<0,]; data_surv_low$pre.level = "low"
data_surv_high = data_surv[data_surv$pre_level>0,]; data_surv_high$pre.level = "high"
data.plot = rbind(data_surv_low, data_surv_high)

#plot change as a function of pre-level
ggplot(data.plot, aes(x=status, y=reaction_norm, color=pre.level))+
  geom_violin()+
  geom_jitter(width=0.1)+
  facet_grid(~pre.level)+
  theme_light()
ggsave("ReactionNorm_bySurvival_byPreLevel.pdf")

t.test(data_surv_low$reaction_norm[data_surv_low$status ==1], data_surv_low$reaction_norm[data_surv_low$status ==0])
t.test(data_surv_high$reaction_norm[data_surv_high$status ==1], data_surv_high$reaction_norm[data_surv_high$status ==0])

#cox proportional model
cox.model.high<-coxme(Surv(time, status.num)~ reaction_norm + sex + (1|group) + (1|id), data=data_surv_high)
cox.model.high

cox.model.low<-coxme(Surv(time, status.num)~ reaction_norm + sex + (1|group) + (1|id), data=data_surv_low)
cox.model.low

#This approach is problematic so we use a Bayesian bivariate mixed effect model (Houslay & Wilson 2017)
#### Run bivariate model with brms ####

#Split monkeys in two groups
data_low = data[data$id %in% unique(data_surv_low$id), ]
data_high = data[data$id %in% unique(data_surv_high$id), ]
#data_low$status.num.inverted = data_low$status.num - 1; data_low$status.num.inverted[data_low$status.num.inverted==-1]=1

#The proper way? Except I keep getting 0 variance for the random intercept for the survival model
bf_tolerance <- bf(std.prox.strength ~ isPost + scale(age) + sex + (1|group) + (1 + isPost | id), 
                  family = "gaussian")
bf_survival <- bf(time|cens(status.num.nas) ~ scale(age_at_event) + sex + (1|group) + (1 | id),
                  family = "weibull") 

#Low IDs
fit.lowIDs <- brm(bf_tolerance + bf_survival, 
                  data = data_low, chains = 3, iter = 4000, cores = 6)
plot(fit.lowIDs)
pp_check(fit.lowIDs)

#Extract random effects
random_effects = ranef(fit.lowIDs)
#Correlate random slope for social tolerance with random intercept for survival (time)
cor.test(random_effects$id[,,"stdproxstrength_isPostpost"], random_effects$id[,,"time_Intercept"])


#High IDs
fit.highIDs <- brm(bf_tolerance + bf_survival, 
                  data = data_high, chains = 3, iter = 4000, cores = 6)
plot(fit.highIDs)
pp_check(fit.highIDs)
#Extract random effects
random_effects = ranef(fit.highIDs)
#Correlate random slope for social tolerance with random intercept for survival (time)
cor.test(random_effects$id[,,"stdproxstrength_isPostpost"], random_effects$id[,,"time_Intercept"])


# 
# # #########################################################################
# #Run bivariate model with MCMCGlmm
# prior_biv_RR_px <-list(R= list(V =diag(c(1,0.0001),2,2),nu=0.002,fix=2),
#                        G=list(G1=list(V= matrix(c(1,0,0,0,
#                                                   0,1,0,0,
#                                                   0,0,1,0,
#                                                   0,0,0,1),4,4,
#                                                 byrow=TRUE),
#                                       nu=4,
#                                       alpha.mu=rep(0,4),
#                                       alpha.V=diag(25^2,4,4))))
# 
# mcmc_biv_RR<-MCMCglmm(cbind(std.prox.strength,
#                             status)~trait-1+
#                         at.level(trait,1):isPost+
#                         at.level(trait,1):scale(age,scale=FALSE)+
#                         at.level(trait,1):sex,
#                       random=~ us(trait+isPost:at.level(trait,1)):id,
#                       rcov=~ idh(trait):units, family=c("gaussian","gaussian"),
#                       prior=prior_biv_RR_px,
#                       nitt=500000,
#                       burnin=50000, thin=450,
#                       verbose=TRUE, data= as.data.frame(data_low),
#                       pr=TRUE, saveX=TRUE,saveZ=TRUE)
# 
# plot(mcmc_biv_RR$VCV)
# 
# summary(mcmc_biv_RR)$Gcovariances
# 
# mcmc_cor_RR_intfit<-mcmc_biv_RR$VCV[,"traitstatus:traitstd.prox.strength.id"]/
#   (sqrt(mcmc_biv_RR$VCV[,"traitstatus:traitstatus.id"])*
#      sqrt(mcmc_biv_RR$VCV[,"traitstd.prox.strength:traitstd.prox.strength.id"]))
# 
# posterior.mode(mcmc_cor_RR_intfit)
# HPDinterval(mcmc_cor_RR_intfit)
# 
# mcmc_cor_RR_slopefit<-mcmc_biv_RR$VCV[,"isPostpre:at.level(trait, 1):traitstatus.id"]/
#   (sqrt(mcmc_biv_RR$VCV[,"traitstatus:traitstatus.id"])*
#      sqrt(mcmc_biv_RR$VCV[,"isPostpre:at.level(trait, 1):isPostpre:at.level(trait, 1).id"]))
# 
# posterior.mode(mcmc_cor_RR_slopefit)
# HPDinterval(mcmc_cor_RR_slopefit)
# 
# # #############################################33
# # random_effects <- ranef(model)
# # fixed_effects <- fixef(model)
# # print(random_effects)
# # 
# # # Assuming 'fixed_effects' is a vector of fixed effects coefficients
# # # and 'random_effects' is a data frame with random effects
# # blups_slope <- fixed_effects[2] + random_effects$id[, "isPostpost"]
# # blups_intercept <- fixed_effects[1] + random_effects$id[, "(Intercept)"]
# # 
# # hist(random_effects[["id"]]$isPostpost)
# # plot(random_effects[["id"]]$'(Intercept)', random_effects[["id"]]$isPostpost)
# # cor.test(random_effects[["id"]]$'(Intercept)', random_effects[["id"]]$isPostpost)
# # 
# # #data$reaction_norm[match(data$id, row.names(random_effects[["id"]]))] = random_effects[["id"]]$isPostpost[match(data$id, row.names(random_effects[["id"]]))]
# # 
# # # Plot the reaction norm
# # plot(data$isPost, data$std.prox.strength, pch = 16, col = "black", ylab = "Response", xlab = "Time")
# # for (i in 1:nrow(random_effects$id)) {
# #   lines(data$isPost, predict(model, newdata = data.frame(id = i, isPost = data$isPost), re.form = NA), col = "black", lty = 2)
# # }
# # i=1
# # i=2


