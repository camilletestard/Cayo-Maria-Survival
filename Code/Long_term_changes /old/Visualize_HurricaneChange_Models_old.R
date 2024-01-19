#run_HurricaneChange_Models.R
#Run models on change in sociality including 2019 and 2021
#IMPORTANT NOTE: for now we don't include rank because we're missing rank for 2021 individuals

library(ggplot2)
library(forcats)
library(lme4)
library(dplyr)
library(glmmTMB)
library(brms)

#Load data
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/')
load("SocialCapital.ALL.RData")
#SocialCapital.ALL=SocialCapital.ALL.cutoffmet
# SocialCapital.ALL=SocialCapital.ALL[SocialCapital.ALL$year<=2018,]
# SocialCapital.ALL$hrs.focalfollowed[SocialCapital.ALL$isPost=="post"]=SocialCapital.ALL$hrs.focalfollowed[SocialCapital.ALL$isPost=="post"]+1
# SocialCapital.ALL$agg.rate=SocialCapital.ALL$agg.events/SocialCapital.ALL$hrs.focalfollowed

#estimate observation time another way
# scan.time = 5
# SocialCapital.ALL$time.obs.scan = SocialCapital.ALL$numScans*scan.time/3600
# SocialCapital.ALL$hrs.focalfollowed[SocialCapital.ALL$year==2018] = SocialCapital.ALL$time.obs.scan[SocialCapital.ALL$year==2018]
# SocialCapital.ALL$agg.rate[SocialCapital.ALL$year==2018] = SocialCapital.ALL$agg.events[SocialCapital.ALL$year==2018]/SocialCapital.ALL$hrs.focalfollowed[SocialCapital.ALL$year==2018]

###################
### Format data ###
###################
SocialCapital.ALL<-within(SocialCapital.ALL,{
  sex<-as.factor(sex)
  ordinal.rank<-as.factor(ordinal.rank)
  id<-as.factor(id)
  isPost<-as.factor(isPost)
  isPost.year<-as.factor(isPost.year)
  year.factor<-as.factor(year)
  year.scaled <- scale(year)
})
SocialCapital.ALL$group.year = paste(SocialCapital.ALL$group, SocialCapital.ALL$year, sep="")

#If only consider IDs present both pre and post
id.isPost = table(SocialCapital.ALL$id, SocialCapital.ALL$isPost)
id.year = table(SocialCapital.ALL$id, SocialCapital.ALL$year); 
# Visualize sampling across years
# data_melt <- melt(id.year) 
# ggplot(data_melt, aes(Var2, Var1)) +                           # Create heatmap with ggplot2
#   geom_tile(aes(fill = value))+ 
#   scale_fill_gradient(low = "white", high = "red", limits=c(0, 1))


id_pre = row.names(as.data.frame(which(id.isPost[,"pre"]>0))); id_post = row.names(as.data.frame(which(id.isPost[,"post"]>0)))
id.PreAndPost = as.data.frame(row.names(as.data.frame(which(id.isPost[,"pre"]>0 & id.isPost[,"post"]>0)))); names(id.PreAndPost)="id"
id.PreAndPost$group = SocialCapital.ALL$group[match(id.PreAndPost$id, SocialCapital.ALL$id)]
table(id.PreAndPost$group)
SocialCapital.prepost = subset(SocialCapital.ALL, id %in% id.PreAndPost$id)

#plot data per individual for all years
# id.year.prepost = table(SocialCapital.prepost$id, SocialCapital.prepost$year);
# data_melt <- melt(id.year.prepost) 
# ggplot(data_melt, aes(Var2, Var1)) +                           # Create heatmap with ggplot2
#   geom_tile(aes(fill = value))+ 
#   scale_fill_gradient(low = "white", high = "red", limits=c(0, 1))


#data = SocialCapital.prepost; #Only consider individuals present before AND after the storm
data = SocialCapital.ALL; data = subset(data, group %in% c("KK","F","V")) #Only consider groups with data before and after the storm (not necessarily the same IDs)
#data = SocialCapital.ALL #OR whole data
only.post<- droplevels(subset(data, isPost.year %in% c("post2018","post2019","post2021")))

#Adjust levels
data = data %>%
  mutate(isPost = fct_relevel(isPost,
                              "pre", "post")) %>%
  mutate(isPost.year = fct_relevel(isPost.year,
                              "pre", "post2018","post2019","post2021"))

############################
### Plot amount of data ###
############################
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')

data %>%
  ggplot(aes(x=year.factor, y=hrs.focalfollowed, fill=isPost))+
  geom_violin(scale="width", width=1.5)+ geom_vline(xintercept = 3.5, linetype=2)+
  geom_jitter(width=0.1, alpha=0.7,size=1)+ facet_grid(~group)+theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90))

data %>%
  ggplot(aes(x=year.factor, y=numScans, fill=isPost))+
  geom_violin(scale="width", width=1.5)+ geom_vline(xintercept = 3.5, linetype=2)+
  geom_jitter(width=0.1, alpha=0.7,size=1)+ facet_grid(~group)+theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90))

##################################
### Test change in aggression  ###
##################################

#Plot pre vs. post hurricane
data %>%
  ggplot(aes(x=isPost, y=agg.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_classic(base_size=15)+
  ylab('Aggression rate')+ xlab('Hurricane Status')+ facet_grid(~ group)

# (mean(data$agg.rate[data$isPost=="post" & data$group=="V"])-
#     mean(data$agg.rate[data$isPost=="pre"& data$group=="V"]))/mean(data$agg.rate[data$isPost=="pre"& data$group=="V"])

#Aggression rates across different years
data %>%
  ggplot(aes(x=year.factor, y=agg.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Aggression rate')+ xlab('Year')+ labs(fill="Hurricane Status")+
  ylim(0,30)+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  facet_grid(~ group)
ggsave("AggressionRates_LongTermChange.pdf")

#test=data[data$isPost=="pre",]
data %>%
  ggplot(aes(x=hrs.focalfollowed, y=agg.events, color=isPost.year))+
  geom_point()+
  theme_light(base_size=15)
  
#model aggression rates 
#using regular glm
agg.model<-glmer(agg.events~ isPost+ sex +age + offset(log(hrs.focalfollowed))+ (1|id) +(1|group), data = data, family = poisson((link = "log")))
summary(agg.model)

#using brms
agg.model.brms <- brm(agg.events~ isPost+ sex +age + offset(log(hrs.focalfollowed))+ (1|id) +(1|group),
            data = data, family = poisson())
summary(agg.model.brms)
plot(agg.model.brms, variable = c("b_isPostpost"))
plot(conditional_effects(agg.model.brms, effects = "isPost"))
#Self-note: brms is much slower to run and yields very similar results

# !!!!!!!!!!!!!!!!!!!!!!!!!!
#Get the change across years..?
agg.model.post<-  glmer(agg.events~ year.scaled+group+ sex + age + offset(log(hrs.focalfollowed))+ (1|id),data=only.post,family = poisson(link = "log"))
summary(agg.model.post)

#Number of aggression partners across different years
data %>%
  ggplot(aes(x=year.factor, y=numPartnersAgg, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Num. aggression partners')+ xlab('Year')+ labs(fill="Hurricane Status")+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  facet_grid(~ group)
ggsave("NumPartnersAgg_PrePost.png")

#model aggressive partners
library(lmerTest)
plot(density(data$numPartnersAgg))
agg.model.partner<-glmer(numPartnersAgg~ isPost*group + offset(log(hrs.focalfollowed))+ (1|id), data = data,family = poisson(link = "log"))
summary(agg.model.partner)
#anova(agg.model.partner)
#Important note 

# data %>%
#   mutate(isPost = fct_relevel(isPost, 
#                               "pre", "post")) %>%
#   mutate(isPost.year = fct_relevel(isPost.year, 
#                                    "pre", "post2018","post2019","post2021")) %>%
#   ggplot(aes(x=isPost.year, y=agg.rate, fill=isPost))+
#   geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
#   theme_classic(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
#   ylab('Aggression rate')+ xlab('Hurricane Status')+ facet_grid(~ group)
# mean(data$agg.rate[data$isPost.year=="pre"]); 
# mean(data$agg.rate[data$isPost.year=="post2019"])
# mean(data$agg.rate[data$isPost.year=="post2021"])
# table(data$isPost.year)
# table(data$group, data$isPost)

################################
### Test change in proximity ###
################################
data %>%
  ggplot(aes(x=isPost, y=prob.prox, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.05, aes(alpha = 0.7))+ #geom_boxplot(width=0.25)+
  theme_classic(base_size=15)+
  ylab('P(proximity)')+ xlab('Hurricane Status')+ facet_grid(~ group)
mean(data$prob.prox[data$isPost=="pre"]); 
mean(data$prob.prox[data$isPost=="post"])

#Plot change in p(proximity) across years
data %>%
  ggplot(aes(x=year.factor, y=prob.prox, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  geom_vline(xintercept = 3.5, linetype=2)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('P(proximity)')+ xlab('Year')+ labs(fill="Hurricane Status")+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")#+
  #facet_grid(~ group)
ggsave("ProximityRates_LongTermChange_Pooled.pdf")

#Check the linear relationship between observation effort and 
#p(proximity)
test=data[data$group.year=="V2017",]
test %>%
  ggplot(aes(x=numScans, y=prox.events, color=year.factor))+
  geom_jitter()+
  theme_light(base_size=15)
# Number proximity events
test %>%
  ggplot(aes(x=numScans, y=prob.prox, color=year.factor))+
  geom_jitter()+
  theme_light(base_size=15)
#IMPORTANT NOTE: there is a clear linear relationship between number of proximity events and 
#observation effort in 2018 but not for other years. Probably because of the wide difference in observation effort

data$prob.prox[data$prob.prox==0]=0.0000001
prox.model<-glmmTMB(prob.prox~ isPost*group + offset(log(numScans))+ (1|id), data = data, family = beta_family(link="logit"))
summary(prox.model)


#Plot change in number of unique proximity parnters
data %>%
  ggplot(aes(x=year.factor, y=numPartnersProx, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  geom_vline(xintercept = 3.5, linetype=2)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Number proximity parters')+ xlab('Year')+ labs(fill="Hurricane Status")+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  facet_grid(~ group)
ggsave("ProximityPartners_PrePost.pdf")
plot(density(data$numPartnersProx))

#Check relationship between observation effort and number of proximity partners
test=data[data$group.year=="KK2017",]
test=data[data$year==2018,]
test %>%
  ggplot(aes(x=numScans, y=numPartnersProx, color=year.factor))+
  geom_jitter()+
  theme_light(base_size=15)

data.prox = data
data.prox$numScans<-scale(data.prox$numScans)
prox.partner.model<-glmer(numPartnersProx~ isPost*group + offset(log(hrs.focalfollowed))+ (1|id), data = data, family = poisson(link = "log"))
summary(prox.partner.model)

plot(data$numPartnersProx, data$numScans)
plot(data$numPartnersProx, data$hrs.focalfollowed)
plot(data$numScans, data$hrs.focalfollowed)
#Important note: offset(log(numScans)) causes an error... Initially I thought it could be
#because number of proximity partners does not scale linearly with number of scans but does with number of hours...
#This is not the case. For now I use number of hours
#Also unsure if poisson is appropriate here?? Is this count/number of observations?

# data %>%
#   mutate(isPost = fct_relevel(isPost, 
#                               "pre", "post")) %>%
#   mutate(isPost.year = fct_relevel(isPost.year, 
#                                    "pre", "post2018","post2019","post2021")) %>%
#   ggplot(aes(x=isPost.year, y=prob.prox, fill=isPost))+
#   geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
#   theme_classic(base_size=15)+theme(axis.text.x = element_text(angle = 90))+
#   ylab('P(proximity)')+ xlab('Hurricane Status')+ facet_grid(~ group)
# mean(data$prob.prox[data$isPost.year=="pre"]); 
# mean(data$prob.prox[data$isPost.year=="post2019"])
# mean(data$prob.prox[data$isPost.year=="post2021"])


###############################
### Test change in grooming ###
###############################

#Test change in number of grooming events
data %>%
  ggplot(aes(x=year.factor, y=groom.events.scans, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Number of groom events')+ xlab('Hurricane Status')+ facet_grid(~ group)

#Check relationship between observation effort and number of grooming events
test=data[data$group.year=="KK2017",]
test=data[data$year==2018,]
data %>%
  ggplot(aes(x=numScans, y=groom.events.scans, color=year.factor))+
  geom_jitter()+
  theme_light(base_size=15)

data %>%
  ggplot(aes(x=year.factor, y=prob.groom, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  geom_vline(xintercept = 3.5, linetype=2)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylim(0,0.4)+
  ylab('p(grooming)')+ xlab('Year')+ facet_grid(~ group)
ggsave("GroomingRates_LongTermChange.pdf")

data$prob.groom[data$prob.groom==0]=0.0000001
prob.groom.model<-glmmTMB(prob.groom~ isPost*group + offset(log(numScans))+ (1|id), data = data, family = beta_family(link="logit"))
summary(prob.groom.model)

groom.event.model<-glmer(groom.events.scans~ isPost*group + offset(log(hrs.focalfollowed))+ (1|id), data = data, family = poisson(link="log"))
summary(prob.groom.model)



numP.model<-glmer(num.partners~ isPost*group + sex + hrs.focalfollowed+ (1|id), data = data, family = poisson((link = "log")))
summary(numP.model)
