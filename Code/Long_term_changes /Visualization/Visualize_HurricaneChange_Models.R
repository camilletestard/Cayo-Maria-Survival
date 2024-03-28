#Visualize_HurricaneChange_Models.R
#Visualize changes in sociality rates 2015-2021

library(ggplot2)
library(lme4)
library(dplyr)
library(forcats)
library(ggbeeswarm)
library(glmmTMB)


#Load data
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/')
load("SocialCapital.ALL.RData")

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
  group<-as.factor(group)
  year.scaled <- scale(year)
})
SocialCapital.ALL$group.year = paste(SocialCapital.ALL$group, SocialCapital.ALL$year, sep="")
SocialCapital.ALL$Location=ifelse(SocialCapital.ALL$group=="V","Small Cayo","Big Cayo")

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


data = SocialCapital.prepost; #Only consider individuals present before AND after the storm
#data = SocialCapital.ALL; #data = subset(data, group %in% c("KK","F","V")) #Only consider groups with data before and after the storm (not necessarily the same IDs)
#data = SocialCapital.ALL #OR whole data
only.post<- droplevels(subset(data, isPost.year %in% c("post2018","post2019","post2021","post2022")))

#Adjust levels
data = data %>%
  mutate(isPost = fct_relevel(isPost,
                              "pre", "post")) %>%
  mutate(isPost.year = fct_relevel(isPost.year,
                              "pre", "post2018","post2019","post2021","post2022"))

############################
### Plot amount of data ###
############################
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')

data %>%
  ggplot(aes(x=year.factor, y=hrs.focalfollowed, fill=isPost))+
  geom_violin(scale="width", width=1.5)+ geom_vline(xintercept = 5.5, linetype=2)+
  geom_jitter(width=0.1, alpha=0.7,size=1)+ facet_grid(~group)+theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90))

data %>%
  ggplot(aes(x=year.factor, y=numScans, fill=isPost))+
  geom_violin(scale="width", width=1.5)+ geom_vline(xintercept = 5.5, linetype=2)+
  geom_jitter(width=0.1, alpha=0.7,size=1)+ facet_grid(~group)+theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90))

#######################################
#######################################
### Visualize change in aggression  ###
#######################################
#######################################

#Plot pre vs. post hurricane
data %>%
  ggplot(aes(x=isPost, y=agg.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_classic(base_size=15)+
  ylab('Aggression rate')+ xlab('Hurricane Status')+ facet_grid(~ group)

#Aggression rates across different years
data %>%
  ggplot(aes(x=year.factor, y=agg.rate, fill=isPost))+
  geom_violin(scale ="width")+ #geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Aggression rate')+ xlab('Year')+ labs(fill="Hurricane Status")+
  ylim(0,40)+
  geom_vline(xintercept = 5.5, linetype = "dashed", colour = "red")
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')
ggsave("AggressionRates_LongTermChange_Pooled_PrePost.pdf")

#Aggression rates across different years and groups
data %>%
  ggplot(aes(x=year.factor, y=agg.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Aggression rate')+ xlab('Year')+ labs(fill="Hurricane Status")+
  ylim(0,30)+
  geom_vline(xintercept = 5.5, linetype = "dashed", colour = "red")+
  facet_grid(~ Location)
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')
ggsave("AggressionRates_LongTermChange.pdf")

#Check correlation between number of aggressive events and observation effort
data %>%
  ggplot(aes(x=hrs.focalfollowed, y=agg.events, color=isPost.year))+
  geom_point()+
  theme_light(base_size=15)
  

#Number of aggression partners across different years
data %>%
  ggplot(aes(x=year.factor, y=numPartnersAgg, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Num. aggression partners')+ xlab('Year')+ labs(fill="Hurricane Status")+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  facet_grid(~ group)
ggsave("NumPartnersAgg_LongTermChange.png")


#For each individual, get mean agg rate pre and mean agg rate post
meanAggRate_pre_post = aggregate(data$agg.rate, by = list(data$id, data$isPost), FUN = mean)
pre = meanAggRate_pre_post[meanAggRate_pre_post$Group.2=="pre",c(1,3)]
post = meanAggRate_pre_post[meanAggRate_pre_post$Group.2=="post", c(1,3)]
D<-data.frame(id=pre[,1], pre=pre[,2], post=post[,2])
D$post_pre = D$post-D$pre; length(which(D$post_pre<0))/nrow(D)
ggpaired(
  D,
  cond1="pre",
  cond2="post", fill="condition")


#####################################
#####################################
### Visualize change in proximity ###
#####################################
#####################################

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
  geom_violin(scale ="width")+
  #geom_beeswarm()+ #geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('P(proximity)')+ xlab('Year')+ labs(fill="Hurricane Status")+
  geom_vline(xintercept = 5.5, linetype = "dashed", colour = "red")+ylim(0, 0.85)
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')
ggsave("ProximityRates_LongTermChange_Pooled_PrePost.pdf")

#Plot change in p(proximity) across years separated by group
data %>%
  ggplot(aes(x=year.factor, y=prob.prox, fill=isPost))+
  geom_violin(scale ="width")+ #geom_jitter(width = 0.1, alpha = 0.7)+
  geom_vline(xintercept = 5.5, linetype=2)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('P(proximity)')+ xlab('Year')+ labs(fill="Hurricane Status")+
  geom_vline(xintercept = 5.5, linetype = "dashed", colour = "red")+
  facet_grid(~ Location)
ggsave("ProximityRates_LongTermChange_ByLocation.pdf")

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


#Plot change in number of unique proximity partners
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


plot(data$numPartnersProx, data$numScans)
plot(data$numPartnersProx, data$hrs.focalfollowed)
plot(data$numScans, data$hrs.focalfollowed)


meanProxRate_pre_post = aggregate(data$prob.prox, by = list(data$id, data$isPost), FUN = mean)
pre = meanProxRate_pre_post[meanProxRate_pre_post$Group.2=="pre",c(1,3)]
post = meanProxRate_pre_post[meanProxRate_pre_post$Group.2=="post", c(1,3)]
D<-data.frame(id=pre[,1], pre=pre[,2], post=post[,2])
D$post_pre = D$post-D$pre; length(which(D$post_pre>0))/nrow(D)
ggpaired(
  D,
  cond1="pre",
  cond2="post", fill="condition")
####################################
####################################
### Visualize change in grooming ###
####################################
####################################

#Test change in number of grooming events
data %>%
  ggplot(aes(x=year.factor, y=groom.events.scans, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Number of groom events')+ xlab('Hurricane Status')
#+ facet_grid(~ group)

#Check relationship between observation effort and number of grooming events
test=data[data$group.year=="KK2017",]
test=data[data$year==2018,]
data %>%
  ggplot(aes(x=numScans, y=groom.events.scans, color=year.factor))+
  geom_jitter()+
  theme_light(base_size=15)

#Visualize change in grooming rate
data %>%
  ggplot(aes(x=year.factor, y=groom.rate, fill=isPost))+
  geom_violin(scale ="width")+ #geom_jitter(width = 0.1, alpha = 0.7)+
  geom_vline(xintercept = 5.5, linetype=2)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  #facet_grid(~Location)+
  #ylim(0,0.4)+
  ylab('Groom rate')+ xlab('Year')
ggsave("GroomingRates_LongTermChange.pdf")

data$prob.groom[data$prob.groom==0]=0.0000001
prob.groom.model<-glmmTMB(prob.groom~ isPost + offset(log(numScans))+ (1|id), data = data, family = beta_family(link="logit"))
summary(prob.groom.model)

groom.event.model<-glmer(groom.events.scans~ isPost*group + offset(log(hrs.focalfollowed))+ (1|id), data = data, family = poisson(link="log"))
summary(prob.groom.model)



numP.model<-glmer(num.partners~ isPost*group + sex + hrs.focalfollowed+ (1|id), data = data, family = poisson((link = "log")))
summary(numP.model)


meanGroomRate_pre_post = aggregate(data$groom.rate, by = list(data$id, data$isPost), FUN = mean)
pre = meanGroomRate_pre_post[meanGroomRate_pre_post$Group.2=="pre",c(1,3)]
post = meanGroomRate_pre_post[meanGroomRate_pre_post$Group.2=="post", c(1,3)]
D<-data.frame(id=pre[,1], pre=pre[,2], post=post[,2])
D$post_pre = D$post-D$pre; length(which(D$post_pre>0))/nrow(D)
ggpaired(
  D,
  cond1="pre",
  cond2="post", fill="condition")
