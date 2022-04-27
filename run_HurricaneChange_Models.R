#run_HurricaneChange_Models.R
#Run models on change in sociality includiong 2019 and 2021
#IMPORTANT NOTE: for now we don't inlcude rank because we're missing rank for 2021 individuals

library(ggplot2)
library(forcats)
library(lme4)

#Load data
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data/')
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
  year <- as.factor(year)
})
SocialCapital.ALL$group.year = paste(SocialCapital.ALL$group, SocialCapital.ALL$year, sep="")

#If only consider IDs present both pre and post
id.isPost = table(SocialCapital.ALL$id, SocialCapital.ALL$isPost)
id_pre = row.names(as.data.frame(which(id.isPost[,"pre"]>0))); id_post = row.names(as.data.frame(which(id.isPost[,"post"]>0)))
id.PreAndPost = as.data.frame(row.names(as.data.frame(which(id.isPost[,"pre"]>0 & id.isPost[,"post"]>0)))); names(id.PreAndPost)="id"
id.PreAndPost$group = SocialCapital.ALL$group[match(id.PreAndPost$id, SocialCapital.ALL$id)]
table(id.PreAndPost$group)
SocialCapital.prepost = subset(SocialCapital.ALL, id %in% id.PreAndPost$id)

data = SocialCapital.prepost; data = subset(data, group %in% c("KK","F","V")) #Only consider within-individual comparisons
#data = SocialCapital.ALL #OR whole data
only.post<- droplevels(subset(data, isPost.year %in% c("post2018","post2019","post2021")))

# #If exclude 2018 data 
# data <- data[data$year!="2018",]

############################
### Plot amount of data ###
############################
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')

data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=year, y=hrs.focalfollowed, fill=isPost))+
  geom_violin(scale="width", width=1.5)+ geom_vline(xintercept = 3.5, linetype=2)+
  geom_jitter(width=0.1, alpha=0.7,size=1)+ facet_grid(~group)+theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90))

data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=year, y=numScans, fill=isPost))+
  geom_violin(scale="width", width=1.5)+ geom_vline(xintercept = 3.5, linetype=2)+
  geom_jitter(width=0.1, alpha=0.7,size=1)+ facet_grid(~group)+theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90))

######################################
### Test change in aggression rates ###
######################################

#Plot pre vs. post hurricane
data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=isPost, y=agg.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_classic(base_size=15)+
  ylab('Aggression rate')+ xlab('Hurricane Status')+ facet_grid(~ group)

#Show different years
data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=year, y=agg.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Aggression rate')+ xlab('Year')+ labs(fill="Hurricane Status")+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  facet_grid(~ group)
ggsave("AggressionRates_PrePost.png")

data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  mutate(isPost.year = fct_relevel(isPost.year, 
                                   "pre", "post2018","post2019","post2021")) %>%
  ggplot(aes(x=isPost.year, y=agg.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_classic(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('Aggression rate')+ xlab('Hurricane Status')+ facet_grid(~ group)
mean(data$agg.rate[data$isPost.year=="pre"]); 
mean(data$agg.rate[data$isPost.year=="post2019"])
mean(data$agg.rate[data$isPost.year=="post2021"])
table(data$isPost.year)
table(data$group, data$isPost)

agg.model<-glmer(agg.events~ isPost*group + sex +age + hrs.focalfollowed+ (1|id), data = data, family = poisson((link = "log")))
summary(agg.model)

agg.model.post<-  glmer(agg.events~ isPost.year + sex + group+ hrs.focalfollowed+ (1|id),data=only.post,family = poisson((link = "log")))
summary(agg.model.post)

############################################
### Test change in probability(proximity) ###
############################################
data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=isPost, y=prob.prox, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.05, aes(alpha = 0.7))+ #geom_boxplot(width=0.25)+
  theme_classic(base_size=15)+
  ylab('P(proximity)')+ xlab('Hurricane Status')+ facet_grid(~ group)
mean(data$prob.prox[data$isPost=="pre"]); 
mean(data$prob.prox[data$isPost=="post"])

data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=year, y=prob.prox, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  geom_vline(xintercept = 3.5, linetype=2)+
  theme_light(base_size=15)+theme(axis.text.x = element_text(angle = 45))+
  ylab('P(proximity)')+ xlab('Year')+ labs(fill="Hurricane Status")+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  facet_grid(~ group)
ggsave("ProximityRates_PrePost.png")

data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  mutate(isPost.year = fct_relevel(isPost.year, 
                                   "pre", "post2018","post2019","post2021")) %>%
  ggplot(aes(x=isPost.year, y=prob.prox, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_classic(base_size=15)+theme(axis.text.x = element_text(angle = 90))+
  ylab('P(proximity)')+ xlab('Hurricane Status')+ facet_grid(~ group)
mean(data$prob.prox[data$isPost.year=="pre"]); 
mean(data$prob.prox[data$isPost.year=="post2019"])
mean(data$prob.prox[data$isPost.year=="post2021"])

data$prox.events<-scale(data$prox.events)
prox.model<-glmer(prox.events~ isPost*group + sex + numScans+ (1|id), data = data, family = poisson((link = "log")))
summary(prox.model)

##########################################################################
### Test change in grooming duration and number of grooming partners ###
##########################################################################

#Test change in time spent grooming
data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=isPost, y=groom.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_classic(base_size=15)+
  ylab('Groom rate')+ xlab('Hurricane Status')+ facet_grid(~ group)
mean(data$groom.rate[data$isPost==0]); mean(data$groom.rate[data$isPost==1])
data$groom.dur<-scale(data$groom.dur)

groom.model<-glmer(groom.dur~ isPost*group + sex + hrs.focalfollowed+ (1|id), data = data, family = poisson((link = "log")))
summary(groom.model)

data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=year, y=groom.rate, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  theme(axis.text.x = element_text(angle = 45))+
  ylab('Groom rate')+ xlab('year')+ facet_grid(~ group)
ggsave("GroomingRate_PrePost.png")

#Test change in probability of grooming
data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=isPost, y=prob.groom, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_classic(base_size=15)+
  ylab('p(grooming)')+ xlab('Hurricane Status')+ facet_grid(~ group)

data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=year, y=prob.groom, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  theme(axis.text.x = element_text(angle = 45))+
  ylab('p(grooming)')+ xlab('year')+ facet_grid(~ group)

#Test change in number of grooming partners
data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=isPost, y=num.partners, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_classic(base_size=15)+
  ylab('#grooming partners')+ xlab('Hurricane Status')+ facet_grid(~ group)
mean(data$num.partners[data$isPost==0]); mean(data$num.partners[data$isPost==1])

data %>%
  mutate(isPost = fct_relevel(isPost, 
                              "pre", "post")) %>%
  ggplot(aes(x=year, y=num.partners, fill=isPost))+
  geom_violin(scale ="width")+ geom_jitter(width = 0.1, alpha = 0.7)+
  theme_light(base_size=15)+
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "red")+
  theme(axis.text.x = element_text(angle = 45))+
  ylab('# grooming partners')+ xlab('year')+ facet_grid(~ group)
ggsave("NumGroomPartners_PrePost.png")

numP.model<-glmer(num.partners~ isPost*group + sex + hrs.focalfollowed+ (1|id), data = data, family = poisson((link = "log")))
summary(numP.model)
