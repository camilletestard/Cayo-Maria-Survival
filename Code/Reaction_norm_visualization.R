#Visualize_HurricaneChange_Models.R
#Visualize changes in sociality rates 2015-2021

library(ggplot2)
library(lme4)
library(dplyr)
library(forcats)
library(ggbeeswarm)
library(glmmTMB)
library(sjPlot)
library(ggpubr)


#Load data
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/')
load("SocialCapital.ALL.RData")
load('Survival_Adults_TimeVarying_allgroups.RData')

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

SurvivalData.ALL$age_at_end = SurvivalData.ALL$DOD-SurvivalData.ALL$DOB
SurvivalData.ALL$age_at_end[is.na(SurvivalData.ALL$age_at_end)] = as.Date("2023-01-01")-SurvivalData.ALL$DOB[is.na(SurvivalData.ALL$age_at_end)]

id_pre = row.names(as.data.frame(which(id.isPost[,"pre"]>0))); id_post = row.names(as.data.frame(which(id.isPost[,"post"]>0)))
id.PreAndPost = as.data.frame(row.names(as.data.frame(which(id.isPost[,"pre"]>0 & id.isPost[,"post"]>0)))); names(id.PreAndPost)="id"
id.PreAndPost$group = SocialCapital.ALL$group[match(id.PreAndPost$id, SocialCapital.ALL$id)]
table(id.PreAndPost$group)
SocialCapital.prepost = subset(SocialCapital.ALL, id %in% id.PreAndPost$id)
SocialCapital.prepost$prob.prox[which(is.na(SocialCapital.prepost$prob.prox))]=0
#SocialCapital.prepost$numPartnersProx= SocialCapital.prepost$numPartnersProx/SocialCapital.prepost$numScans

df=aggregate(numPartnersProx ~ id + isPost, data=SocialCapital.prepost, mean)
df_prob=aggregate(prob.prox ~ id + isPost, data=SocialCapital.prepost, mean)
df_obseffort=aggregate(numScans ~ id + isPost, data=SocialCapital.prepost, mean)
df_final = merge(merge(df, df_obseffort, by=c("id", "isPost")), df_prob, by=c("id", "isPost"))
df_final$sex = SocialCapital.prepost$sex[match(df_final$id, SocialCapital.prepost$id)]
df_final$sex=as.character(df_final$sex); df_final$sex[df_final$sex==FALSE]="F"; df_final$sex=as.factor(df_final$sex)
df_final$group = SocialCapital.prepost$group[match(df_final$id, SocialCapital.prepost$id)]

split_df <- split(df_final, df_final$isPost)
data = split_df[["pre"]]; data$isPost=NULL; names(data)[2]="pre_numPartners"; names(data)[3]="pre_numScans"; names(data)[4]="pre_prob.prox"
data$post_numPartners = split_df[["post"]]$numPartnersProx
data$post_numScans = split_df[["post"]]$numScans
data$post_prob.prox = split_df[["post"]]$prob.prox
data$diff_numP = data$post_numPartners - data$pre_numPartners
data$diff_prob.prox = data$post_prob.prox - data$pre_prob.prox
data$propdiff_prob.prox = (data$post_prob.prox - data$pre_prob.prox)/data$pre_prob.prox*100


dead.ids = unique(SurvivalData.ALL$id[which(SurvivalData.ALL$DOD>"2017-10-01")])
data$status=0
data$status[!is.na(match(data$id, dead.ids))]=1; data$status=as.factor(data$status)
data$age_at_end = as.numeric(SurvivalData.ALL$age_at_end[match(data$id, SurvivalData.ALL$id)])

thresh_pre = mean(data$pre_prob.prox);
thresh_post = mean(data$post_prob.prox);
# data$quadrant = 1; 
# data$quadrant[which(data$pre_numPartners<thresh_pre & data$post_numPartners>thresh_post)] = 2
# data$quadrant[which(data$pre_numPartners>thresh_pre & data$post_numPartners<thresh_post)] = 3
# data$quadrant[which(data$pre_numPartners>thresh_pre & data$post_numPartners>thresh_post)] = 4
# 
# tbl=table(data$quadrant[data$status=="dead"])
# contingency_table[1,1]=39; contingency_table[1,2]=7; 
# contingency_table[2,1]=7; contingency_table[2,2]=15; 
# result <- fisher.test(contingency_table)
# print(result)

setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')
ggplot(data, aes(x=pre_prob.prox, y=post_prob.prox, color=status))+
  geom_point(alpha=0.5)+
  #geom_vline(xintercept = thresh_pre)+
  #geom_hline(yintercept = thresh_post)+
  xlim(0,0.85)+
  ylim(0,0.85)+
  xlab('p(proximity) pre-hurricane')+
  ylab('p(proximity) post-hurricane')+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  theme_minimal()
ggsave("reaction_norm.pdf")

ggplot(data, aes(x=diff_prob.prox, fill=status))+
  #geom_histogram()+
  geom_density()+
  theme_minimal()
ggsave("reaction_norm_distribution.pdf")

ggplot(data, aes(y=diff_prob.prox, x=status, fill=status))+
  geom_violin()+
  geom_boxplot(width=0.2)+
  theme_minimal()
ggsave("reaction_norm_violin.pdf")

ggpaired(data, cond1 = "pre_prob.prox", cond2 = "post_prob.prox", color="status", line.color = "status")
ggsave("reaction_norm_paired_boxplot.pdf")

data.dead = data[data$status==1,]
data.alive = data[data$status==0,]

ggpaired(data.dead, cond1 = "pre_prob.prox", cond2 = "post_prob.prox", color="status")
ggsave("reaction_norm_paired_boxplot_dead.pdf")
ggpaired(data.alive, cond1 = "pre_prob.prox", cond2 = "post_prob.prox", color="status")
ggsave("reaction_norm_paired_boxplot_alive.pdf")

# Install and load required packages if not already installed
scaled_data=data
scaled_data[,c(2,3,4,7:12,14)] <- as.data.frame(scale(data[,c(2,3,4,7:12,14)]))

mylogit_numPartners <- glmer(status ~ age_at_end + diff_numP*pre_numPartners + pre_numScans + post_numScans + sex + (1|group), data = scaled_data, family = "binomial")
mylogit_probProx <- glmer(status ~ age_at_end + diff_prob.prox*pre_prob.prox + pre_numScans + post_numScans + sex + (1|group), data = scaled_data, family = "binomial")

cor.test(data$pre_prob.prox, data$diff_prob.prox)

# Print model summary
summary(mylogit_numPartners)
tab_model(mylogit_numPartners)
performance:: check_model(mylogit_numPartners)

summary(mylogit_probProx)
tab_model(mylogit_probProx)
performance:: check_model(mylogit_probProx)

# Plot the survival curve
plot(Predict(logistic_model), xlab = "Time", ylab = "Survival Probability")


