# Reaction norm tutorial:

#Load libraries
library(lme4) 
library(MCMCglmm) 
library(tidyverse) 
library(broom) 
#library(nadiv) 
library(gridExtra) 
library(lattice)

#load data
setwd("~/Documents/GitHub/Cayo-Maria-Survival/ReactionNormTutorial/")
df_plast<-read_csv("aggression.csv")

df_plast<-df_plast%>% mutate(fitness=fitness/mean(fitness,na.rm=TRUE))

#Run random regression model:

#Random intercept model
lmer_a<-lmer(aggression~opp_size +scale(body_size)+ 
               scale(assay_rep,scale=FALSE)+block+ (1|ID), data=df_plast)
plot(lmer_a) 
qqnorm(residuals(lmer_a))

augment(lmer_a)%>% select(ID,block,opp_size,.fitted,aggression)%>% 
  filter(block==-0.5)%>% gather(type,aggression, `.fitted`:aggression)%>% 
  ggplot(.,aes(x=opp_size,y=aggression,group=ID))+ 
  geom_line(alpha=0.3)+ 
  theme_classic()+ facet_grid(.~type)


#Random slope model
lmer_a_rr<-lmer(aggression~opp_size+ scale(body_size)+ 
                  scale(assay_rep,scale=FALSE)+block+ (opp_size|ID), data=df_plast)

augment(lmer_a_rr)%>% 
  select(ID,block,opp_size,.fitted,aggression)%>% 
  filter(block==-0.5)%>% 
  gather(type,aggression, `.fitted`:aggression)%>% 
  ggplot(.,aes(x=opp_size,y=aggression,group=ID))+ geom_line(alpha=0.3)+
  theme_classic()+ facet_grid(.~type)

#Run bivariate model with MCMCGlmm

prior_biv_RR_px <-list(R= list(V =diag(c(1,0.0001),2,2),nu=0.002,fix=2), 
                       G=list(G1=list(V= matrix(c(1,0,0, 0,1,0, 0,0,1),3,3, byrow=TRUE), 
                                      nu=3, alpha.mu=rep(0,3), alpha.V=diag(25^2,3,3))))

mcmc_biv_RR<-MCMCglmm(cbind(scale(aggression), fitness)~trait-1+
                        at.level(trait,1):opp_size+ 
                        at.level(trait,1):scale(assay_rep,scale=FALSE)+ 
                        at.level(trait,1):block+ 
                        at.level(trait,1):scale(body_size), 
                      random=~ us(trait+opp_size:at.level(trait,1)):ID, 
                      rcov=~ idh(trait):units, family=c("gaussian","gaussian"), 
                      prior=prior_biv_RR_px, 
                      nitt=950000, 
                      burnin=50000, 
                      thin=450, 
                      verbose=TRUE, 
                      data= as.data.frame(df_plast), 
                      pr=TRUE, saveX=TRUE,saveZ=TRUE) 
plot(mcmc_biv_RR$VCV)




