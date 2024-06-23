library("ggplot2")

#Load data
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/R.Data/")
load("BisonProximity_AM.RData")
AM.data = node_strength_all; AM.ids = node_ids
load("BisonProximity_PM.RData")
PM.data = node_strength_all; PM.ids = node_ids

#Set group year list
# group = c("V","KK","S","V","F","V","TT","V","F")
# years = c(2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
# groupyears = paste0(group,years)
group = c("F","F","HH","F","V","R","KK","R","V","F","HH","F","KK","V","V","KK","S","V","F","V","TT","V","F")
years = c(2013,2014,2014,2015,2015,2015,2015,
          2016,2016,2016,2016,2017,2017,2017,
          2018,2018, 2019, 2019,2021,2021,2022,2022,2022)
groupyears = paste0(group,years)

i=4; gy=1; AM.strength.all= data.frame(); PM.strength.all= data.frame(); corel.gy=vector(); pval.gy=vector()

for (gy in 1:length(groupyears)){
  
  data_path<-'~/Documents/GitHub/Cayo-Maria-Survival/Data/Data All Cleaned/BehavioralDataFiles/'
  data = read.csv(paste0(data_path,"Group",groupyears[gy],"_GroupByYear.txt")) #load meta data
  
  data$group = group[gy]
  data$year= years[gy]; 
  data$id = as.factor(data$id)
  data$sex[data$sex=="FALSE"]="F"; data$sex = as.factor(data$sex);

  AM.strength<-AM.data[[groupyears[gy]]][i,]; 
  AM.strength[1,]=as.numeric(AM.strength[1,]); #scale(as.numeric(AM.strength[1,])); 
  AM.strength[2,]=groupyears[gy]
  AM.strength[3,]=group[gy]
  AM.strength[4,]=years[gy]
  
  PM.strength<-PM.data[[groupyears[gy]]][i,]; 
  PM.strength[1,]=as.numeric(PM.strength[1,]) #scale(as.numeric(PM.strength[1,])); 
  PM.strength[2,]=groupyears[gy]
  PM.strength[3,]=group[gy]
  PM.strength[4,]=years[gy]
  
  col=intersect(colnames(AM.strength), colnames(PM.strength))
  
  idx_arranged = match(col, data$id)
  AM.strength.t=data.table::transpose(AM.strength[,col]); AM.strength.t$id = data$id[idx_arranged]; AM.strength.t$sex = data$sex[idx_arranged]
  PM.strength.t=data.table::transpose(PM.strength[,col]); PM.strength.t$id = data$id[idx_arranged]; PM.strength.t$sex = data$sex[idx_arranged]
  
  result<-cor.test(as.numeric(AM.strength.t$V1), as.numeric(PM.strength.t$V1))
  corel.gy[gy]=result$estimate
  pval.gy[gy]=result$p.value
  
  AM.strength.all = rbind(AM.strength.all, AM.strength.t)
  PM.strength.all = rbind(PM.strength.all, PM.strength.t)
  
}

data.frame(groupyears, corel.gy, pval.gy)

names(AM.strength.all) = c("strength","groupyear"); names(PM.strength.all) = c("strength","groupyear")
AM.strength.all$strength= as.numeric(AM.strength.all$strength); PM.strength.all$strength= as.numeric(PM.strength.all$strength); 
cor.test(AM.strength.all$strength, PM.strength.all$strength)

strength.all = cbind(AM.strength.all$strength, PM.strength.all)
names(strength.all)=c("AM","PM", "groupyear","group","year","id","sex")
strength.all$isPost = "pre"; strength.all$isPost[strength.all$year>2017]="post"

strength.all$groupyear=as.factor(strength.all$groupyear)

ggplot(strength.all, aes(x=AM, y=PM, color=groupyear))+
  geom_point(alpha=0.5)+
  geom_smooth(method='lm')+
  geom_abline(intercept=0, slope=1)+
  facet_wrap(~groupyear, scales="free")+ #if not standardized
  #facet_grid(~groupyear)+
  geom_point(mapping = aes(x = PM, y = AM), alpha = 0) +
  # xlim(0, 2.5)+
  # ylim(0, 2.5)+
  theme_light()

ggplot(strength.all, aes(x=PM, y=AM, color=isPost))+
  geom_point(alpha=0.3)+
  geom_smooth(method='lm')+
  #facet_grid(~isPost)+
  theme_light()+
  xlab("Afternoon proximity")+
  ylab("Morning proximity")
cor.test(strength.all$AM, strength.all$PM)

setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Correl_AM_PM_prox.pdf")
  

mdl<-lme4::lmer(PM~ AM*sex +(1|id)+(1|groupyear), data=strength.all)

ggsave("corr_AM_PM.pdf",p)