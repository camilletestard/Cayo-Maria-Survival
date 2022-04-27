##Adult interaction survival models##
setwd('~/Documents/GitHub/Cayo-Maria-Survival/R.Data')
load('SocialCapital_Adults.RData')
data= SocialCapital.ALL[SocialCapital.ALL$group!="KK",]
length(which(data$Survival==1))/nrow(data)
Scale = 365.25

Y=Surv(data$Age_entry.days/Scale, data$Age_event.days/Scale, data$Survival)

#interactions
Fit1<-coxme(Y~std.num.partners+percentrank+std.num.partners*percentrank + (1|year.prehurr),data=data)
summary(Fit1)
Fit2<-coxme(Y~group.size+percentrank+group.size*percentrank + (1|year.prehurr), data=data)
summary(Fit2)
Fit3<-coxme(Y~sex+ group.size+sex*group.size + (1|year.prehurr),data=data)
summary(Fit3)
Fit4<-coxme(Y~std.num.partners+group.size+std.num.partners*group.size + (1|year.prehurr),data=data)
summary(Fit4)
Fit5<-coxme(Y~std.num.partners+sex+std.num.partners*sex + (1|year.prehurr),data=data)
summary(Fit5)
Fit6<-coxme(Y~sex+percentrank+sex*percentrank + (1|year.prehurr),data=data)
summary(Fit6)

interdata<-read.csv(file.choose()) ##Adult interaction table complete##
interdata$'Regression coefficient'<-interdata$Regression.coefficient
interdata$'Interaction term'<-interdata$Interaction.term
interdata$'Hazard ratio (95% cl)'<-interdata$Hazard.ratio..95..cl.
interdata$'Wald Z'<-interdata$Wald.Z
interdata$'P-value'<-interdata$P.value
library("data.table")
library("dplyr")
library("formattable")
library("tidyr")

i1<-interdata %>%
  select(c('Interaction term', 'Regression coefficient', 'Hazard ratio (95% cl)', 'Wald Z', 'P-value'))
            formattable(i1,
                        align =c("l","c","c","c","c", "c", "c", "c", "r"),
                        list(`Indicator Name` = formatter(
                          "span", style = ~ style(color = "grey",font.weight = "bold"))
                        ))

