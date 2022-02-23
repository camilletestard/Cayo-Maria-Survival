##Adult interaction survival models##
data<-read.csv(file.choose()) ##Adult data finished##
library("survival")
library(survminer)
Scale=365.25
Y=Surv(data$Age_entry.days./Scale,data$Age_event.days./Scale,data$Survival==2)

#interactions
Fit1<-coxph(Y~Social_integration+Rank+Social_integration*Rank,data=data)
summary(Fit1)
Fit2<-coxph(Y~Group_size+Rank+Group_size*Rank, data=data)
summary(Fit2)
Fit3<-coxph(Y~Sex+ Group_size+Sex*Group_size,data=data)
summary(Fit3)
Fit4<-coxph(Y~Social_integration+Group_size+Social_integration*Group_size,data=data)
summary(Fit4)
Fit5<-coxph(Y~Social_integration+Sex+Social_integration*Sex,data=data)
summary(Fit5)
Fit6<-coxph(Y~Sex+Rank+Sex*Rank,data=data)
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

