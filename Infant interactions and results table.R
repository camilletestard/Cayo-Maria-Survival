##Infant interaction survival models##
Infdata<-read.csv(file.choose()) #Infant survival data finished##
library("survival")
library("survminer")
Y=Surv(Infdata$Time,Infdata$Survival==1)

###interactions###
Fit1<-coxph(Y~Maternal_social+mother_rk+Maternal_social*mother_rk,data = Infdata)
summary(Fit1)
Fit2<-coxph(Y~Group_size+mother_rk+Group_size*mother_rk, data=Infdata)
summary(Fit2)
Fit3<-coxph(Y~Sex+ Group_size+Sex*Group_size,data=Infdata)
summary(Fit3)
Fit4<-coxph(Y~Maternal_social+Group_size+Maternal_social*Group_size,data=Infdata)
summary(Fit4)
Fit5<-coxph(Y~Maternal_social+Sex+Maternal_social*Sex,data=Infdata)
summary(Fit5)
Fit6<-coxph(Y~Sex+ mother_rk+ Sex*mother_rk, data= Infdata)
summary(Fit6)

##Infant interaction table##
InfdataT<-read.csv(file.choose()) ##Infant interaction table file##
library("data.table")
library("dplyr")
library("formattable")
library("tidyr")

InfdataT$'Regression coefficient'<-InfdataT$'Regression.coefficient'
InfdataT$'Interaction term'<-InfdataT$Interaction.term
InfdataT$'Hazard ratio (95% CI)'<-InfdataT$Hazard.ratio..95.cl.
InfdataT$'Wald Z'<-InfdataT$Wald.Z
InfdataT$'P-value'<-InfdataT$P.value
i2<-InfdataT %>%
  select(c('Interaction term','Regression coefficient', 'Hazard ratio (95% CI)', 'Wald Z', 'P-value'))

formattable(i2,
            align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))
