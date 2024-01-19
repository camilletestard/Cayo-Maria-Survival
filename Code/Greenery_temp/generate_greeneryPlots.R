library(readr)
library(ggplot2)

setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/GreeneryIndex/')

#Load data for big Cayo
BigCayo_part1=read.csv('BigCayo_Sept2016-Dec2018.csv')
BigCayo_part2=read.csv('BigCayo_Dec2017-Dec2022.csv')
BigCayo=rbind(BigCayo_part1,BigCayo_part2[15:nrow(BigCayo_part2),])
BigCayo$C0.date = as.Date(parse_datetime(BigCayo$C0.date))

postHurrGreenery = BigCayo[which(BigCayo$C0.date>"2017-09-17"),]
ggplot(postHurrGreenery, aes(x=C0.date, y=C0.mean)) +
  geom_point() +
  geom_smooth(method=lm, se=T)

y=postHurrGreenery$C0.mean; x = 1:nrow(postHurrGreenery)
summary(lm(y ~ x))$coefficients

years = c(2018, 2019, 2020, 2021, 2022); y_year = vector()
for (year in 1:length(years)){
  date_idx = which(BigCayo$C0.date>"2017-01-01" & BigCayo$C0.date < "2017-12-31")
  y_year[year]=mean(postHurrGreenery$C0.mean[date_idx])
}
x=1:length(years)
summary(lm(y_year ~ x))$coefficients

#Subset data which has 0% cloud
#BigCayo=BigCayo[which(BigCayo$C0.cloudCoveragePercent==0),]

p_bigC<-ggplot(BigCayo, aes(x=C0.date, y=C0.mean))+
  geom_point()+
  geom_line()+
  #geom_smooth()+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-17")))+
  ylim(0, 0.55)+
  scale_x_date(date_breaks = "1 year")+
  theme_classic(base_size=15)
ggsave("BigCayo_Greenery_Updated_v2.pdf",p_bigC)

# #Find 'outlier' dates
# BigCayo_part1$C0.date[BigCayo_part1$C0.mean<0.25]
# BigCayo_part2$C0.date[BigCayo_part2$C0.mean<0.2]

#Load data for small Cayo
SmallCayo_part1=read.csv('SmallCayo_Sept2016-Dec2018.csv')
SmallCayo_part2=read.csv('SmallCayo_Dec2017-Dec2022.csv')
SmallCayo=rbind(SmallCayo_part1,SmallCayo_part2[14:nrow(SmallCayo_part2),])
SmallCayo$C0.date =  as.Date(parse_datetime(SmallCayo$C0.date))

#SmallCayo=SmallCayo[which(SmallCayo$C0.cloudCoveragePercent==0),]

p_smallC<-ggplot(SmallCayo, aes(x=C0.date, y=C0.mean))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-17")))+
  ylim(0, 0.55)+
  scale_x_date(date_breaks = "1 year")+
  theme_classic(base_size=15)
ggsave("SmallCayo_Greenery_Updated_v2.pdf",p_smallC)

#Find 'outlier' dates
SmallCayo_part1$C0.date[SmallCayo_part1$C0.mean<0.25]
SmallCayo_part2$C0.date[SmallCayo_part2$C0.mean<0.2]


#Directly compared smal cayo and big cayo
ggplot(SmallCayo, aes(x=C0.date, y=C0.mean))+
  geom_point()+
  geom_line()+
  geom_line(data=BigCayo, aes(x=C0.date, y=C0.mean),color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-17")))+
  ylim(0, 0.55)+
  scale_x_date(date_breaks = "1 year")+
  theme_classic(base_size=15)

#Plot data splitting 3 areas of the island
lbc_data=read.csv('LowerBigCayo_Landsat 8-9 L1-4-NDVI-2016-01-22-TO-2021-01-22.csv')
lbc_data$C0.date=as.Date(lbc_data$C0.date)
ubc_data = read.csv('UpperBigCayo_Landsat 8-9 L1-4-NDVI-2016-01-22-TO-2021-01-22.csv')
ubc_data$C0.date=as.Date(ubc_data$C0.date)
sc_data=read.csv('SmallCayo_Landsat 8-9 L1-4-NDVI-2016-01-22-TO-2021-01-22.csv')
sc_data$C0.date=as.Date(sc_data$C0.date)

ggplot(lbc_data, aes(x=C0.date, y=C0.mean))+
  geom_point()+
  geom_line()+
  geom_line(data=sc_data, aes(x=C0.date, y=C0.mean),color = "red")+
  geom_line(data=ubc_data, aes(x=C0.date, y=C0.mean),color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-17")))+
  ylim(0, 0.7)+
  scale_x_date(date_breaks = "1 year")+
  theme_classic(base_size=15)
