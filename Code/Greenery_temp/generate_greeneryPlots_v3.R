library(readr)
library(ggplot2)

setwd('~/Documents/GitHub/Cayo-Maria-Survival/Data/GreeneryIndex/')

#Load NDVI data 
greenery =read.csv('Sentinel-2 NDVI-2015-05-12_2022-12-08.csv')
greenery$C0.date = as.Date(parse_datetime(greenery$C0.date))

greeneryShortTerm = greenery[1:max(which(greenery$C0.date < "2018-09-01")),]
greenery_pre = greeneryShortTerm$C0.mean[greeneryShortTerm$C0.date < "2017-09-20"]
greenery_post = greeneryShortTerm$C0.mean[greeneryShortTerm$C0.date > "2017-09-20"]
perc_change = (mean(greenery_pre)-mean(greenery_post))/mean(greenery_pre)
t.test(greenery_pre, greenery_post)

ggplot(greeneryShortTerm, aes(x=C0.date, y=C0.mean))+
  geom_point()+
  geom_line()+
  #geom_smooth()+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-17")))+
  #xlim(as.Date("2017-03-17"), as.Date("2018-03-17"))+
  #ylim(0, 0.55)+
  theme_classic(base_size=15)
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')
ggsave("GreeneryShortTerm_Updated.pdf")

ggplot(greenery, aes(x=C0.date, y=C0.mean))+
  geom_point()+
  geom_line()+
  #geom_smooth()+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-17")))+
  #xlim(as.Date("2017-03-17"), as.Date("2018-03-17"))+
  #ylim(0, 0.55)+
  theme_classic(base_size=15)
setwd('~/Documents/GitHub/Cayo-Maria-Survival/Results/')
ggsave("Greenery_Updated.pdf")


