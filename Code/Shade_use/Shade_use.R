
library(ggplot2)

#Load shade use data
setwd("~/Documents/GitHub/Cayo-Maria-Survival/Data/Shade_use/")
data=readxl::read_xlsx("Space use data.xlsx")

data$Shade.perc=as.numeric(data$Shade.perc); data$Shade.perc[is.na(data$Shade.perc)]=0
data$MonkeyTemp=as.numeric(data$MonkeyTemp)
data$AM.PM=as.factor(data$AM.PM)
data$shade.status[which(data$shade.status=="YU"|data$shade.status=="YA")]="Y"
data$shade.status[which(data$shade.status=="Na")]="N"
data$shade.status=as.factor(data$shade.status)

ggplot(data, aes(x=shade.status, y=Focal.Clique.Size, color=AM.PM))+
 # geom_jitter(width=0.1, alpha=0.3)+
  geom_boxplot(width=0.5, alpha=0.1)+
  facet_grid(~AM.PM)+
  theme_light()

pie(c(length(which(data$shade.status=="N")),length(which(data$shade.status=="Y"))))

ggplot(data, aes(x=Shade.perc, y=MonkeyTemp, color=AM.PM))+
  geom_jitter()+
  geom_smooth(method = "lm")
cor.test(data$Shade.perc, data$MonkeyTemp)


setwd("~/Documents/GitHub/Cayo-Maria-Survival/Results/")
ggsave("Shade_use.pdf")

####################
# ggplot(df, aes(y = y, x = x, fill = g, color = g, group = interaction(x, g))) +
#   geom_boxplot(alpha = 0.1, width=0.75) +
#   geom_point(position = position_dodge(width=0.75))
# 
# ggplot(data, aes(x=Focal.Clique.Size, color=shade.status))+
#   geom_density()
# 
# ggplot(data, aes(x=Shade.perc, y=Clique.Size, color = AM.PM))+
#   geom_jitter()+
#   geom_smooth()
# 
# ggplot(data, aes(x=MonkeyTemp, y=Focal.Clique.Size))+
#   geom_jitter()
# 
# cor.test(data$MonkeyTemp,data$Shade.perc)
# ggplot(data, aes(x=MonkeyTemp, y=Shade.perc))+
#   geom_jitter()
# 
# data.NoShade = data[which(data$Shade.perc==0),]
# 
# test=table(data$ID, data$shade.status)
# data2=data.frame(ID=rownames(test), shade = test[,1], no.shade = test[,3])
# data2$shade.diff = (data2$shade - data2$no.shade)/(data2$shade + data2$no.shade)
# 
# data3=merge(data2, aggregate(Clique.Size ~ ID, data, mean), by="ID")
# cor.test(data3$shade.diff, data3$Clique.Size)
# 
# ggplot(data, aes(x=AM.PM, y=Shade.perc))+
#   geom_violin()+
#   geom_boxplot(width=0.2)+
#   geom_jitter(width=0.1, alpha=0.2)+
#   theme_classic(base_size = 14)
# 
# ggplot(data, aes(x=AM.PM, y=Focal.Clique.Size))+
#   geom_violin()+
#   geom_boxplot(width=0.2)+
#   geom_jitter(width=0.1, alpha=0.2)
#   
# 
