##Load in packages
library(dplyr)
library(MuMIn)
library(glmnet)
library(e1071)
library(ggplot2)
library(car)

##Data input
ABBB <- read.csv("Asian_black_bear_blood.csv")
ABBBS <- read.csv("Asian_black_bear_blood_standardized.csv")

##The correlation between methylation level and age
#SLC12A5-1
cor.test(ABBB$age,ABBB$SLC12A5_1_methylation_rate_ave)

SLC1 <- ggplot(ABBB,aes(x=age,y=SLC12A5_1_methylation_rate_ave))+theme_bw()+
geom_point(aes(shape=environment,color=sex),size=2,stroke=2)+
labs(x="Age (year)",y="DNA methylation (%)")+
scale_shape_manual(name="environment",labels=c("Captive"="captive","Wild"="wild"),values=c("Captive"=1, "Wild"=3))+
scale_color_manual(name="sex",labels=c("F"="female","M"="male"),values=c("F"="firebrick2","M"="dodgerblue4"))+
theme(axis.text.x=element_text(size=20),axis.text.y=element_text(size=20))+
theme(axis.title.x=element_text(size=17),axis.title.y=element_text(size=17))+
annotate("text",size=6,x=-Inf,y=Inf,hjust=-.1,vjust=2,label="R=0.94, p<0.001")+ 
labs (title="SLC12A5-1")+
theme(plot.title=element_text(size=20,hjust = 0.5))

#SLC12A5-2
cor.test(ABBB$age,ABBB$SLC12A5_2_methylation_rate_ave)

SLC2 <- ggplot(ABBB,aes(x=age,y=SLC12A5_2_methylation_rate_ave))+theme_bw()+
geom_point(aes(shape=environment,color=sex),size=2,stroke=2)+
labs(x="Age (year)",y="DNA methylation (%)")+
scale_shape_manual(name="environment",labels=c("Captive"="captive","Wild"="wild"),values=c("Captive"=1, "Wild"=3))+
scale_color_manual(name="sex",labels=c("F"="female","M"="male"),values=c("F"="firebrick2","M"="dodgerblue4"))+
theme(axis.text.x=element_text(size=20),axis.text.y=element_text(size=20))+
theme(axis.title.x=element_text(size=17),axis.title.y=element_text(size=17))+
annotate("text",size=6,x=-Inf,y=Inf,hjust=-.1,vjust=2,label="R=0.96, p<0.001")+ 
labs (title="SLC12A5-2")+
theme(plot.title=element_text(size=20,hjust = 0.5))


