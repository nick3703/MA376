## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ------------------------------------------------------------------------------------------------------------------
grape<-read.table("http://www.isi-stats.com/isi2/data/Polyphenols.txt",header=T)
grape <- grape %>% mutate(Ethanol=Ethanol.) %>% select(-Ethanol.)
grape <- grape %>% mutate(Time.hrs=Time.hrs.)%>% select(-Time.hrs.)


## ------------------------------------------------------------------------------------------------------------------
null.lm<-lm(PC~1,data=grape)
summary(null.lm)


## ------------------------------------------------------------------------------------------------------------------
sep.means<-lm(PC~0+as.factor(Ethanol),data=grape)
summary(sep.means)


## ------------------------------------------------------------------------------------------------------------------
grape<-grape %>% mutate(Ethanolf=as.factor(Ethanol))
contrasts(grape$Ethanolf)<-contr.sum
effects.mod<-lm(PC~Ethanolf,data=grape)
anova(effects.mod)


## ------------------------------------------------------------------------------------------------------------------
gr.means <- grape %>% group_by(Ethanolf)%>%summarize(means=mean(PC))%>%
  mutate(Ethanol=as.numeric(as.character(Ethanolf)))
grape %>% ggplot(aes(x=Ethanol,y=PC))+geom_point()+geom_point(aes(x=Ethanol,y=means),color="red",size=4,data=gr.means)


## ------------------------------------------------------------------------------------------------------------------
reg.lm<-lm(PC~Ethanol,data=grape)
summary(reg.lm)


## ------------------------------------------------------------------------------------------------------------------
anova(reg.lm)


## ------------------------------------------------------------------------------------------------------------------
grape %>% ggplot(aes(x=Ethanol,y=PC))+geom_point()+
  geom_point(aes(x=Ethanol,y=means),color="red",size=4,data=gr.means)+
  stat_smooth(method="lm", se=FALSE)


## ------------------------------------------------------------------------------------------------------------------
grape<-grape %>% mutate(Timef=as.factor(Time.hrs))
reg.lm<-lm(PC~Time.hrs,data=grape)
sep.means<-lm(PC~Timef,data=grape)


## ------------------------------------------------------------------------------------------------------------------

gr.means <- grape %>% group_by(Timef)%>%summarize(means=mean(PC))%>%
  mutate(Time=as.numeric(as.character(Timef)))
grape %>% ggplot(aes(x=Time.hrs,y=PC))+geom_point()+
  geom_point(aes(x=Time,y=means),color="red",size=4,data=gr.means)+
  stat_smooth(method="lm",se=FALSE)

