## ----setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE,fig.width=5,fig.height=3)
library(tidyverse)


## ------------------------------------------------------------------
house.dat<-read.table("http://www.isi-stats.com/isi2/data/housing.txt",header=T)
house.dat <- house.dat %>% mutate(price=price.1000)%>%select(-price.1000)%>%
  group_by(lake)
house.dat %>% ggplot(aes(x=sqft,y=price,color=lake))+geom_point()+
  stat_smooth(method="lm",se=FALSE,fullrange=T)


## ------------------------------------------------------------------
contrasts(house.dat$lake)=contr.sum
inter.lm<-lm(price~sqft+lake+sqft:lake,data=house.dat)
summary(inter.lm)


## ------------------------------------------------------------------
contrasts(house.dat$lake)=contr.treatment
inter.lm<-lm(price~sqft+lake+sqft:lake,data=house.dat)
summary(inter.lm)


## ------------------------------------------------------------------
library(car)
Anova(inter.lm,type=3)


## ------------------------------------------------------------------
lakehouses<-house.dat %>% filter(lake=="lakefront")
lake.lm<-lm(price~sqft,data=lakehouses)
summary(lake.lm)



## ------------------------------------------------------------------
nonlakehouses<-house.dat %>% filter(lake!="lakefront")
nonlake.lm<-lm(price~sqft,data=nonlakehouses)
summary(nonlake.lm)



## ------------------------------------------------------------------
confint(inter.lm)


## ------------------------------------------------------------------
pred.var=5127.43+2700^2*.000801+2*2700*-1.94454
pred.se=sqrt(pred.var)
pred.se


## ------------------------------------------------------------------
conf.int=predict(inter.lm,data.frame(sqft=2700,lake="lakefront"),se.fit=TRUE,interval="confidence")


## ------------------------------------------------------------------
pred.int=predict(inter.lm,data.frame(sqft=2700,lake="lakefront"),se.fit=TRUE,interval="prediction")
conf.int$fit
pred.int$fit  

