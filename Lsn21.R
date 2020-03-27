## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,fig.width=12, fig.height=6)
library(tidyverse)


## ---------------------------------------------------------------------------------------------------------------------------------------------
pistachio.raw<-read.table("http://www.isi-stats.com/isi2/data/pistachioStudy.txt",header=T)
pistachio.dat <- pistachio.raw %>% mutate(Peroxide=Peroxide..remaining.)%>%
  select(-Peroxide..remaining.)
pistachio.dat<-pistachio.dat %>% select(Temperature,AirVelocity,Peroxide)


## ----warning=FALSE, out.width='60%',message=FALSE,fig.align="center"--------------------------------------------------------------------------
library(GGally)
pistachio.dat %>% ggpairs()


## ----warning=FALSE----------------------------------------------------------------------------------------------------------------------------
library(rgl)
x=pistachio.dat$Temperature
y=pistachio.dat$AirVelocity
z=pistachio.dat$Peroxide
plot3d(x,y,z)


## ---------------------------------------------------------------------------------------------------------------------------------------------
pistachio.lm<-lm(Peroxide~AirVelocity+Temperature,data=pistachio.dat)
summary(pistachio.lm)


## ---------------------------------------------------------------------------------------------------------------------------------------------
uni1.lm<-lm(Peroxide~Temperature,data=pistachio.dat)
summary(uni1.lm)


## ---------------------------------------------------------------------------------------------------------------------------------------------
uni2.lm<-lm(Peroxide~AirVelocity,data=pistachio.dat)
summary(uni2.lm)


## ---------------------------------------------------------------------------------------------------------------------------------------------
pistachio.std<-pistachio.raw %>%mutate(Peroxide=Peroxide..remaining.)%>%
  select(std.temp,std.air,std.peroxide,Peroxide)
std.lm<-lm(Peroxide~std.temp+std.air,data=pistachio.std)
summary(std.lm)


## ----message=FALSE,warning=FALSE--------------------------------------------------------------------------------------------------------------
#install.packages("p3d", repos="http://R-Forge.R-project.org")
library(p3d)
Plot3d(std.lm)


## ----out.width='60%',fig.align="center"-------------------------------------------------------------------------------------------------------
std.lm %>%ggplot(aes(x=.fitted,y=.resid,color=as.factor(std.temp)))+geom_point()


## ---------------------------------------------------------------------------------------------------------------------------------------------
inter.lm<-lm(Peroxide~std.temp*std.air,data=pistachio.std)
summary(inter.lm)
Plot3d(inter.lm)


## ---------------------------------------------------------------------------------------------------------------------------------------------
anova(std.lm,inter.lm)

