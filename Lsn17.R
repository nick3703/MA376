## ----setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ----------------------------------------------------------------------------------
grape<-read.table("http://www.isi-stats.com/isi2/data/Polyphenols.txt",header=T)
grape <- grape %>% mutate(Ethanol=Ethanol.) %>% select(-Ethanol.)
grape <- grape %>% mutate(Time.hrs=Time.hrs.)%>% select(-Time.hrs.)
reg.lm<-lm(PC~Ethanol,data=grape)
our.stat<-summary(reg.lm)$'r.squared'


## ----------------------------------------------------------------------------------
M<-1000
empirical.dist<-data.frame(trial=seq(1,M),stat=NA)
for(i in 1:M){
  grape.shuff<-grape %>% mutate(Ethanol.shuff=sample(Ethanol))
  shuff.lm<-lm(PC~Ethanol.shuff,data=grape.shuff)
  empirical.dist[i,]$stat=summary(shuff.lm)$'r.squared'
}


## ----------------------------------------------------------------------------------
empirical.dist %>% ggplot(aes(x=stat))+
  geom_histogram()+geom_vline(xintercept=our.stat,color="red",lwd=2)
empirical.dist%>%filter(stat>our.stat)%>%summarize(pval=n()/M)


## ----------------------------------------------------------------------------------
resids=reg.lm$residuals
yhat=reg.lm$fitted.values
qqnorm(resids)
qqline(resids)
#hist(resids)
#plot(yhat,resids)


## ----------------------------------------------------------------------------------
anova(reg.lm)


## ----------------------------------------------------------------------------------
confint(reg.lm,level=0.95)

