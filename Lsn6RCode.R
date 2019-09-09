library(tidyverse)
omega.dat<-read.table('http://www.isi-stats.com/isi2/data/FishOmega3.txt',fill=TRUE)


omega.df<-data.frame(omega3=as.numeric(as.character(omega.dat$V1[-1])),omegaper=as.numeric(as.character(omega.dat$V2[-1])),
                     fish=omega.dat$V3[-1],fishcat=(paste(omega.dat$V4,omega.dat$V5,
                                                          omega.dat$V6,omega.dat$V7))[-1])

group.means<-omega.df%>%group_by(fishcat)%>%summarize(mean=mean(omegaper))

group.means
#Null model
n=nrow(omega.df)

null.lm<-lm(omegaper~1,data=omega.df)

#$\hat{\sigma^2}*(n-1)$ backs out SST

SST=summary(null.lm)$sigma^2*(n-1)

#Also can find by:

sum((omega.df$omegaper-mean(omega.df$omegaper))^2)

#SSE can be found by:

Mult.means.lm<-lm(omegaper~fishcat,data=omega.df)

SSE=summary(Mult.means.lm)$sigma^2*22 #degrees of freedom are 22
SSE
#Or

sum((omega.df$omegaper-Mult.means.lm$fitted.values)^2)


#SSM can then be found by:

SSM<-SST-SSE

#SO the model R^2 is given by:

SSM/SST

#The group means 

group.means$mean

#For the top three are one SE away from the bottom two values

#We need to test if all means are the same
M<-5000
stats.df<-data.frame(trial=seq(1,M),stat=NA)

for(j in 1:M){
  omega.df$shuffled.cat<-sample(omega.df$fishcat)
  shuff.lm<-lm(omegaper~shuffled.cat,data=omega.df)
  stats.df[j,]$stat<-summary(shuff.lm)$r.squared
}

#So if labels don't matter we get R2 values such as:

stats.df %>% ggplot(aes(x=stat))+geom_histogram()+
  geom_vline(xintercept=SSM/SST,color="red",lwd=2)


stats.df %>% filter(stat>SSM/SST)%>%summarise(perc=n()/M)

#So, pretty rare that we would have observed an R2 like we actually did by chance

#Alternatively we can compute an F statistic

dfmod=4 #We have five means, so our model is estimating 4 alpha values
dfred=n-1-4 #Start with n observations, estimate mu, estimate 4 alpha values

Fstat=(SSM/dfmod)/(SSE/dfred)
Fstat

#When H0 is true, how rare would it be to get an Fstat of 5.6?
M<-5000
stats.df<-data.frame(trial=seq(1,M),stat=NA)

for(j in 1:M){
  omega.df$shuffled.cat<-sample(omega.df$fishcat)
  shuff.lm<-lm(omegaper~shuffled.cat,data=omega.df)
  SSE.shuf<-sum((omega.df$omegaper-shuff.lm$fitted.values)^2)
  SSM.shuf<-SST-SSE.shuf
  stats.df[j,]$stat<-(SSM.shuf/dfmod)/(SSE.shuf/dfred)
}


#How rare is our value:
stats.df %>% ggplot(aes(x=stat))+geom_histogram()+
  geom_vline(xintercept=Fstat,color="red",lwd=2)


stats.df %>% filter(stat>Fstat)%>%summarise(perc=n()/M)


#If certain validity conditions are met, then we don't need to simulate
#the distribution for the F statistic, we know it, it has an F distribution

#The F distribution has parameters, the first parameter is the model DF
#The second parameter is the residual DF

pf(Fstat,dfmod,dfred)

#Now of course we are looking at the wrong tail here

1-pf(Fstat,dfmod,dfred) #pretty darn close to our simulation


#Note we can also do:
Mult.means.lm<-lm(omegaper~fishcat,data=omega.df)
anova(Mult.means.lm)


#How to replicate a dataframe
rep.df<-bind_rows(replicate(3, rep.df, simplify = FALSE))

rep.lm<-lm(omegaper~fishcat,data=rep.df)
anova(rep.lm)
