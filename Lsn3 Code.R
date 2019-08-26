#Libraries Needed
library(tidyverse)

#Functions Written

#Data

scent.dat<-read.table("http://www.isi-stats.com/isi2/data/OdorRatings.txt",header=TRUE)

#Code
str(scent.dat)

scent.dat %>% summarize(mean=mean(rating),std=sd(rating))


#Seperate means seperate standard deviations

scent.dat %>% group_by(condition)%>% summarize(mean=mean(rating),std=sd(rating),num.obs=n())

#This is different from

lm.mod<-lm(rating~condition,data=scent.dat)
summary(lm.mod)

#Is this the same?

lm.mod.2<-lm(rating~0+condition,data=scent.dat)
summary(lm.mod.2)

#To see what is being fit:
model.matrix(lm.mod)

model.matrix(lm.mod.2)
