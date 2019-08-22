

#knitr::opts_chunk$set(echo = TRUE,warning=FALSE,message=FALSE,results="hide",fig.show="hide")
#install.packages("tidyverse")


library(tidyverse)
berkley.data<-read.table("http://www.isi-stats.com/isi2/data/Berkeley.txt",header=T)
str(berkley.data)
berkley.data.new<-berkley.data %>%mutate(Program.factor = as.factor(Program))

ggplot(data=berkley.data,aes(Accepted))+
  geom_histogram(stat="count")


no.program<-berkley.data %>% select(-Program)


table(no.program)
#install.packages("ggmosaic")
library(ggmosaic)
ggplot(data=no.program)+
  geom_mosaic(aes(x=product(Accepted,Sex),fill=Accepted))

ggplot(data=berkley.data)+
  geom_mosaic(aes(x=product(Accepted,Sex),fill=Accepted))+
  facet_grid(Program~.)
