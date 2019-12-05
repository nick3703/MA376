sigacts<-read_csv("SIGACTs.csv")

sigacts.time<-sigacts %>% mutate(dtg=dmy(DOI))

dat.group<-sigacts.time %>% group_by(month=floor_date(dtg, "month"))%>%summarise(count=n()) %>% drop_na()

dat.group %>% ggplot(aes(x=month,y=count))+geom_point()                                     


dat.group <- dat.group %>% mutate(obs=seq(1,nrow(dat.group)))

first.glm<-glm(count~obs,data=dat.group,family=poisson(link="log"))

null.glm<-glm(count~1,data=dat.group,family=poisson(link="log"))

#Is our model any better than the null model?

anova(first.glm,test="Chisq")

first.glm$fitted.values  #Fitted Mean Values

#So Variance should be approx 126-141

#Normal model

linear.mod<-lm(count~obs,data=dat.group)

#
ab <- read.dta("http://www.stata-press.com/data/lf2/couart2.dta")

ab %>% ggplot(aes(x=art))+geom_histogram()

pois.mod <- glm(art~fem+mar+kid5+phd+ment, family=poisson, data=ab)
summary(pois.mod)

anova(pois.mod,test="Chisq")

#So something going on

deviance(pois.mod) #But really bad compared to saturated model

qchisq(.95,df.residual(pois.mod)) #So if we are rejecting that our model fits well compared to saturated model

quas.mod <- glm(art~fem+mar+kid5+phd+ment, family=quasipoisson, data=ab)

#This is no longer appropriate

anova(quas.mod,test="Chisq")

anova(quas.mod,test="F")

summary(quas.mod)
