squirrels  <- read.table('http://www.isi-stats.com/isi2/data/Squirrels.txt', header=T)

glimpse(squirrels)

squirrels <- squirrels %>% mutate(Location = as.factor(Location))

contrasts(squirrels$Location) <- contr.sum

squirrels.lm <- lm(Length~Location,data = squirrels)

summary(squirrels.lm)
anova(squirrels.lm)
R2 <- summary(squirrels.lm)$r.squared

M<-5000
stats.df<-data.frame(trial=seq(1,M),stat=NA)

for(j in 1:M){
  squirrels$shuffled.cat<-sample(squirrels$Location)
  shuff.lm<-lm(Length~shuffled.cat,data=squirrels)
  stats.df[j,]$stat<-summary(shuff.lm)$r.squared
}

