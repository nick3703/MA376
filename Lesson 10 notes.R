library(tidyverse)
strawberries <- read.table("http://www.isi-stats.com/isi2/data/StrawberryStorageRCBD.txt",
                           header=T)

str(strawberries)
strawberries <- strawberries %>% mutate(Storage = as.factor(Storage))%>%
  mutate(Variety = as.factor(Variety))

contrasts(strawberries$Storage)<- contr.sum
contrasts(strawberries$Variety)<- contr.sum

straw.lm <- lm(Firmness~Storage+Variety,data=strawberries)
summary(straw.lm)

library(emmeans)
emmeans(straw.lm,"Storage")
emmeans(straw.lm, "Variety")


anova(straw.lm)


#Where does 11.188 come from?

#Where does 63.511 come from?


#What's changed?
straw2.lm <- lm(Firmness~Storage,data=strawberries)
anova(straw2.lm)
