#Project-2
require(tidyverse)
require(MASS)

Tick.load <- read.csv("~/STAT331/data/Tick load.csv")
View(Tick.load)

summary(Tick.load)
TickNum <- Tick.load[-1]

#simple linear regression

set.seed(22217)			#this allows everyone to get the same sets
TickTraining <-  sample_frac(TickNum, .7)	#For fun, I used 70%
TickTesting<-  setdiff(TickNum, TickTraining)


Reg1 <- lm(AdultLag  ~ .,data = TickNum, na.action = na.exclude)
summary(Reg1)

Reg2 <- lm(AalSite ~ AvgDD10 + PrecipTot60 + Wind10  )

RegModel1 <- lm(AaLSite ~ AvgDD60, data = Tick.load)
summary(RegModel1)

reg3 <- glm.nb(AaLSite ~AvgDD60 + PrecipTot10 +  Wind30 + DayPeriod + AdultLag +SD,data = TickNum)
