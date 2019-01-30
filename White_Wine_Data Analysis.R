#project 250

library(tidyverse)
winequality_white <- read.csv("~/Downloads/winequality-white.csv", sep=";")


#1
set.seed(12345)			#this allows everyone to get the same sets
WineTraining <-  sample_frac(winequality_white, .7)	# used 70%
WineTesting<-  setdiff(winequality_white, WineTraining)

#2
#scatterplot of all the variables with the quality of alcohol.
WineTraining %>%
    gather(-quality, key= "var", value = "value", na.rm=TRUE)%>%
    ggplot(aes(y=value, x= quality))+
    geom_jitter()+
    stat_smooth(method="lm")+
    facet_wrap(~var, scales="free")

#3
require(corrplot)
corvec<- t(cor(y=WineTraining$quality, x=WineTraining, use="pairwise.complete.obs"))   #t transposes the vector, for easier viewing
#now, look at it
corvec

#correlation plot
cor.white <- cor(WineTraining)
corrplot(cor.white, method = "number")

# alcohol is a promising variable as quality of wine greatly depends on contain of alcohol.
wwq.p <- lm(WineTraining$quality ~WineTraining$pH)
wwq.a <- lm(WineTraining$quality ~WineTraining$alcohol)
summary(wwq.p)
summary(wwq.a)

wwq.ap <- lm(quality ~ pH +alcohol, data=WineTraining)
wwq.api <- lm(quality ~ pH*alcohol, data = WineTraining)

#4
wwq.all <- lm(quality ~., data=WineTraining)
summary(wwq.all)


#5

#6
anova(wwq.p,wwq.a, wwq.ap, wwq.all)

#7
WineTraining2 <- na.omit(WineTraining)

wwq2.all <- lm(quality ~., data=WineTraining2)
wwq.step <- step(wwq2.all, trace=0)
wwq2.step <- step(wwq2.all)

summary(wwq2.step)




#9
wwq2.p <- lm(quality ~ pH, data=WineTraining2)
wwq2.a <- lm(quality ~alcohol, data=WineTraining2)
wwq2.ap <- lm(quality ~pH + alcohol, data=WineTraining2)
wwq2.api <- lm(quality ~pH * alcohol, data=WineTraining2)
wwq2.all <- lm(quality ~., data=WineTraining2)
AIC (wwq2.p,wwq2.a,wwq2.ap,wwq2.api,wwq2.all,wwq2.step)

#10
WineTesting2<-WineTesting %>% 
    select(fixed.acidity,free.sulfur.dioxide,sulphates ,pH,density ,alcohol ,residual.sugar ,volatile.acidity) %>% 
    na.omit()

pred.a <- predict(wwq2.a, WineTesting2)
pred.ap <- predict(wwq2.ap, WineTesting2)
pred.step <- predict(wwq2.step, WineTesting2)

actuals_preds <- data.frame(cbind(actuals=WineTesting2$alcohol, 
                                  a=pred.a, ap=pred.ap, step=pred.step)) 

correlation_accuracy <- cor(actuals_preds, use="pairwise.complete.obs")
View(correlation_accuracy)

require(olsrr)
model.wine <- lm(quality ~.,data = winequality.white)
#all possible regression
ols_step_all_possible(model.wine)


#best subset Regression
#Select the subset of predictors that do the best at meeting some well-defined objective criterion, such as having the largest R2 value or the smallest MSE, Mallow’s Cp or AIC.
ols_step_best_subset(model.wine)

#step wise AIC model
require(MASS)
stepAIC(model.wine)

#prediction
best.model <-lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + free.sulfur.dioxide + density + pH + sulphates + alcohol,  data = WineTraining)

predictmodel <- predict(best.model,WineTesting)
summary(predictmodel)

