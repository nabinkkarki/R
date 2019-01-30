#HW#8: CART models
#Nabin Kumar Karki
#11/14/2017

#packages
library(dplyr); library(ggplot2);library(tidyr)
library(rpart); library(caret)                  #new packages
library(rpart.plot); library(e1071); library(MASS)


abalone_clean <- read.csv("~/STAT220/abalone_clean.csv")
View(abalone_clean)

#1
avalone<-abalone_clean[c(2:10)]#Eliminate X1 variable

#a
set.seed(1234567)
trainIndex<-createDataPartition(avalone$sex, p = .65,list = FALSE)#spilliting the data into 65% and 35%.
avaloneTrain<-avalone[trainIndex,]
avaloneTest<-avalone[-trainIndex,]

#2
#using rpart to explain the sex data set of avalone species.
avalone.rpart<-train(sex~.,avalone,method="rpart",maxdepth=8,na.action = na.rpart)
avalone.rpart$finalModel

#rpart.plot
rpart.plot(avalone.rpart$finalModel, digits=3, extra=2, under=TRUE)
prp(avalone.rpart$finalModel, digits=3, extra=101, box.palette = "BlGnYl")    #color the tree.
rpart.plot(avalone.rpart$finalModel, digits=3, extra=101, under=TRUE, box.palette = "BlGnYl")    
#by custom, “No” and “<” goes to the left, while “Yes” and “>” goes to the right.
#like most everything, you can play with the colors if you want.
show.prp.palettes()

#Variable viscera.weight is importannt because it explains the sex of avalone.

summary(avalone.rpart$finalModel)

avalonePredict.rpart <-predict(avalone.rpart, newdata=avaloneTrain, na.action=na.pass)#using training set.
summary(avalonePredict.rpart)

avalonePredict.rpart <-predict(avalone.rpart, newdata=avaloneTest, na.action=na.pass)
summary(avalonePredict.rpart)
table(avaloneTest$sex, avalonePredict.rpart, useNA="always")
confusionMatrix(avalonePredict.rpart, avaloneTest$sex)

#By analyzing the above we can conclude that the our model did a preety job of predicting the sex of avalone.



#3
avaloneSex<-abalone_clean[c(2:10)] %>% 
  distinct() %>%
  mutate('sex'= substring(sex,1,1)) %>% 
  filter(sex %in% c("M","F")) %>%
  droplevels()	

summary(avaloneSex$sex)

sex.rpart<-rpart(sex~., data= avaloneSex,na.action = na.rpart)
rpart.plot(sex.rpart)
prp(sex.rpart, cex=.7, extra=101,box.palette="OrPu", shadow.col=0) #prettier?
sex.predict <-predict(sex.rpart, avaloneSex, type="class", na.action=na.pass)
table(sex.predict, avaloneSex$sex,  useNA="always")
confusionMatrix(sex.predict, avaloneSex$sex)

#This model is different from the above one because in this model we filter the indeterminate sex avalone 
#which lead the similar result without considering the indeterminate sex.


