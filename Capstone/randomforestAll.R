# Random Forest on MICE imputed data
library(randomForest)
library(rpart.plot)

oneSpecies <- read.csv("~/Capstone/mice_imputed_one.csv")
threeSpecies <- read.csv("~/Capstone/mice_imputed_three.csv")

threeSpecies<-threeSpecies[-c(1,3)]#remove X1,spID 1,2,3...

threeSpecies$SpCode<-as.factor(threeSpecies$SpCode)#change to factor

set.seed(123)
threeSpecies_all.rf<-randomForest(SpCode~.,threeSpecies) #with all variables
varImpPlot(threeSpecies_all.rf,n.var = 8)#view the top 8 importance plot
threeSpecies_all.rf
varImpPlot(threeSpecies_all.rf, type = 1)
varImpPlot(threeSpecies_all.rf, type = 2)

#top 8 variables are: StiLtoBlL+WtoBl1+Elev+StiScW+ LWbase +NeBasiPiL+StiL
threeSpecies_8.rf<-randomForest(SpCode~StiLtoBlL+WtoBl1+Elev+StiScW+ LWbase +NeBasiPiL+StiL,three)
threeSpecies_8.rf

#three variables
threeSpecies_3.rf<-randomForest(SpCode~StiLtoBlL+WtoBl1+Elev,three)
threeSpecies_3.rf

# CAP
one <- read.csv("~/Capstone/kNN_imputed_one.csv")
three <- read.csv("~/Capstone/kNN_imputed_three.csv")

library(randomForest)
library(rpart.plot)
three<-three[-c(1,3)]#remove X1,spID 1,2,3...

three$SpCode<-as.factor(three$SpCode)#change to factor
set.seed(123)
three_all.rf<-randomForest(SpCode~.,three, importance = TRUE) #with all variables
varImpPlot(three_all.rf,n.var = 8)#view the top 8 importance plot
three_all.rf

varImpPlot(three_all.rf, type = 1)
varImpPlot(three_all.rf, type = 2)

#top 8 variables are: StiLtoBlL+WtoBl1+Elev+StiScW+ LWbase +NeBasiPiL+StiL
three_8.rf<-randomForest(SpCode~StiLtoBlL+WtoBl1+Elev+StiScW+ LWbase +NeBasiPiL+StiL,three)
three_8.rf

#three variables
three_3.rf<-randomForest(SpCode~StiLtoBlL+WtoBl1+Elev,three)
three_3.rf

