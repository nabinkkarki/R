#loading required packages.
require(randomForest)
require(rpart.plot)

#importing the full data
four.Species <- read.csv("~/Capstone/four.csv")

##remove X1,spID 
fourSpecies <- four.Species[-c(1,3)]
   
fourSpecies$SpCode<-as.factor(fourSpecies$SpCode)#change to factor

set.seed(12345)
four_all.rf<-randomForest(SpCode~.,fourSpecies, importance = TRUE) #with all variables
varImpPlot(four_all.rf,n.var = 8)#view the top 8 importance plot
four_all.rf

varImpPlot(four_all.rf, type = 1) #Accuracy
varImpPlot(four_all.rf, type = 2) #gini index

#top 8 variables are: StiLtoBlL+WtoBl1+Elev+StiScW+ LWbase +NeBasiPiL+StiL
four_8.rf<-randomForest(SpCode~StiLtoBlL+WtoBl1+Elev+StiScW+ LWbase +NeBasiPiL+StiL,fourSpecies)
four_8.rf

#three variables
four_3.rf<-randomForest(SpCode~StiLtoBlL+WtoBl1+Elev,fourSpecies)
four_3.rf

