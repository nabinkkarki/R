#loading required packages.
require(randomForest)
require(rpart.plot)

#importing the full data
Three.Species <- read.csv("~/Capstone/knn_final_three.csv")

#data cleaning
ThreeSpecies <- Three.Species[-c(1:3,7)] # removed unnecessary variables

set.seed(12345)
#with all the variables
three_all.rf<-randomForest(SpCode~.,ThreeSpecies, importance = TRUE) #with all variables
three_all.rf

varImpPlot(three_all.rf, type = 1) #Accuracy
varImpPlot(three_all.rf, type = 2) #gini index

#top 15 variables 
three_15.rf<-randomForest(SpCode~IndContin+AbHair.+AdHair+WtoBl1+X.WP2.WP1+StiScShape+StiScWtoL+Elev+StipeHairs+LWbase+NeBasiPiL+StiL+StiLtoBlL+ BasalPiL+AcroPiL,ThreeSpecies, importance = TRUE)
three_15.rf

varImpPlot(three_15.rf, type = 1) #Accuracy
varImpPlot(three_15.rf, type = 2) #gini index

#top 8 variables
three_8.rf<-randomForest(SpCode ~IndContin+WtoBl1+AdHair+AbHair.+Elev+StiScShape+X.WP2.WP1+StiScWtoL,ThreeSpecies, importance = TRUE)
three_8.rf

varImpPlot(three_8.rf, type = 1) #Accuracy
varImpPlot(three_8.rf, type = 2) #gini index


