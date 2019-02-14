#loading required packages.
require(randomForest)
require(ggplot2)
#importing the full data
four.Species <- read.csv("~/Capstone/four.csv")

##remove X1,spID 
fourSpecies <- four.Species[-c(1,3)]

fourSpecies$SpCode[fourSpecies$SpCode == 1] <- "Albo"
fourSpecies$SpCode[fourSpecies$SpCode == 2] <- "Dubia"
fourSpecies$SpCode[fourSpecies$SpCode == 3] <- "Rufa"
fourSpecies$SpCode[fourSpecies$SpCode == 4] <- "Grisea"

fourSpecies$SpCode<-as.factor(fourSpecies$SpCode)#change to factor

set.seed(12345)
four_all.rf<-randomForest(SpCode~.,fourSpecies, importance = TRUE) #with all variables
four_all.rf

par(mfcol = c(1,2))
varImpPlot(four_all.rf, type = 1, sort = T, main = "All Important 
Variables") #Accuracy
varImpPlot(four_all.rf, type = 2, sort = T, main = "All Important 
Variables") #gini index

par(mfcol = c(1,2))
varImpPlot(four_13.rf, type = 1, sort = T, n.var = 13, main = "Top 13- Important 
 Variables") #Accuracy
varImpPlot(four_13.rf, type = 2, sort = T, n.var = 13, main = "Top 13-Important 
Variables") #gini index

#error rate of rf
error_df = data.frame(error_rate = four_all.rf$err.rate[,'OOB'],num_trees = 1:four_all.rf$ntree)
ggplot(error_df, aes(x = num_trees, y = error_rate))+
    geom_line()





#top 13 variables 
four_13.rf<-randomForest(SpCode~Elev+WtoBl1+StiScWtoL1+StiLtoBlL+LWbase+ProFreeP+StiScW+SpSize+BasalPiL+NeBasiPiL+NoFreeP+StiL+WtoBl2,fourSpecies, importance = TRUE)
four_13.rf

varImpPlot(four_13.rf, type = 1) #Accuracy
varImpPlot(four_13.rf, type = 2) #gini index

#8 variables
four_8.rf<-randomForest(SpCode~Elev+WtoBl1+StiScWtoL1+StiLtoBlL+LWbase+ProFreeP+StiScW+SpSize,fourSpecies, importance = TRUE)
four_8.rf

varImpPlot(four_8.rf, type = 1) #Accuracy
varImpPlot(four_8.rf, type = 2) #gini index

#
four_7.rf<-randomForest(SpCode~Elev+WtoBl1+StiScWtoL1+StiLtoBlL+LWbase+ProFreeP+StiScW, data = fourSpecies, importance= TRUE)
four_7.rf
varImpPlot(four_7.rf, type = 1) #Accuracy
varImpPlot(four_7.rf, type = 2) #gini index

#4 variables
four_4.rf<-randomForest(SpCode~Elev+WtoBl1+StiScWtoL1+StiLtoBlL, data = fourSpecies, importance= TRUE)
four_4.rf

