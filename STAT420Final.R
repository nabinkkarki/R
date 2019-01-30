#Final Exam (Take-home)
#Nabin Kumar Karki

#require packages
require(tidyverse)
require(corrplot)
require(MASS)
require(caret)
require(rpart)
require(randomForest)
require(rpart.plot)
require(cluster)
require(coefplot)

#importing the data set.
frmgham <- read.csv("~/STAT420/data/frmgham.dat")

#numerical data
frmgham.NumAll <- frmgham[-c(3,8:9,11:13,21:22)]  #gets most of the numeric and integer variables


#1
#Regression Model

#correlation plot
m <- cor(frmgham.NumAll)
corrplot(m, method = "number")


#partication of data into training and testing data set.
set.seed(12345)
frmgham.Trainig <-sample_frac(frmgham.NumAll,.7 )
frmgham.Testing <- setdiff(frmgham.NumAll,frmgham.Trainig)

#running the full regression model on training data set
full.Model <- lm(DIABP ~ .,data = frmgham.Trainig)
summary(full.Model)

#variable selection using step AIC.
least.AIC <- stepAIC(full.Model)

#best model from step AIC.
best.model <- lm(DIABP ~ SEX + RANDID + AGE + SYSBP + CURSMOKE + DIABETES + PREVCHD + 
                     PREVMI + PREVHYP + TIME + DEATH + ANGINA + HOSPMI + MI_FCHD + 
                     TIMEAP + TIMECHD + TIMESTRK + TIMEHYP, data = frmgham.Trainig)

summary(best.model)


#prediction using best model
prediction <- predict(best.model, data = frmgham.Testing, se.fit = TRUE, interval = "prediction", level = .95)
head(prediction$fit)


#2
#PCA
#running PCA on numerical data
frmgham.PCA <- princomp(frmgham.NumAll, scale = TRUE)
PCA.frmgham <-princomp(frmgham.NumAll,cor=TRUE,scores=TRUE)

#summary
summary(frmgham.PCA)
summary(PCA.frmgham)

#makes a scree plot for various outputs
screeplot(frmgham.PCA)
screeplot(PCA.frmgham)


#makes a cool loading plot
biplot(frmgham.PCA)		
biplot(PCA.frmgham)

#how many components to choose.
#loooking at the screen plot of PCA.frmgham we can choose top 5.
#from screenplot of frmgham.PCA only one component.

#LDA
#predicting smoker and non smoker

#Let’s make a testing and a training set of our data:

frmgham.LDA <- lda( CURSMOKE ~., data = frmgham.Trainig, na.action = na.exclude)
frmgham.LDA$scaling
plot(frmgham.LDA$scaling)

#We might explore the two variables with the largest coefficients:
    pSmoke <- ggplot(data=frmgham.Trainig, aes(x=SEX, y=PERIOD, color=frmgham.Trainig$CURSMOKE)) +
    geom_jitter()
pSmoke

#We can also use predict to see how well we did (internal validation).
smoke.predict <- predict(frmgham.LDA, frmgham.Trainig, na.action=na.exclude)
plot(smoke.predict$class, smoke.predict$x)
table(frmgham.Trainig$CURSMOKE, smoke.predict$class, useNA = "always" )


#using testing data set
smoket.predict <- predict(frmgham.LDA, frmgham.Testing)
table(frmgham.Testing$CURSMOKE, smoket.predict$class, useNA = "always" )

# Random Forest
#CART method used to produce a single tree
Diabetes.rpart <- train(HYPERTEN  ~ .,frmgham.NumAll, method="rpart", na.action=na.omit)
prp(Diabetes.rpart$finalModel, extra=101)

#random forest
set.seed(12345)			
Diabetes.rf <- randomForest(HYPERTEN   ~ ., data=frmgham.NumAll, na.action=na.omit)  
Diabetes.rf				#even this is very pleasing output, with a clear OOB error

#Which variables are most important
varImpPlot(Diabetes.rf)
varImpPlot(Diabetes.rf, n.var=10)	

#How well does it work internally?
Diabetes.rfp <- predict(Diabetes.rf, na.roughfix(frmgham.NumAll))
#what’s na.roughfix? It assigns the simple mean, median, or mode to na values
#that’s pretty rough, indeed, but does something to avoid the NA crisis.

#confusionMatrix(frmgham.NumAll$HYPERTEN,Diabetes.rfp)
#table(frmgham.NumAll$HYPERTEN,Diabetes.rfp, useNA="always")

#clustering 
frmghamScale <- frmgham.NumAll %>% 
    mutate_if(is.numeric, funs(scale))

#pam
pam.frmgham2 <- pam(frmghamScale,2)
summary(pam.frmgham2)
plot(pam.frmgham2)

pam.frmgham3 <- pam(frmghamScale,3)
summary(pam.frmgham3)
plot(pam.frmgham3)

agnes.frmgham <- agnes(frmgham.NumAll)
summary(agnes.frmgham)

#
#daisy
d.frmgham <- daisy(frmgham, metric = "euclidean", stand = FALSE)
summary(d.frmgham)
plot(d.frmgham)
as.matrix(d.frmgham)[,"DK"] # via as.matrix.dist(.)
## compare with 
as.matrix(daisy(frmgham, metric = "gower"))

#gender prediction using logestic regression
#all variables
frmgham1 <- frmgham.NumAll %>% 
    mutate(SEX = as.numeric(as.factor(SEX))) %>%
    select_if(is.numeric) %>%
    mutate(SEX=as.factor(recode(SEX, `1`="F", `2`="M", .default=NA_character_))) 

set.seed(12345)
frmgham.Trainig1 <-sample_frac(frmgham1,.7 )
frmgham.Testing1 <- setdiff(frmgham1,frmgham.Trainig1)

modelall <- glm(SEX ~., data = frmgham.Trainig1, family = "binomial")
summary(modelall)

coefplot(modelall)
pall <- predict(modelall, frmgham.Testing1, type = "response")
labelsall <-ifelse(pall > 0.5,'Male',"Female")
table(labelsall,frmgham.Testing1$SEX)
table(labelsall, frmgham.Testing1$SEX, useNA = "always")
