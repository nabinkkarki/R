#Final Exam (Take-Home Portion)
#Nabin Kumar Karki
#12/09/2017

#libraries
library(randomForest); library(rpart); library(rpart.plot);  library(caret)  #classification packages
library(tidyverse); #library(tidyr); library(ggplot2); library(dplyr) #data and graphing packages

#Download file once!  Then, comment the download and read lines to avoid doing this multiple times.
URL<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(URL, destfile="FStormData.csv.bz2")	#downloading the file 

#Read the file into memory - this is slow because the file is huge.
raw<- read.csv("FStormData.csv.bz2")#reading the download file into the memory	
View(raw) #viewing the file

#Section 1 Data Processing will make the data.frame slightly less gigantic.
processed <- raw %>%								#filtering of data is done
    select(STATE, BGN_DATE, LATITUDE, LONGITUDE, EVTYPE, FATALITIES:PROPDMGEXP, REFNUM) %>%
    mutate(Damage = ifelse(PROPDMGEXP=="K", PROPDMG*10^3, 
                           ifelse(PROPDMGEXP=="M", PROPDMG*10^6,
                                  ifelse(PROPDMGEXP=="B", PROPDMG*10^9, PROPDMG)))) %>%	#basically mutating damage fatalities in thousand,millions and billions.
    mutate(LATITUDE = replace(LATITUDE, LATITUDE==0, NA)) %>%	#repalcing latitude zero with NA.
    mutate(LONGITUDE = replace(LONGITUDE, LONGITUDE==0, NA)) %>% #Repalcing longitude zero with NA.
    filter(PROPDMG > 5000000 |INJURIES > 10 | FATALITIES > 0 ) %>% #filtering the damage
    group_by(EVTYPE) %>% filter(n() >4)	 %>%	#grouping by EVTYPE and filtering count >4.
    mutate(MO = ifelse(STATE == "MO", 1, 0))

#confirm 626 tornadoes in the dataset
table(processed$EVTYPE)

#Section 2 Date Stuff
processed$BGN_DATE <-gsub( " .*$", "", processed$BGN_DATE)			#removing 0:00:00.
processed$BGN_DATE <- as.Date(processed$BGN_DATE, format="%m/%d/%Y")	#formating date with y-m-d.
processed <- processed %>% filter(BGN_DATE > "1995-12-31")	%>%		#filtering out date before 1996.
    droplevels()	

#viewing the processed data set.
View(processed)

#Section 3 Partition Stuff
set.seed(122017)								#generate random number
weatherTrain <- sample_frac(processed, .6)					#splitting data into 60% 
weatherTest  <- anti_join(processed, weatherTrain, by ='REFNUM' )	#remaining 40% in weatherTest

#4
Pro2<-processed %>% 
    separate(BGN_DATE,c("year","month","day"),sep = "-") %>% #seperating BGN_DATE into year,month,day
    group_by(STATE,year,EVTYPE) %>% 
    summarise(FATALITIES= sum(FATALITIES),PROPDMG = sum(PROPDMG)) 
  

#a) table to show the number of tornado events in MO by Year.
    Pro2 %>% 
        filter( EVTYPE %in%c("TORNADO") ,STATE %in% c("MO")) %>% #filtering MO state and TORNADO event type from our data set.
        select(STATE,year,EVTYPE) %>% 
        summarise(TORNADO=n())
    
    
#b)  barchart by Year, of FATALITIES in Missouri.

    Pro3<-Pro2 %>% 
        filter( STATE %in% c("MO")) %>% #filtering MO state
        select(STATE,year,FATALITIES) 
    
    ggplot(data = Pro3) +                   #barchart of fatalities in MO
       geom_col(mapping = aes(x = year, y =FATALITIES))
     
    

#c) scatterplot of Property Damage and Fatalities, colored by Year.

    ggplot(data = Pro2) + 
        geom_jitter(mapping = aes(y= FATALITIES, x= PROPDMG, color = year))
    
   
#4)Using caret

Pro.rpart <- train(FATALITIES ~.,processed,method = "rpart",maxdepth=8, na.action=na.rpart)
Pro.rpart$finalModel	#showing different result

#rpart.plot
 prp(Pro.rpart$finalModel, digits=3, extra=101, box.palette = "PuBu")	#cooler
 rpart.plot(Pro.rpart$finalModel, digits=3, extra=101, under=TRUE, box.palette = "PuBu")
 
 summary(Pro.rpart$finalModel)#Now, checking how well the model works

 #we can see how it considers multiple variables and “surrogates” at each node. 
 #It then chooses the variable that improves the model the best, injuries.
 
 ProPredict.rpart <-predict(Pro.rpart, newdata=weatherTest,na.action=na.pass)
 summary(ProPredict.rpart)
 table(weatherTest$FATALITIES, ProPredict.rpart, useNA="always")

 #5)using RandomForest.
 set.seed(122017)				
 Pro.rf <- train(MO ~ LATITUDE + LONGITUDE + EVTYPE + FATALITIES + INJURIES + Damage
, data=weatherTrain, method="rf", na.action=na.omit)  
 
 Pro.rf$finalModel	
 varImpPlot(Pro.rf$finalModel, n.var=10)	#latitude is the most important followed by longitude.
 Pro.rfP <- predict(Pro.rf, weatherTest, na.action=na.roughfix)
 summary(Pro.rfP) #notice that these aren’t whole numbers.
 confusionMatrix(factorweatherTest$MO,Pro.rfP)#unable to fix the error
 table(weatherTest$MO,pro.rfP, useNA="always")
 
 #confusionMatrix calculates the various parameter including the the accuracy of the prediction model.
 