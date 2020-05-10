library(caret) # for the confusionMatrix() fucntion
library(e1071) # for the Na&iuml;ve Bayes algorithm
library(mice) # for multiple imputation of missing data
library(dplyr)
##install.packages('tidyverse')
library(tidyverse)
##install.packages('ggplot2')
library(ggplot2)
##install.packages('caretEnsemble')
library(caretEnsemble)
##install.packages('psych')
library(psych)
##install.packages('Amelia')
library(Amelia)
##install.packages('GGally')
library(GGally)
##install.packages('rpart')
library(rpart)
##install.packages('randomForest')
library(randomForest)

data1 <- read.csv(file = "ModelData.csv")

missingvals <- data.frame(sapply(data1, function(y) sum(length(which(is.na(y))))))
colnames(missingvals) <- c("Count of Missing Values")
missingvals

data1$X = NULL
data1$SCHEDULED_DEPARTURE = NULL

data1$DAY_OF_WEEK = factor(data1$DAY_OF_WEEK, levels = c(1,2,3,4,5,6,7), labels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

sapply(data1, class)
data1$Pressure <- as.numeric(as.character(data1$Pressure))
data1$Wind.Speed <- as.numeric(as.character(data1$Wind.Speed))
data1$Wind.Direction <- as.numeric(as.character(data1$Wind.Direction))
data1$humidity <- as.numeric(as.character(data1$humidity))
data1$AIRLINE <- as.factor(as.character(data1$AIRLINE))

summary(data1$Pressure)
table(is.na(data1$Pressure))
data1$pressureCat =cut(data1$Pressure,
                         breaks = c(min(data1$Pressure)-1, 1013,max(data1$Pressure)+1),
                         labels = c('low pressure','high pressure'))

table(data1$pressureCat)
table(is.na(data1$pressureCat))
sum(is.na(data1$pressureCat))
data1[is.na(data1$pressureCat),]

data1$tempC = data1$temperature - 273.15
data1$tempC = round(data1$tempC, digits = 0)
summary(data1$tempC)
data1$tempCat = cut(data1$tempC,
                      breaks = c(min(data1$tempC)-1,-9,-5,0, max(data1$tempC)+1),
                      labels=c("Freezing", "Cold", "Mild" ,"Warm"))
table(is.na(data1$tempCat))

par(mfrow=c(1,2))
hist(data1$Wind.Speed, breaks = 30)
summary(data1$Wind.Speed)
data1$WindSpeedBinned <- cut(data1$Wind.Speed,
          breaks = c(min(data1$Wind.Speed)-1,3,8,max(data1$Wind.Speed)+1),
          labels=c("low", "middle", "high"))
table(is.na(data1$WindSpeedBinned))

summary(data1$humidity)
data1$humidityCat =cut(data1$humidity,
                       breaks = c(min(data1$humidity)-1, 69.0, mean(data1$humidity), 87.0, max(data1$humidity)+1),
                       labels = c('very low','low','high','very high'))


data1$Delayed = ifelse(data1$DEPARTURE_DELAY > 0, 'Delayed', 'On Time')

missmap(data1)
###DelayTimes that are NA are flights that were cancelled

data1 = data1 %>%
  filter(!is.na(data1$Delayed))

BayesModelData = data1 %>%
  select(Delayed, DAY_OF_WEEK, AIRLINE, ORIGIN_AIRPORT, pressureCat, tempCat, WindSpeedBinned, humidityCat)

set.seed(1234)
indexTrain = createDataPartition(y = BayesModelData$Delayed, list = FALSE)
training = BayesModelData[indexTrain,]
testing = BayesModelData[-indexTrain,]

### Check Dimersions of Splits are Same as Original Dataset

prop.table(table(BayesModelData$Delayed)) * 100

prop.table(table(training$Delayed)) * 100

prop.table(table(testing$Delayed)) * 100

x = training[,-1]
y = training$Delayed

model = train(x, y, 'nb', trControl = trainControl(method = 'cv', number = 10))

model
model$finalModel

testing$Delayed = as.factor(testing$Delayed)
Predict = predict(model, newdata = testing)

confusionMatrix(Predict, testing$Delayed)

X = varImp(model)
plot(X)

### Model on Cancelled flights 
data1$cancelled = ifelse(is.na(data1$DEPARTURE_DELAY), 'Cancelled', 'Not Cancelled')
data1$cancelled = as.factor(data1$cancelled)

BayesModelDataCan = data1 %>%
  select(cancelled, AIRLINE, DAY_OF_WEEK, ORIGIN_AIRPORT, pressureCat, tempCat, WindSpeedBinned, humidityCat)

indexTrain = createDataPartition(y = BayesModelDataCan$cancelled, list = FALSE)
training = BayesModelDataCan[indexTrain,]
testing = BayesModelDataCan[-indexTrain,]

### Check Dimersions of Splits are Same as Original Dataset

prop.table(table(BayesModelDataCan$cancelled)) * 100

prop.table(table(training$cancelled)) * 100

prop.table(table(testing$cancelled)) * 100

x = training[,-1]
y = training$cancelled

model = train(x, y, 'nb', trControl = trainControl(method = 'cv', number = 10))

model
testing$cancelled = as.factor(testing$cancelled)
Predict = predict(model, newdata = testing)

confusionMatrix(Predict, testing$cancelled)

X = varImp(model)
plot(X)
