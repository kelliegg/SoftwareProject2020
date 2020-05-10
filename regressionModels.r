library(mltools)
library(Amelia)
library(dplyr)
library(GGally)
library(caret)
regression <- read.csv(file = "ModelData.csv")
regression$X = NULL

regression$tempC = regression$temperature - 273.15
regression$tempC = round(regression$tempC, digits = 0)

model <- filter(regression, regression$DEPARTURE_DELAY > 15)

modelData = model %>%
  select(DEPARTURE_DELAY, Pressure, Wind.Speed, tempC, humidity)


# Select data to put into training
training_index <- createDataPartition(modelData$DEPARTURE_DELAY, p=0.75, list=FALSE)

# Create training & testing dataset
training <- modelData[training_index,] 
testing <- modelData[-training_index,]

fitControl = trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 10)


##Linear model AND Random Forrest model
modelRF = train(DEPARTURE_DELAY ~ . , data = training, method = 'rf', trControl= fitControl)
modelRF

modelLM = train(DEPARTURE_DELAY ~ . , data = training, method = 'lm', trControl= fitControl)
modelLM

##Linear model
modelLM$finalModel
LMPredict = predict(modelLM, testing)
plot(LMPredict, testing$DEPARTURE_DELAY, col = "green")
abline(1,1)

##Random Forrest model
modelRF$finalModel
RFPredict = predict(modelRF, testing)
plot(RFPredict, testing$DEPARTURE_DELAY, col = "yellow")
abline(1,1)



