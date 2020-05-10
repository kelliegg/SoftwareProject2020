library(C50)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(ROCR)
library(rattle)

##merging the flight data and weather data together 
flights = read.csv(file = 'flightsNYDone.csv')
weather = read.csv(file = 'weather.csv')
flights$X = NULL
weather$X = NULL

weather$DateTime = paste(weather$date, weather$time)
weather$time = NULL
weather$date = NULL

data = merge(flights, weather, by = c('DateTime'))
write.csv(file ='ModelData.csv', data)
data = read.csv(file = 'ModelData.csv')

summary(data$humidity)
hist(data$humidity)






