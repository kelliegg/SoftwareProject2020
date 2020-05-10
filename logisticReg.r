library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(plyr)
library(caret) 
library(e1071) 
library(mice) 
library(ggplot2)
library(caretEnsemble)
library(psych)
library(Amelia)
library(GGally)
library(rpart)
library(randomForest)
library(rattle)
library(assertr)
data = read.csv("NEWYORK.csv", header = TRUE)

data$X = NULL

##### DESCRIPTIVE ANALYSIS#####
flight_sum <- data %>% 
  #group flight cancellation and flight delay into one level
  mutate(delay = ifelse(DEPARTURE_DELAY >= 15 | is.na(DEPARTURE_DELAY) == TRUE, 1, 0),
         AIRLINE = factor(AIRLINE),
         ORIGIN_AIRPORT = factor(ORIGIN_AIRPORT)) %>% 
  #select relevant variables and save to a new data table
  select(delay, YEAR, MONTH, DAY, DAY_OF_WEEK, AIRLINE, ORIGIN_AIRPORT, DISTANCE,)
head(flight_sum)

#Correlation between departure delay and arrival delay, excluding cancelled flights
cor(data[c("DEPARTURE_DELAY", "ARRIVAL_DELAY")],use = "pairwise.complete.obs")

pairs(data[c("DEPARTURE_DELAY", "ARRIVAL_DELAY")])

#Number of flight delays and cancellations in this dataset
table(flight_sum$delay)

#Proportion of flight delays and cancellations in this dataset
round(table(flight_sum$delay)/nrow(flight_sum),2)

#Plot flight delays at take-off (not including cancellations - NAs) group by airlines
ggplot(data, aes(x= AIRLINE, y = DEPARTURE_DELAY, col = AIRLINE)) +
  geom_jitter(alpha = 0.5, size = 0.3) +
  coord_flip() +
  theme(legend.position = "none") +
  xlab("Airline Names") +
  ylab("Departure Delay (in minutes)") 

#Number of flights by different airlines
data %>% mutate(delay_group = case_when(DEPARTURE_DELAY <15 ~ "on-time", DEPARTURE_DELAY >=15 ~ "delayed", is.na(DEPARTURE_DELAY) == TRUE ~ "cancelled")) %>%
  ggplot(aes(x = AIRLINE, fill = delay_group)) +
  geom_bar(stat = "count", position = "dodge") +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("on-time" = "deepskyblue4", "delayed" = "yellow", "cancelled" = "red")) +
  xlab("Airline Names") +
  ylab("Flight Count") 

## Analysis with weather and flight data combined
data1 = read.csv("ModelData.csv", header = TRUE)

missmap(data1)

data1$X = NULL
data1$SCHEDULED_DEPARTURE = NULL

analysis <- data1 %>% 
mutate(delay = ifelse(DEPARTURE_DELAY >= 15 | is.na(DEPARTURE_DELAY) == TRUE, 1, 0))

analysis$tempC = analysis$temperature - 273.15
analysis$tempC = round(analysis$tempC, digits = 0)

analysis$temperature = NULL
analysis$DEPARTURE_DELAY = NULL

missmap(analysis)
write.csv(analysis, file = "rShinyData.csv")

analysis %>% mutate(delay = factor(delay)) %>%
  ggplot(aes(x = delay, y=tempC, col = delay)) +
  geom_boxplot()

analysis %>% mutate(delay = factor(delay)) %>%
  ggplot(aes(y = tempC, x=DAY_OF_WEEK, col = delay)) +
  geom_jitter() +
  scale_x_discrete(limits = 1:7)

analysis %>% mutate(delay = factor(delay)) %>%
  ggplot(aes(y = Wind.Speed, x=DAY_OF_WEEK, col = delay)) +
  geom_jitter() +
  scale_x_discrete(limits = 1:7)

analysis %>% mutate(delay = factor(delay)) %>%
  ggplot(aes(x = delay, y=humidity, col = delay)) +
  geom_boxplot()

analysis %>% mutate(delay = factor(delay)) %>%
  ggplot(aes(x = delay, y=Wind.Speed, col = delay)) +
  geom_boxplot()

analysis %>% mutate(delay = factor(delay)) %>%
  ggplot(aes(x = delay, y=Pressure, col = delay)) +
  geom_boxplot()

#############################################
#                                           #
#        EXAMPLE ONE FLIGHT DATA            #
#                                           #
#############################################
library(caret)

flight_sum$ORIGIN_AIRPORT <- as.factor(flight_sum$ORIGIN_AIRPORT)
flight_sum$DAY_OF_WEEK <- as.factor(flight_sum$DAY_OF_WEEK)
flight_sum$DISTANCE <- as.integer(flight_sum$DISTANCE)
flight_sum$AIRLINE <- as.factor(flight_sum$AIRLINE)
flight_sum$delay <- as.factor(flight_sum$delay)

set.seed(13) 

# Select columns to be used in algorithm training
feature<- c("delay", "DAY_OF_WEEK", "AIRLINE","ORIGIN_AIRPORT")

# Created sorted version of the ontime data
flight_sum_sorted <- flight_sum[,feature] 

# Select data to put into training
training_index <- createDataPartition(flight_sum_sorted$delay, p=0.75, list=FALSE)

# Create training & testing dataset
training_data <- flight_sum_sorted[training_index,] 
testing_data <- flight_sum_sorted[training_index,] 

# METHOD 1: Logistic Regression
log_reg_mod <- train(delay ~ ., data = training_data, method = "glm", family = "binomial",
                     trControl=trainControl(method = "cv", number = 5, repeats = 5))

# Predict
log_reg_predict <- predict(log_reg_mod, testing_data)

# Confusion matrix 
confusion_matrix_reg <- confusionMatrix(log_reg_predict, testing_data[,"delay"])
confusion_matrix_reg


#############################################
#                                           #
#   EXAMPLE TWO FLIGHT AND WEATHER DATA     #
#                                           #
#############################################

analysis$ORIGIN_AIRPORT <- as.factor(analysis$ORIGIN_AIRPORT)
analysis$DAY_OF_WEEK <- as.factor(analysis$DAY_OF_WEEK)
analysis$AIRLINE <- as.factor(analysis$AIRLINE)
analysis$delay <- as.factor(analysis$delay)
analysis$DateTime<- as.factor(analysis$DateTime)
analysis$tempC<- as.integer(analysis$tempC)

set.seed(13) 

# Select columns to be used in algorithm training
feature1<- c("delay", "DAY_OF_WEEK", "AIRLINE","ORIGIN_AIRPORT", "Pressure", "Wind.Speed", "humidity", "tempC")

analysis_sorted <- analysis[,feature1] 

# Select data to put into training
training_index <- createDataPartition(analysis_sorted$delay, p=0.75, list=FALSE)

# Create training & testing dataset
training_data <- analysis_sorted[training_index,] 
testing_data <- analysis_sorted[-training_index,] 

# METHOD 1: Logistic Regression
log_reg_mod <- train(delay ~ ., data = training_data, method = "glm", family = "binomial",
                     trControl=trainControl(method = "cv", number = 5, repeats = 5))

# Predict
log_reg_predict <- predict(log_reg_mod, testing_data)

# Confusion matrix 
confusion_matrix_reg <- confusionMatrix(log_reg_predict, testing_data[,"delay"])
confusion_matrix_reg

plot(varImp(log_reg_mod))

library(randomForest) 
random_forest <- randomForest(training_data[-1], training_data$delay, method = "rf", proximity = TRUE, importance = TRUE)
random_forest
random_forest_validation <- predict(random_forest, testing_data)

# Confusion matrix 
confusion_matrix_rf <- confusionMatrix(random_forest_validation, testing_data[,"delay"])
confusion_matrix_rf

plot(varImp(random_forest))



