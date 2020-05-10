library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(plyr)
flights = read.csv("flightsData.csv", header = TRUE)


head(flights)

substringRight = function(x,n) {
  substr(x, nchar(x)-n+1, nchar(x))
}


flightsNY <-flights[flights$ORIGIN_AIRPORT == c('JFK','EWR','LGA'), ]

#Removing some columns
flightsNY$DIVERTED <- NULL
flightsNY$CANCELLATION_REASON <- NULL
flightsNY$AIR_SYSTEM_DELAY <- NULL
flightsNY$SECURITY_DELAY <- NULL
flightsNY$AIRLINE_DELAY <- NULL
flightsNY$LATE_AIRCRAFT_DELAY <- NULL
flightsNY$WEATHER_DELAY <- NULL

head(flightsNY)
write.csv(flightsNY, file = "NEWYORK.csv")


## Add Location asNew York
flightsNY$location = 'New York'

hist(flightsNY$DEPARTURE_DELAY)
flightsNY$date = paste(flightsNY$DAY, flightsNY$MONTH, flightsNY$YEAR, sep='/')

table(is.na(flightsNY$SCHEDULED_DEPARTURE))

flightsNY$DeptTimeString = as.character(flightsNY$SCHEDULED_DEPARTURE)
table(is.na(flightsNY$DeptTimeString))


flightsNY$DeptTimeString[which(str_length(flightsNY$DeptTimeString) < 4)] = paste0('0', flightsNY$DeptTimeString[which(str_length(flightsNY$DeptTimeString) < 4)])
table(is.na(flightsNY$DeptTimeString))
flightsNY$DeptTimeString = paste0(substr(flightsNY$DeptTimeString, 1, 2), ':', substr(flightsNY$DeptTimeString, 3,4))
table(is.na(flightsNY$DeptTimeString))
colnames(flightsNY)[27] = 'time'


#organisng dates and rounding time in flightsNY data
library(lubridate)
flightsNY$TimeRounded = as.POSIXct(flightsNY$time, format = '%H:%M')
table(is.na(flightsNY$TimeRounded))
flightsNY$TimeRounded = round(flightsNY$TimeRounded, 'hours')
flightsNY$TimeRounded = strftime(flightsNY$TimeRounded, "%H:%M")

### earlisest flights and latest flight
max(flightsNY$SCHEDULED_DEPARTURE)
min(flightsNY$SCHEDULED_DEPARTURE)

flightsNY$dateDT = as.Date(flightsNY$date, format = '%d/%m/%Y')
table(is.na(flightsNY$dateDT))
flightsNY$date = as.character(flightsNY$dateDT)
flightsNY$DateTime = paste0(flightsNY$date,' ', flightsNY$TimeRounded)
table(is.na(flightsNY$DateTime))

##### We want to join the two dataframes by the column dateTime

flightsFilteredFull=  flightsNY %>%
  select(DateTime, DAY_OF_WEEK, AIRLINE, ORIGIN_AIRPORT, SCHEDULED_DEPARTURE, DEPARTURE_DELAY)

write.csv(file = 'flightsNYDone.csv', flightsFilteredFull)
