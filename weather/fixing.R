library(dplyr)
library(stringr)

pressure = read.csv("pressure.csv", header = TRUE)
windSpeed = read.csv("wind_speed.csv", header = TRUE)
windDirection = read.csv("wind_direction.csv", header = TRUE)
temp = read.csv("temperature.csv", header = TRUE)
humidity = read.csv("humidity.csv", header = TRUE)
pressure$datetime = as.character(pressure$datetime)

pressure = filter(pressure, str_detect(datetime, '2015-01|2015-02|2015-03'))
windSpeed = filter(windSpeed, str_detect(datetime, '2015-01|2015-02|2015-03'))
windDirection = filter(windDirection, str_detect(datetime, '2015-01|2015-02|2015-03'))
temp = filter(temp, str_detect(datetime, '2015-01|2015-02|2015-03'))
humidity = filter(humidity, str_detect(datetime, '2015-01|2015-02|2015-03'))



pressure = pressure %>%
  select(datetime, New.York)

names(pressure)[2] = 'Pressure'

windSpeed = windSpeed %>%
  select(datetime, New.York)
names(windSpeed)[2]= 'Wind Speed'


windDirection = windDirection %>%
  select(datetime, New.York)
names(windDirection)[2] = 'Wind Direction'


temp = temp %>%
  select(datetime, New.York)
names(temp)[2] = 'temperature'

humidity = humidity %>%
  select(datetime, New.York)
names(humidity)[2] = 'humidity'

weather = merge(pressure, windSpeed, by = c('datetime'), all = TRUE)
weather = merge(weather, windDirection, by = c('datetime'), all = TRUE)
weather = merge(weather, temp, by = c('datetime'), all = TRUE)
weather = merge(weather, humidity, by = c('datetime'), all = TRUE)

weather$time =str_sub(weather$datetime, -8, -4)

weather$date = str_sub(weather$datetime, 1,10)

weather$datetime = NULL

write.csv(file = 'weather.csv', weather)
