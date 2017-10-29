install.packages("weatherData")

library(weatherData)
library(tidyverse)

#install.packages("devtools")
#library("devtools")
#install_github("Ram-N/weatherData")

getWeatherForDate("PIT", "2014-05-05")

checkDataAvailabilityForDateRange("SFO", "2010-10-29", "2013-01-12")
