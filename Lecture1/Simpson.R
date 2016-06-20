## Read the csv file
Airlines = read.csv('Airline.csv', header = TRUE) 

## Create a frequency table by airline
require(dplyr)
airline.summary <- Airlines %>% group_by(Airline) %>% 
            summarize(PrecentDelay = 100 * sum(Delayed)/(sum(OnTime) + sum(Delayed)))
airline.summary

## Create a frequency table by airport
airport.summary <- Airlines %>% group_by(AirportCode) %>% 
  summarize(PrecentDelay = 100 * sum(Delayed)/(sum(OnTime) + sum(Delayed))) %>%
  arrange(PrecentDelay)
airport.summary
 
## Create a summary table by airport and airline
airport.by.airline <- Airlines %>% group_by(AirportCode, Airline) %>% 
  summarize(PrecentDelay = 100 * sum(Delayed)/(sum(OnTime) + sum(Delayed)),
            TotalFlights = sum(OnTime) + sum(Delayed)) %>%
            arrange(AirportCode)
airport.by.airline