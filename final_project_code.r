# Load necessary libraries
library(tidyverse)
library(lubridate)
library(nycflights23)

# Prepare the flights dataset: select relevant columns, create flight_date, and remove missing delays
flights_data <- nycflights23::flights %>%
  select(year, month, day, dep_delay, dep_time, arr_delay, arr_time, origin) %>%
  mutate(flight_date = make_date(year, month, day)) %>%
  select(flight_date, dep_delay, dep_time, arr_delay, arr_time, origin) %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay))

# Preview the first few rows
head(flights_data)

# Count flights with late arrivals by origin
late_arrivals <- flights_data %>%
  filter(arr_delay > 0) %>%
  count(origin)

# Plot number of delayed arrivals by origin airport
ggplot(late_arrivals, aes(x = origin, y = n, fill = origin)) +
  geom_col() +
  labs(
    title = "Delayed Arrivals by Airport of Departure",
    x = "Origin Airport",
    y = "Number of Late Flights"
  ) +
  guides(fill = "none")

# Count flights with late departures by origin
late_departures <- flights_data %>%
  filter(dep_delay > 0) %>%
  count(origin)

# Plot number of delayed departures by origin airport
ggplot(late_departures, aes(x = origin, y = n, fill = origin)) +
  geom_col() +
  labs(
    title = "Delayed Departures by Airport",
    x = "Origin Airport",
    y = "Number of Late Flights"
  ) +
  guides(fill = "none")

# Function to calculate the proportion of flights delayed over 15 minutes
calculate_delay_rates <- function(df) {
  df %>%
    summarise(
      dep_delay_rate = mean(dep_delay > 15, na.rm = TRUE),
      arr_delay_rate = mean(arr_delay > 15, na.rm = TRUE)
    )
}

# Separate datasets by airport
JFK_flights <- flights_data %>%
  filter(origin == "JFK") %>%
  select(flight_date, dep_delay, dep_time, arr_delay, arr_time)

EWR_flights <- flights_data %>%
  filter(origin == "EWR") %>%
  select(flight_date, dep_delay, dep_time, arr_delay, arr_time)

LGA_flights <- flights_data %>%
  filter(origin == "LGA") %>%
  select(flight_date, dep_delay, dep_time, arr_delay, arr_time)

# Compute delay rates for each airport
JFK_delay_rates <- calculate_delay_rates(JFK_flights)
EWR_delay_rates <- calculate_delay_rates(EWR_flights)
LGA_delay_rates <- calculate_delay_rates(LGA_flights)

# Display the results
JFK_delay_rates
EWR_delay_rates
LGA_delay_rates

delay_rates_all <- bind_rows(
  #The purpose of this code is to organize the results of the table into one organized data set
  
  JFK_delay_rates %>% mutate(Airport = "JFK"),
  EWR_delay_rates %>% mutate(Airport = "EWR"),
  LGA_delay_rates %>% mutate(Airport = "LGA")
)

total_delay_rates <- delay_rates_all %>%
  summarise(
    Airport = "Across All Three Airports",
    across(where(is.numeric), mean, na.rm = TRUE)
  )

final_delay_table <- bind_rows(delay_rates_all, total_delay_rates)



#Investigating Factors that Cause Flight Delay 

## 1.) Is there a correlation between the time of day the flight departs/arrives vs the rate of delay. 
### Morning -5:00 AM to 12:00 PM | Afternoon- 12:00 PM to 5:00 PM | Evening - 5:00 PM to 11:59 AM | Night - 12:00 AM to 5:00 AM 

time_vs_rate_of_delay <- flights_data %>%
  mutate(
    time_period = case_when(
      dep_time >= 500 & dep_time < 1200 ~ "Morning",
      dep_time >= 1200 & dep_time < 1700 ~ "Afternoon",
      dep_time >= 1700 & dep_time <= 2359 ~ "Evening",
      TRUE ~ "Night"
    ),
  ) %>%
  
  delay_rate_by_time <- time_vs_rate_of_delay %>%
  group_by(time_period) %>%
  summarise(
    num_flights = n(),
    num_dep_delayed = sum(dep_delay > 15, na.rm = TRUE),
    dep_delay_rate = mean(dep_delay > 15, na.rm = TRUE),
    num_arr_delayed = sum(arr_delay > 15, na.rm = TRUE),
    arr_delay_rate = mean(arr_delay > 15, na.rm = TRUE)
  )

delay_rate_by_time

