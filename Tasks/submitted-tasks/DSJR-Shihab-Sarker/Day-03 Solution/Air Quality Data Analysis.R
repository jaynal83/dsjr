# Load Packages
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(hms)
library(purrr)
library(ggplot2)

# Import data
city_names <- excel_sheets("data/Air Quality Data.xlsx")

dfAirQuality <- map_df(city_names, ~ {
  read_excel("data/Air Quality Data.xlsx", 
             sheet = .x,
             col_types = c("date", "text", "numeric", "numeric")) %>%
    mutate(CityName = .x)
})

# Clean and process the data
dfAirQuality_cleaned <- dfAirQuality %>%
  transmute(
    Date = as.Date(Date),
    Time = parse_hm(Time),
    CityName = as.factor(CityName),
    PM2.5,
    Temperature
  ) %>%
  drop_na()

# View the cleaned data
show(dfAirQuality_cleaned)


# Clean and process the data
dfAirQuality_cleaned <- dfAirQuality %>%
  transmute(
    Date = as.Date(Date),
    Time = parse_hm(Time),
    CityName = as.factor(CityName),
    PM2.5,
    Temperature
  ) %>%
  drop_na()

# Combine Date and Time into a single datetime column
dfAirQuality_cleaned <- dfAirQuality_cleaned %>%
mutate(DateTime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M"))

# View the cleaned data
print(dfAirQuality_cleaned)

# Filter Dhaka city only
dfDhaka <- dfAirQuality_cleaned %>%
  filter(CityName == "Dhaka")

# Plot the data
ggplot(dfDhaka, aes(x = Date)) +
  geom_smooth(aes(y = PM2.5, color = "PM2.5")) +
  labs(title = "Changes in PM2.5 over Time in Dhaka City",
       x = "Year",
       y = "PM2.5",
       color = "Legend") +
  theme_minimal()