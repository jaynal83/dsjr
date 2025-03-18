# Load Packages
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(hms)
library(purrr)

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
