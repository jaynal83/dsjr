library(dplyr)
library(hms)
library(readxl)
library(lubridate)

city_names <- excel_sheets("data/Air Quality Data.xlsx")

air_data <- lapply(city_names, function(city) {
  read_excel("data/Air Quality Data.xlsx", 
             sheet = city) %>%
    mutate(CityName = city) 
}) %>% bind_rows()

glimpse(air_data)

air_data <- air_data %>%
  mutate(
    Date = date(Date),
    Time = parse_hm(Time), 
    CityName = as.factor(CityName),
         ) %>%
  select(Date, Time, CityName, PM2.5, Temperature)
head(air_data)
air_data$CityName 
View(air_data)
