library(dplyr)
library(hms)
library(readxl)
library(lubridate)
library(ggplot2)

# Load data
Air_Quality_Data <- read_excel("data/Air Quality Data.xlsx", sheet = "Dhaka")

# Define AQI breakpoints
aqi_breakpoints <- data.frame(
  PM25_Low = c(0, 12.1, 35.5, 55.5, 150.5, 250.5),
  PM25_High = c(12.0, 35.4, 55.4, 150.4, 250.4, 500.4),
  AQI_Low = c(0, 51, 101, 151, 201, 301),
  AQI_High = c(50, 100, 150, 200, 300, 500)
)

# AQI Calculation Function
calculate_aqi <- function(pm_value) {
  row <- aqi_breakpoints[pm_value >= aqi_breakpoints$PM25_Low & pm_value <= aqi_breakpoints$PM25_High, ]
  
  if (nrow(row) == 0) return(NA)  # Handle out-of-range values
  
  aqi <- ((row$AQI_High - row$AQI_Low) / (row$PM25_High - row$PM25_Low)) * (pm_value - row$PM25_Low) + row$AQI_Low
  
  return(round(aqi))
}

# Process Data
new_data <- Air_Quality_Data %>%
  mutate(
    Date = as.Date(Date),
    Time = parse_hm(Time)
  ) %>%
  filter(!is.na(Time), !is.na(Date), !is.na(PM2.5)) %>%
  rowwise() %>%
  mutate(AQI = calculate_aqi(PM2.5),
         Day = day(Date),
         Month = month(Date),
         Year = year(Date)
         ) %>%  # Correctly apply function row-wise
  ungroup()  # Remove row-wise grouping

graph <- ggplot(new_data, aes(x = Year, y = AQI, col = Day)) +
  facet_wrap(vars(Month), scales = "free") +
  geom_point() +
  geom_hline(yintercept = c(50, 100, 150, 200, 250, 300), 
             color = "red", linetype = "dashed") +
  labs(title = "Air Quality Index Over Time (Month wise)",
       y = "AQI") + 
  theme_bw()
print(graph)

