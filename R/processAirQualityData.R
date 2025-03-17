################################################################################
# 0. Libraries
################################################################################

library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(gtsummary)
library(ggplot2)

################################################################################
# 1. Read/load/import data
# 
################################################################################

airQualityDataSheets <- excel_sheets(
  path = "./data/Air Quality Data.xlsx"
)

dfAQ <- map_dfr(
  airQualityDataSheets,
  ~read_excel(
    path = "./data/Air Quality Data.xlsx",
    sheet = .x
  ) %>%
    mutate(
      Date = date(Date),
      Time = as.integer(str_split(Time, ":")[[1]][1]),
      cityNames = .x
    ) %>%
    filter(
      !is.na(Date),
      !is.na(Time)
    )
)


dfMonthlyAQI <- dfAQ %>%
  mutate(
    Year = year(Date),
    Month = factor(
      month(Date),
      levels = c(1:12),
      labels = c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
      )
    ),
  ) %>%
  group_by(
    cityNames, Year, Month
  ) %>%
  summarise(
    avgPM2.5 = mean(PM2.5, na.rm = T),
    AQI = factor(
      case_when(
        avgPM2.5>0 & avgPM2.5<=50 ~1,
        avgPM2.5>50 & avgPM2.5<=100~2,
        avgPM2.5>100 & avgPM2.5<=150~3,
        avgPM2.5>150 & avgPM2.5 <=200~4,
        avgPM2.5>200 & avgPM2.5<=300~5,
        avgPM2.5>300 ~ 6
      ),
      levels = c(1,2,3,4,5,6),
      labels = c(
        "Good", "Moderate", "Unhealthy for Sensitive Groups", 
        "Unhealthy", "Very Unhealthy", "Hazardous"
      ),
      ordered = TRUE
    ),
    avgTemp = mean(Temperature, na.rm = T)
  )



ggplot(
  data = dfMonthlyAQI %>% 
    filter(
      cityNames=="Dhaka"
    ),
  aes(
    x = Month,
    y = factor(Year)
  )
)+
  geom_tile(
    aes(
      fill = AQI
    ),
    alpha =0.35
  )+
  geom_text(
    aes(
      label = round(avgPM2.5, digits = 0)
    )
  )+
  scale_fill_manual(
    values = c(
      "Good"="green", 
      "Moderate" = "yellow", 
      "Unhealthy for Sensitive Groups" = "orange", 
      "Unhealthy" ="red",
      "Very Unhealthy" ="purple",
      "Hazardous" = "maroon"
    )
  )+
  coord_fixed()+
  theme_bw()+
  xlab("")+
  ylab("Years")+
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )
