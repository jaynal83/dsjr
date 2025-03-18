library(tidyverse)
library(readxl)

path <- "data/Air Quality Data.xlsx"
str_sub(path, -4, -1)

cityNames <- excel_sheets(path)

data <- read_excel(path, cityNames[1]) |> mutate(city=cityNames[1])

for (cityName in cityNames[2:15]){
  new_data <- read_excel(path, cityName) |> mutate(city=cityName)
  data <- bind_rows(data, new_data)
}
rm(new_data, cityName)

data <- data |> mutate(city=as.factor(city))

data |>
  select(-Temperature) |>
  #filter(city=='Gazipur') |>
  filter(!is.na(Date), !is.na(PM2.5)) |>
  #tidy-ed more than 280,000 rows with these date and time shenanigans.
  mutate(dateOnly = str_sub(Date,1,10)) |>
  mutate(timeOnly = ifelse(str_detect(Time, ":"),
                           ifelse(str_detect(Time, 'PM'), str_sub(Date,-8,-1),
                                  paste0(Time,':00')),
                                      str_sub(Date,-8,-1))) |>
  mutate(dateTime = as_datetime(paste0(dateOnly,' ',timeOnly))) |>
  filter(!is.na(dateTime)) |>
  mutate(month = month(dateTime)) |>
  #glimpse()
  ggplot(aes(dateTime, PM2.5, col = month)) +
  geom_point(size = .3) +
  geom_smooth(method= 'lm', col= 'darkblue', linewidth = .7) + 
  geom_smooth(method= 'gam', col='darkred', linewidth = .7) +
  facet_wrap(~city) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90))
