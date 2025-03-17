

#####remove all list
rm(list=ls())

getwd()

##### Load library
library(tidyverse)
library(readr)

#####Load data
player_stat <- read_csv("./EPL-Player-Stats-Upto-2020-09-24.csv")


# View the data
str(player_stat)

player_stat <- player_stat %>%
  mutate(Shooting_1  = gsub("%", "", `Shooting accuracy %`),
         Tackle_1  = gsub("%", "", `Tackle success %`),
         Cross_1  = gsub("%", "", `Cross accuracy %`),)

player_stat <- player_stat %>%
  mutate(Shooting_1 = as.numeric(Shooting_1),
         Tackle_1 = as.numeric(Tackle_1),
         Cross_1 = as.numeric(Cross_1)    )


player_stat_data <- player_stat %>%
  select(-`Shooting accuracy %`, -`Tackle success %`, -`Cross accuracy %`) %>%
    pivot_longer(
    cols = Appearances:Cross_1,
    names_to = "StatsName",
    values_to = "Statsvalue"
  ) %>%
  filter(!is.na(Statsvalue))


player_stat_data <- player_stat_data %>%
  mutate(StatsName = ifelse(StatsName == "Shooting_1", "Shooting accuracy %", 
                            ifelse(StatsName == "Tackle_1", "Tackle success %",
                                   ifelse(StatsName == "Cross_1", "Cross accuracy %", StatsName))))


table(player_stat_data$StatsName)

