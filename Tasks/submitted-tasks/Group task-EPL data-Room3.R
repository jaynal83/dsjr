# Load required packages
library(tidyverse)


# Read the CSV file
epl_data <- read.csv("G:/EPL-Player-Stats-Upto-2020-09-24.csv")

# Print column names to verify
colnames(epl_data)


# Clean the data (remove percentage symbols and convert to numeric)
epl_data_cleaned <- epl_data %>%
  mutate(across(c(`Tackle.success..`, `Cross.accuracy..`, `Shooting.accuracy..`), ~ as.numeric(gsub("%", "", .))))

# Convert the data from wide to long format
EPL_data <- epl_data_cleaned %>%
  pivot_longer(
    cols = -c(Name, Jersey.Number, Club, Position, Nationality, Age), # Update column names here
    names_to = "StatsName",
    values_to = "StatsValue"
  )

# View the first few rows of the transformed data
head(EPL_data)

# Optionally, save the transformed data to a new CSV file
write_csv(EPL_data, "epl_player_stats_long_format.csv")
