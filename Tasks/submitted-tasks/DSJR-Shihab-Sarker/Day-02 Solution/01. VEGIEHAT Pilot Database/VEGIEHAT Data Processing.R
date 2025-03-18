# Load Packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Import data
dfVEGIEHAT <- read_xlsx("./Data/VEGIEHAT-Pilot-Database.xlsx", sheet = "Sheet1")

# Filter rows with non-empty values
dfItemsChosen <- dfVEGIEHAT %>%
  filter(
    !is.na(ItemsToChoose),
    nchar(ItemsToChoose) > 0,
    nchar(UserId) > 0
  ) %>%
  select(
    SubmissionId, SubmissionTime, UserId, DistrictName,
    UpazilaName, ItemsToChoose, `Value - Rice`, `Value - Flour`, `Value - Lentil`, 
    `Value - Soybean Oil`, `Value - Salt`, `Value - Sugar`, `Value - Eggs`, 
    `Value - Chicken`, `Value - Potato`, `Value - Eggplant`, `Value - Onion`, 
    `Value - Green Chilli`
  ) %>%
  mutate(
    ItemsToChoose = str_trim(ItemsToChoose, side = "both"),
    SubmissionDateOnly = format(as.Date(SubmissionTime), "%d/%m/%Y"),
    SubmissionTimeOnly = format(SubmissionTime, "%H:%M:%S")
  ) %>%
  separate_rows(
    ItemsToChoose, sep = ","
  ) %>%
  mutate(
    ItemsToChoose = str_trim(
      ItemsToChoose,
      side = "both"
    )
  ) %>%
  mutate(
    PurchaseUnit = case_when(
      ItemsToChoose %in% c("Rice", "Flour", "Lentil", "Salt", "Sugar", "Potato", 
                           "Eggplant", "Onion", "Green Chilli", "Chicken") ~ "1 kg",
      ItemsToChoose == "Soybean Oil" ~ "1L",
      ItemsToChoose == "Eggs" ~ "1 Hali",
      TRUE ~ NA_character_
    )
    )%>%
  mutate(
    Price = case_when(
      ItemsToChoose == "Rice" ~ `Value - Rice`,
      ItemsToChoose == "Flour" ~ `Value - Flour`,
      ItemsToChoose == "Lentil" ~ `Value - Lentil`,
      ItemsToChoose == "Soybean Oil" ~ `Value - Soybean Oil`,
      ItemsToChoose == "Salt" ~ `Value - Salt`,
      ItemsToChoose == "Sugar" ~ `Value - Sugar`,
      ItemsToChoose == "Eggs" ~ `Value - Eggs`,
      ItemsToChoose == "Chicken" ~ `Value - Chicken`,
      ItemsToChoose == "Potato" ~ `Value - Potato`,
      ItemsToChoose == "Eggplant" ~ `Value - Eggplant`,
      ItemsToChoose == "Onion" ~ `Value - Onion`,
      ItemsToChoose == "Green Chilli" ~ `Value - Green Chilli`,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
  SubmissionId, SubmissionDateOnly, SubmissionTimeOnly, UserId, DistrictName,
  UpazilaName, ItemsToChoose, PurchaseUnit, Price
  )

show(dfItemsChosen)