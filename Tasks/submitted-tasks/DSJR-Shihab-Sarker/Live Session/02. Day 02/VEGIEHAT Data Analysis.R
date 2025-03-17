# Load Packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Import data
dfVEGIEHAT <- read_xlsx("./Data/VEGIEHAT-Pilot-Database.xlsx", sheet = "Sheet1",
                        range = cell_cols("A:F"),
                        col_types = c("text","date","text","text","text","text" ))

  
# Filter rows with non-empty values
dfItemsChosen <- dfVEGIEHAT %>%
  filter(
    !is.na(ItemsToChoose),            # Remove NA in ItemsToChoose
    nchar(ItemsToChoose) > 0,         # Remove empty ItemsToChoose
    nchar(UserId) > 0                 # Remove empty UserId
  ) %>%
  select(
    SubmissionId, UserId, ItemsToChoose  # Select relevant columns
  ) %>%
  separate_rows(
    ItemsToChoose, sep = ","            # Split comma-separated items
  ) %>%
  mutate(
    ItemsToChoose = str_trim(
      ItemsToChoose,                    # Trim leading/trailing spaces
      side = "both"
    )
  ) %>%
  distinct(
    SubmissionId, UserId, ItemsToChoose  # Remove duplicates
  ) %>%
  mutate(
    ItemChosen = 1                      # Mark item as chosen
  ) %>%
  pivot_wider(
    names_from = ItemsToChoose,           # Create columns for each item
    values_from = ItemChosen,             # Set value to 1 for chosen items
    values_fill = list(ItemChosen = 0)    # Fill missing values with 0
  )

# Find the frequency of selecting 'Soybean oil'
frequency_soybean_oil <- dfItemsChosen %>%
  count(`Soybean Oil`)
print(frequency_soybean_oil)
# Interpretation: The frequency of selecting Soybean Oil is 77.

# Find the frequency of selecting 'Soybean oil'
frequency_soybean_oil_eggs <- dfItemsChosen %>%
  count(`Soybean Oil`, Eggs)
print(frequency_soybean_oil_eggs)
# Interpretation: The frequency of selecting Soybean Oil and Eggs is 53.
