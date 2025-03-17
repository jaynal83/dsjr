
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)


file_path <- "C:/Joynal vai/dsjr-178-main/data/VEGIEHAT-Pilot-Database.xlsx"  
df <- read_excel(file_path)

str(df)

#time and date separation
df <- df %>%
  mutate(
    SubmissionTime = ymd_hms(SubmissionTime),  
    SubmissionDateOnly = as.Date(SubmissionTime), 
    SubmissionTimeOnly = format(SubmissionTime, "%H:%M:%S")  )
  )

# Convert wide data to long format
df_long <- df %>%
  pivot_longer(
    cols = starts_with("Value"),  
    names_to = "ItemsToChoose_1",
    values_to = "Price"
  ) %>%
  mutate(
    ItemsToChoose_1 = gsub("^Value\\s*-\\s*", "", ItemsToChoose),  # Remove 'Value - ' prefix
    PurchaseUnit = case_when(
      ItemsToChoose == "Soybean Oil" ~ "1L",
      TRUE ~ "1 kg"
    )
  ) %>%
  filter(!is.na(Price))  

# Select relevant columns and re-order them
final_data <- df_long %>%
  select(
    SubmissionId, SubmissionDateOnly, SubmissionTimeOnly, UserId,
    DistrictName, UpazilaName, ItemsToChoose_1, PurchaseUnit, Price,`Your Comments`
  )




head(final_data)
