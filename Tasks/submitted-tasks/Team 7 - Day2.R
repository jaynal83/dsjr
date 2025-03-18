# Importing libraries

library(tidyverse)
library(readxl)

# Reading the datafile
VEGIEHAT_Pilot_Database <- read_excel("data/VEGIEHAT-Pilot-Database.xlsx")
#View(VEGIEHAT_Pilot_Database)
#names(VEGIEHAT_Pilot_Database)

newData = VEGIEHAT_Pilot_Database %>%
  # Filter the data with these columns
  filter(                           
    !is.na(`ItemsToChoose`),
    nchar(`ItemsToChoose`)>0, 
    nchar(`SubmissionId`)>0,
    nchar(`SubmissionTime`)>0,
    nchar(DistrictName)>0,
    nchar(UpazilaName)>0,
  ) %>% 
  # Creating Two columns by splitting the SubmissionTime column
  mutate(
    SubmissionDateOnly = as.Date(substr(SubmissionTime,1, 10)),
    SubmissionTimeOnly = substr(SubmissionTime,12, 19),
  )%>%
  # Separate ItemToChoose column's every Item as new row
  separate_rows(
    `ItemsToChoose`, sep = ","
  ) %>%
  # Merging every columns with price values to a single price column
  pivot_longer(
    cols = starts_with("Value -"), 
    names_to = "price_type", 
    values_to = "Price", 
    values_drop_na = TRUE
  ) %>% 
  # Creating the PurchaseUnit column where eggs-4pcs, Soyabean Oil-1L 
  # and unit of other items is 1 kg
  mutate(
    PurchaseUnit = ifelse(ItemsToChoose=="Soybean Oil", "1 L", 
                          ifelse(ItemsToChoose=="Eggs", " 4pcs", "1 kg"))
  )%>%
  # Renaming the Your Comments to Comments. Here we used backtics(``) for 
  # specifying column name in which space had used.
  rename(
    Comments = `Your Comments`
  )%>%
  # Now select or filter by column names for our desired dataFrame
  select(SubmissionId, SubmissionDateOnly,SubmissionTimeOnly, UserId, 
         DistrictName, UpazilaName, ItemsToChoose, PurchaseUnit, Price, Comments)


view(newData)
dim(VEGIEHAT_Pilot_Database)
dim(newData)

glimpse(newData)
