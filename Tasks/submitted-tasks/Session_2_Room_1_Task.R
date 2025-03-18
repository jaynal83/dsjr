library(tidyverse)
library(readxl)

path = './data/VEGIEHAT-Pilot-Database.xlsx'

data <- read_excel(path)

df <- data |>
  #choosing proper rows
  filter(
    !is.na(ItemsToChoose),
    nchar(ItemsToChoose)>0,
    nchar(UserId)>0
  ) |>
  #selecting our needed columns
  select(SubmissionId, UserId, ItemsToChoose, DistrictName, UpazilaName,
         SubmissionTime, starts_with('Value'), 'Your Comments') |>
  #renaming 1 column
  rename('Comments' = 'Your Comments') |>
  #expanding items to choose
  separate_rows(ItemsToChoose, sep = ',') |>
  #time shenanigans, very silly !!!
  mutate(ItemsToChoose= str_trim(ItemsToChoose, side='both')) |>
  mutate(SubmissionDateOnly=as.Date(SubmissionTime),
         SubmissionTimeOnly=format(SubmissionTime, "%H:%M:%S"),
         ) |>
  #converting our wide data of individual product prices to long data 
  pivot_longer(
    starts_with("Value"),
    names_to = 'values',
    values_to = 'Price'
  ) |>
  #removing prices that that do not match itemsToChoose products
  filter(!is.na(Price), str_detect(values,ItemsToChoose)) |>
  #setting purchase unit for each product, only 1 liquid and 1 dozen-ized.
  mutate(
    PurchaseUnit = ifelse(ItemsToChoose=='Soybean Oil','1L',
                          ifelse(ItemsToChoose=='Eggs', '4 eggs', '1kg'))
  ) |>
  #selecting the necessary variables in order
  select(SubmissionId, SubmissionDateOnly, SubmissionTimeOnly,
         UserId, DistrictName, DistrictName,
         ItemsToChoose, PurchaseUnit, Price,
         Comments)
#export
write.csv(df, './Tasks/submitted-tasks/s2r1.csv', row.names = F)
#
# programmed by
#
# Mahbub Elahi
# Ahmed Ar Rafi
# Khalid Muntasir
# ? Sorry I forgot the last person :(