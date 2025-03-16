library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)


setwd("C://Users//Dell//Desktop//Data Science with R")
file_path <- "C:/Users/Dell/Desktop/Data Science with R/VEGIEHAT-Pilot-Database.xlsx"
dfVEGIEHAT <- read_excel(file_path)
head(dfVEGIEHAT)


dfItemsChosen <- dfVEGIEHAT %>%
  filter(
    !is.na(ItemsToChoose),
    nchar(ItemsToChoose) > 0,
    nchar(UserId) > 0
  ) %>%
  select(
    SubmissionId, SubmissionTime
    , UserId, ItemsToChoose
  )
head(dfItemsChosen)
dfItemsChosen=dfItemsChosen %>% separate(SubmissionTime, into = c("Date", "Time"), sep = " ") 
head(dfItemsChosen)

dfItemsChosen <- dfItemsChosen %>%
  separate_rows(ItemsToChoose, sep = ",") %>%
  mutate(
    ItemsToChoose= stringr::str_trim( # Corrected str_trim
      ItemsToChoose
      ,
      side = "both"
    ))
head(dfItemsChosen)

dfItemsChosen <- dfItemsChosen %>%
  distinct(
    SubmissionId
    , Date, Time, UserId, ItemsToChoose
    
  )

head(dfItemsChosen)


dfItemsChosen <- dfItemsChosen %>%
  mutate(
    itemChosen = 1
  ) %>%
  pivot_wider(
    names_from = ItemsToChoose,
    values_from = itemChosen,
    values_fill = list(itemChosen = 0)
  )

head(dfItemsChosen)
write.csv(dfItemsChosen,"edited.csv")
