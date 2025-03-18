################################################################################
# 0. Libraries
################################################################################

library(readr)
library(dplyr)
library(gtsummary)

################################################################################
# 1. Read/load/import data
################################################################################

dfData1 <- read_csv(
  file = "./data/Data-1.csv"
)

# Checking variable properties and first few data points
glimpse(dfData1)

# Defining appropriate measurement scale for "sex" 
# "pain" and "marital"
dfData1 <- dfData1 %>%
  mutate(
    sexNominal = factor(
      x = sex,
      levels = c(1,2),
      labels = c("Male", "Female")
    ),
    painOrdinal = factor(
      x = pain,
      levels = c(1,2, 3),
      labels = c("No Pain", "Mild Pain", "Severe Pain"),
      ordered = TRUE
    ),
    maritalNominal = factor(
      x = marital,
      levels = c(1,2,3,4),
      labels =  c("Married", "Divorced", "Widow(er)", "Never Married")
    ),
    painYn = as.logical(painYn)
  )
