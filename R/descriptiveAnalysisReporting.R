################################################################################
# 0. Libraries
################################################################################

library(readr)
library(dplyr)
library(gtsummary)
library(labelled)

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

var_label(dfData1) <- list(
  clusterID = "Cluster ID",
  householdID = "Household ID",
  age = "Age of Participants",
  sexNominal = "Gender of Participants",
  maritalNominal = "Marital Status",
  painYn = "Proportion of Participants Having Pain",
  painOrdinal = "Pain Severity"
)

################################################################################
# 2. Data Analysis - Descriptive Statistics
################################################################################

dfTable1 <- dfData1 %>%
  tbl_summary(
    include = c(
      age, sexNominal, maritalNominal, painOrdinal,  painYn
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p})",
      all_continuous() ~ "{mean} ({sd})"
    )
  )

dfTable1 <- dfData1 %>%
  tbl_summary(
    include = c(
      age, sexNominal, maritalNominal, painOrdinal,  painYn
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p})",
      all_continuous() ~ "{mean} \u00B1 {sd}"
    ),
    digits = everything() ~1,
    missing_text = "(Missing)"
  )


dfTable1 <- dfData1 %>%
  tbl_summary(
    include = c(
      age, sexNominal, maritalNominal, painOrdinal,  painYn
    ),
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p})",
      all_continuous() ~ "{p50} ({p25}-{p75})"
    ),
    digits = everything() ~1,
    missing_text = "(Missing)"
  )


dfTable1 <- dfData1 %>%
  tbl_summary(
    include = c(age, maritalNominal),
    statistic = list(
      maritalNominal ~ "{n} ({p})",
      age ~ "{p50} ({p25}-{p75})"
    ),
    missing_text = "(Missing)"
  )


dfTable1 <- dfData1 %>%
  tbl_summary(
    by = clusterID,
    include = c(
      age, sexNominal, maritalNominal, painOrdinal,  painYn
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p})",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = everything() ~1,
    missing_text = "(Missing)"
  ) 

dfTable1 <- dfData1 %>%
  tbl_summary(
    by = clusterID,
    include = c(
      age, sexNominal, maritalNominal, painOrdinal,  painYn
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p})",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = everything() ~1,
    missing_text = "(Missing)"
  ) %>%
  add_p() 
  
dfTable1 <- dfData1 %>%
  tbl_summary(
    by = clusterID,
    include = c(
      age, sexNominal, maritalNominal, painOrdinal,  painYn
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p})",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = everything() ~1,
    missing_text = "(Missing)"
  ) %>%
  add_p() %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Participants Background Characteristics**") 
  
tbl_1 <- tempfile("Table-1", fileext = ".docx", tmpdir = getwd())

dfTable1 <- dfData1 %>%
  tbl_summary(
    by = clusterID,
    include = c(
      age, sexNominal, maritalNominal, painOrdinal,  painYn
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p})",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = everything() ~1,
    missing_text = "(Missing)"
  ) %>%
  add_p() %>%
  as_gt() %>%
  gt::gtsave(
    filename = tbl_1
  )

ModelProp <- lm(painYn~sexNominal, data = dfData1)

library(sjPlot)
tab_model(dfProp)
