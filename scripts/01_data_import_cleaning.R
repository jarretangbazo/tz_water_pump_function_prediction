# 01_data_import_cleaning.R
# Purpose: Import and clean raw water pump data for analysis
# Author: Jarret Angbazo
# Date: r format(Sys.Date(), '%B %d, %Y')`


# --------------------------
# Setup
# --------------------------
# Load libraries
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
#renv::snapshot()

# Set logs
log_file <- here("logs", "data_cleaning.log")
sink(log_file, append = FALSE, split = TRUE)

cat("=== DATA CLEANING STARTED ===\n")
start_time <- Sys.time()

# Create directories if missing
dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)


# --------------------------
# Import Raw Data
# --------------------------
cat("\n--- Importing Raw Data ---\n")

# Import raw data
# - Source: data/raw/water_pumps.csv
train_values <- read_csv(
  here("data", "raw", "train_values.csv"),
  col_types = cols(.default = col_character())
) %>% clean_names()

train_labels <- read_csv(
  here("data", "raw", "train_labels.csv"),
  col_types = cols(.default = col_character())
) %>% clean_names()


# --------------------------
# Clean Data
# --------------------------
# Data cleaning
# - Remove empty strings
# - Standardize missing values
# - Drop redundant columns
# - drop irrelevant features
# - dealing with numerics, e.g. dealing with zeros, scaling numeric values, binning values, handling outliers, dealing with datetime, dimension reduction for categorical data
cat("\n--- Cleaning Data ---\n")

merged_data <- train_values %>%
  left_join(train_labels, by = "id") %>%
  mutate(
    # Convert dates with validation
    installation_date = ymd(installation_date),
    date_recorded = ymd(date_recorded),
    
    # Create pump age feature
    pump_age = as.numeric(difftime(date_recorded, installation_date, units = "days") / 365.25),
    
    # Handle numeric conversions
    across(c(amount_tsh, gps_height, population), as.numeric),
    
    # Convert categoricals to factors
    across(where(is.character), ~ factor(.)),
    
    # Handle special missing values
    across(where(is.factor), ~ fct_na_value_to_level(., level = "Unknown"))
  ) %>%
  
  # Filter based on report parameters
  filter(
    region == params$region,
    year(installation_date) >= params$start_year
  ) %>%
  
  # Remove redundant columns
  select(-c(id, num_private, recorded_by)) %>%
  
  # Handle missing values
  # Median imputation
  mutate(
    pump_age = if_else(pump_age < 0 | is.na(pump_age), median(pump_age, na.rm = TRUE), pump_age),
    across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE)))
  )

# --------------------------
# Data Validation & Checks
# --------------------------
cat("\n--- Running Data Validation ---\n")

# Check for remaining NAs
na_check <- merged_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count")

# Check class distribution
status_distribution <- merged_data %>%
  count(status_group) %>%
  mutate(proportion = n / sum(n))

# --------------------------
# 6. Save Processed Data
# --------------------------
cat("\n--- Saving Processed Data ---\n")
# Create table with variable, cleaning action taken, and justification.
# For example:
# | Variable       | Action    | Justification        |
# |----------------|-----------|----------------------|
# | id             | drop      | all ids are unique   |
# | amount_tsh     | binning   | large outliers       |
# | date_recorded  | convert   | datetime not useful  |
# | region         | Nothing   |                      |


write_csv(merged_data, here("data", "processed", "cleaned_pump_data.csv"))
saveRDS(merged_data, here("data", "processed", "model_ready_data.rds"))

# --------------------------
# 7. End Cleaning
# --------------------------
end_time <- Sys.time()
duration <- round(end_time - start_time, 1)

cat("\n=== END CLEANING ===")
cat("\nDuration:", duration, "minutes")
cat("\nNA Counts:\n")
print(na_check)
cat("\nStatus Distribution:\n")
print(status_distribution)

sink()
