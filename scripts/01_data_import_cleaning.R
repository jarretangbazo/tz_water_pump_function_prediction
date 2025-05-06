# 01_data_import_cleaning.R
# Purpose: Import and clean raw water pump data for analysis
# Author: Jarret Angbazo
# Date: 2025-05-05


# Load libraries
library(tidyverse)
library(here)

# Create directories if missing
dir.create(here("data", "processed"), showWarnings = FALSE)

# Import raw data
# - Source: data/raw/water_pumps.csv
raw_data <- read_csv(here("data", "raw", "water_pumps.csv"))

# Data cleaning
# - Remove empty strings
# - Standardize missing values
# - Drop redundant columns
cleaned_data <- raw_data %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(install_year = lubridate::year(install_date)) %>%
  drop_na(critical_columns) %>%  # Define critical columns
  select(-redundant_column) %>%  # Remove unnecessary columns
  write_csv(here("data", "processed", "cleaned_data.csv"))

# Save cleaned data
write_csv(cleaned_data, here("data", "processed", "cleaned_data.csv"))


# Notes:
# - Dropped 'redundant_column' due to high missingness.
# - All file paths use here::here() for portability.