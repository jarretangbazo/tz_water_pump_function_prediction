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
library(sf)
#library(lubridate)
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
# - Source: https://www.drivendata.org/competitions/7/pump-it-up-data-mining-the-water-table/data/
train_values <- read_csv(
  here("data", "raw", "tz_train_values.csv"),
  col_types = cols(
    permit = col_logical(),
    public_meeting = col_logical()
    )
  ) %>% clean_names()


train_labels <- read_csv(
  here("data", "raw", "tz_train_labels.csv"),
  col_types = cols(
    status_group = col_factor(
      levels = c("functional", "functional needs repair", "non functional"), 
      ordered = TRUE
      )
    )
  ) %>% 
  clean_names() 


# Merge Data
merged_data <- train_values %>%
  left_join(train_labels, by = "id") %>% 
  arrange(id)


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


# Clean ID
## Remove duplicate IDs
merged_data %>% 
  count(id) %>% 
  filter(n > 1)

## Check ID/WPT_NAME mapping is 1:1
problem_ids <- merged_data %>% 
  group_by(id) %>% 
  summarise(wpt_name_count = n_distinct(wpt_name)) %>% 
  filter(wpt_name_count > 1)
# NOTE: No ID w/ more than one WPT_NAME

problem_wpt_name <- merged_data %>% 
  group_by(wpt_name) %>% 
  summarise(id_count = n_distinct(id)) %>% 
  filter(id_count > 1)
# NOTE: WPT_NAME to ID is not 1:1 -- will use id as unique identifier and will drop 'wpt_name'


# Clean/Validate Geographic Data
geog_data_validation <- merged_data %>% 
  select("id", "longitude", "latitude", "basin", "subvillage", "region", "region_code", "district_code", "lga", "ward") %>% 
  ## Flag missing/implausible geographic coordinates. 
  ### Tanzania lat/long range: 1 to 12 degrees south (lat); 29 to 41 degrees east (long)
  mutate(
    invalid_coords_flag = is.na(latitude) | is.na(longitude) | latitude < -12 | latitude > 0 | longitude < 29 | longitude > 41
    )
  ## Flag inconsistencies in region/region_code


tanzania_regions <- st_read(here("data", "tz_shapefile", "nbs_tz_shapefiles", "Tanzania GIS Maps", "Tanzania.shp"))


geog_data_validation %>% 
  count(region, region_code) %>% 
  print(n = 100)

  mutate(mutate(mutate(
    across(
      .cols = c("subvillage", "region", "lga", "ward", "basin","funder","installer","wpt_name"),
      .fns = list(
        cleaned = ~str_to_lower(str_squish(.))
      ),
      .names = "{.col}_clean"
    )
  )

admin_division_map <- admin_division_check %>% 
  count(region,lga,ward) %>% 
  arrange(desc(region))


table(admin_division_check$basin)



admin_division_check %>% 
  count(extraction_type_class) %>% 
  arrange(desc(n)) %>% 
  print(n=2100)


# admin_division_map <- admin_division_check %>% 
#  distinct(region, lga, ward)
admin_division_map <- admin_division_check %>% 
  count(region, lga, ward) %>% 
  arrange(desc(n))


  

merged_data %>% 
  select(where(is.character) %>% 
  map(unique)

## Check REGION mapped to unique REGION_CODE
problem_region <- admin_division_check %>% 
  group_by(region_clean) %>% 
  summarise(reg_code_count = n_distinct(region_code)) %>% 
  filter(reg_code_count > 1)

## Clean Administrative Division Names
## REGION > LGA (DISTRICT) > WARD > SUBVILLAGE

## check for subvillage names assigned to multiple administrative units
subvillage_check  <-  admin_division_check %>% 
  group_by(subvillage_clean) %>% 
  summarise(
    n_regions = n_distinct(region_clean),
    n_lgas = n_distinct(lga_clean),
    n_wards = n_distinct(ward_clean)
  ) %>% 
  filter(n_regions > 1 | n_lgas > 1 | n_wards > 1)
  

## Use Latitude/Longitude to check for spatial outliers within each subvillage
lat_long_check <- admin_division_check %>% 
  group_by(subvillage_clean) %>% 
  summarise(
    lat_sd = sd(latitude, na.rm = TRUE),
    long_sd = sd(longitude, na.rm = TRUE),
    n = n()
  ) %>% 
  filter(lat_sd > threshold | long_sd > threshold)



  group_by(subvillage) %>% 
  summarise(n_wards = n_distinct(ward))



## Clean CHAR VARS -- check for duplicates based on inconsistent spelling
charvars_spell_check <- merged_data %>% 
  select("id", "funder", "installer", "wpt_name") %>% 
  mutate(
    across(
      .cols = where(is.character),
      .fns = list(
        cleaned = ~str_to_lower(str_squish(.))
      ),
      .names = "{.col}_cleaned"
    )
  ) 

charvars_n_distinct <- charvars_spell_check %>% 
  summarise(across(everything(), ~n_distinct(.)))


    

charvars_spell_check %>% 
  

  char_num_unique_values <- train_values %>% 
  select(where(is.character)) %>% 
  summarise(across(everything(), ~n_distinct(.))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "n_unique") %>% 
  arrange(desc(n_unique))

## Summary Statistics
merged_summ_stats <- merged_data %>% 
  summarise(across(where(is.numeric),
                   .fns = list(
                     min = ~min(., na.rm = TRUE),
                     median = ~median(., na.rm = TRUE),
                     mean = ~mean(., na.rm = TRUE),
                     stdev = ~sd(., na.rm = TRUE),
                     q25 = ~quantile(., 0.25, na.rm = TRUE),
                     q75 = ~quantile(., 0.75, na.rm = TRUE),
                     max = ~max(., na.rm = TRUE)
                   )
                   )) %>% 
  pivot_longer(
    everything(),
    names_pattern = "^(.*)_(min|median|mean|stdev|q25|q75|max)$",
    names_to = c("variable", ".value")
  )



## TRAIN_VALUES
### Missing count - numeric variables
train_values %>% 
  summarise(across(where(is.numeric), ~sum(is.na(.))))

### Summary Statistics Table
train_values_summ_stats <- train_values %>% 
  summarise(across(where(is.numeric),
            .fns = list(
              min = ~min(., na.rm = TRUE),
              median = ~median(., na.rm = TRUE),
              mean = ~mean(., na.rm = TRUE),
              stdev = ~sd(., na.rm = TRUE),
              q25 = ~quantile(., 0.25, na.rm = TRUE),
              q75 = ~quantile(., 0.75, na.rm = TRUE),
              max = ~max(., na.rm = TRUE)
              )
            )) %>% 
  pivot_longer(
    everything(),
    names_pattern = "^(.*)_(min|median|mean|stdev|q25|q75|max)$",
    names_to = c("variable", ".value")
  )


# Need to determine which char vars to drop, which to keep based on correlation with outcome of interest

char_num_unique_values <- train_values %>% 
  select(where(is.character)) %>% 
  summarise(across(everything(), ~n_distinct(.))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "n_unique") %>% 
  arrange(desc(n_unique))

train_values %>% 
  count(installer, sort = TRUE)



train_values_char_proper_names <- train_values %>% 
  select("id", c("installer", "funder", "scheme_name", "ward", "lga", "subvillage", "wpt_name", "basin", "region"))

train_values_char_factor <- train_values %>% 
  select("id", where(is.character), -c("installer", "funder", "scheme_name", "ward", "lga", "subvillage", "wpt_name", "basin", "region"))

train_values_char_proper_names %>% 
  map(unique)

### Check ID mapped to exactly one WPT_NAME
id_wpt_check <- train_values_char_proper_names %>% 
  group_by(id) %>% 
  summarise(n_wpt = n_distinct(wpt_name), .groups = "drop") %>% 
  filter(n_wpt > 1)

problem_ids <- id_wpt_check %>% 
  filter(n_wpt > 1)
# NOTE: ID & WPT_NAME are mapped 1:1

lga_check <- train_values_char_proper_names %>% 
  group_by(region) %>% 
  summarise(n_districts_in_region = n_distinct(lga), .groups = "drop")


train_values_char_proper_names %>% 
  filter(region == "Arusha") %>% 
  filter(lga == "Ngorongoro") %>% 
  filter(ward == "Ngorongoro") %>% 
  map(unique)
#  select(lga) %>% 
#  map(unique)

train_values_char_proper_names %>% 
  group_by(region) %>% 
  map(unique)

train_values_char_factor %>% 
  map(unique)




  count(waterpoint_type)


train_labels %>% 
  ggplot(aes(status_group, fill = status_group)) +
  geom_bar() + 
  labs(x = NULL, y = NULL)







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
