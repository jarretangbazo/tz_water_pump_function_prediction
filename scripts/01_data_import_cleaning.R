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
  ) %>% clean_names() %>% 
  arrange(id)


train_labels <- read_csv(
  here("data", "raw", "tz_train_labels.csv"),
  col_types = cols(
    status_group = col_factor(
      levels = c("functional", "functional needs repair", "non functional"), 
      ordered = TRUE
      )
    )
  ) %>% 
  clean_names() %>% 
  arrange(id)


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

## Summary Stats for numeric variables
summ_stats_all <- merged_data %>% 
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


# Inspect AMOUNT_TSH
## Histogram -- Overall
amount_tsh_median <- median(merged_data$amount_tsh, na.rm = TRUE)

merged_data %>% 
  ggplot(aes(x = amount_tsh)) +
  geom_histogram(fill = "skyblue", color = "black") +
  geom_vline(
    aes(xintercept = amount_tsh_median), 
    color = "red", 
    linetype = "dashed", 
    size = 1.2
  ) +
  labs(
    title = "Histogram of amount_tsh with median line",
    x = "amount_tsh",
    y = "Frequency"
  ) +
  annotate("text", 
           x = amount_tsh_median, 
           y = Inf, 
           label = paste("Median:", round(amount_tsh_median, 2)), vjust = -0.5, color = "red") +
  theme_minimal()


## Histogram -- Exclude amount_tsh = 0 and truncate
amount_tsh_trunc <- merged_data %>% 
  select(id, amount_tsh) %>% 
  filter(amount_tsh != 0) %>% 
  mutate(
    amount_tsh_median = median(amount_tsh, na.rm = TRUE),
    amount_tsh_upper_bound = median(amount_tsh, na.rm = TRUE) + 1.5*IQR(amount_tsh, na.rm = TRUE),
    amount_tsh_outlier = amount_tsh > amount_tsh_upper_bound,
    amount_tsh_trunc = ifelse(amount_tsh < amount_tsh_upper_bound, amount_tsh, amount_tsh_upper_bound),
  ) %>% 
  select(-amount_tsh_upper_bound)

amount_tsh_median_trunc <- median(amount_tsh_trunc$amount_tsh)

amount_tsh_summ_stats %>% 
  ggplot(aes(x = amount_tsh_trunc)) +
  geom_histogram(fill = "skyblue", color = "black") +
  geom_vline(
    aes(xintercept = amount_tsh_median_trunc),
    color = "red",
    linetype = "dashed",
    size = 1.2
  ) +
  labs(
    title = "Histogram of amount_tsh (truncated) with median line",
    x = "amount_tsh (truncated)",
    y = "Frequency"
  ) +
  annotate("text",
           x = amount_tsh_median_trunc,
           y = Inf,
           label = paste("Median:", round(amount_tsh_median_trunc, 2)),
           hjust = -0.5,
           vjust = 10,
           color = "black",
           size = 5
           ) +
  theme_minimal()


# Inspect GPS_HEIGHT
## Histogram -- Overall
merged_data %>% 
  ggplot(aes(x = gps_height)) +
  geom_histogram(fill = "skyblue", color = "black") +
  geom_vline(
    aes(xintercept = median(gps_height, na.rm = TRUE)),
    color = "red",
    linetype = "dashed",
    size = 1.2
  ) +
  labs(
    title = "Histogram of gps_height with median line",
    x = "gps_height",
    y = "Frequency"
  )


# Inspect NUM_PRIVATE
merged_data %>% 
  ggplot(aes(x=num_private)) +
  geom_histogram(fill = "skyblue", color = "black") +
  geom_vline(
    aes(xintercept = median(gps_height, na.rm = TRUE)),
    color = "red",
    linetype = "dashed",
    size = 1.2
  ) + 
  labs(
    title = "Histogram of num_private with median line",
    x = "num_private",
    y = "Frequency"
  )


# Inspect POPULATION
merged_data %>% 
  ggplot(aes(x=population)) +
  geom_histogram(fill = "skyblue", color = "black") +
  geom_vline(
    aes(xintercept = median(population, na.rm = TRUE)),
    color = "red",
    linetype = "dashed",
    size = 1.2
  ) + 
  labs(
    title = "Histogram of POPULATION with median line",
    x = "population",
    y = "Frequency"
  )


# Inspect CONSTRUCTION_YEAR
merged_data %>% 
  select(construction_year) %>% 
  table()
# NOTE: 20709 cases with construction_year = 0


# Validate REGION x REGION_CODE
merged_data %>% 
  distinct(region_code, region) %>% 
  arrange(region_code) %>% 
  print(n = 40)


## Heatmap -- region x region_code
merged_data %>% 
  count(region, region_code) %>% 
  ggplot(aes(y = region, x = region_code, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = region_code), size = 3) +
  scale_fill_gradient(
    low = "white",
    high = "steelblue"
  ) +
  labs(
    title = "Region vs Region_Code Mapping",
    x = "Region Code",
    y = "Region",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
    )
# NOTE: Region and Region_Code are not 1:1



#### Inspect Char variables

# Inspect WATERPOINT_TYPE_GROUP and WATERPOINT_TYPE
merged_data %>% 
  distinct(waterpoint_type_group, waterpoint_type)

# Inspect SOURCE X SOURCE_TYPE X SOURCE_CLASS
merged_data %>% 
  distinct(source_class, source_type, source) %>% 
  arrange(source_class, source_type, source)

# Inspect QUANTITY X QUANTITY_GROUP
merged_data %>% 
  distinct(quantity_group, quantity) %>% 
  arrange(quantity_group)

# Inspect QUALITY_GROUP X WATER_QUALITY
merged_data %>% 
  distinct(quality_group, water_quality) %>% 
  arrange(quality_group, water_quality)

# Inspect PAYMENT X PAYMENT_TYPE
merged_data %>% 
  distinct(payment_type, payment) %>% 
  arrange(payment_type, payment)

# Inspect EXTRACTION_TYPE X EXTRACTION_TYPE_GROUP X EXTRACTION_TYPE_CLASS
merged_data %>% 
  distinct(extraction_type_class, extraction_type_group, extraction_type) %>% 
  arrange(extraction_type_class, extraction_type_group, extraction_type)

# Inspect MANAGEMENT X MANAGEMENT_GROUP
merged_data %>% 
  distinct(management_group, management) %>% 
  arrange(management_group, management)

# Inspect SCHEME_MANAGEMENT X MANAGEMENT
merged_data %>% 
  distinct(scheme_management, management) %>% 
  arrange(scheme_management, management) %>% 
  print(n=100)



#### Clean/Validate Geographic Data
admin_division_map <- admin_division_check %>% 
  count(region,lga,ward) %>% 
  arrange(desc(region))

geog_data_validation <- merged_data %>% 
  select("id", "longitude", "latitude", "basin", "subvillage", "region", "region_code", "district_code", "lga", "ward") %>% 
  ## Flag missing/implausible geographic coordinates. 
  ### Tanzania lat/long range: 1 to 12 degrees south (lat); 29 to 41 degrees east (long)
  mutate(
    invalid_coords_flag = is.na(latitude) | is.na(longitude) | latitude < -12 | latitude > 0 | longitude < 29 | longitude > 41
    )


## Visually inspect Regions on map to identify errors
tanzania_regions <- st_read(here("data", "tz_shapefile", "nbs_tz_shapefiles", "Tanzania GIS Maps", "Tanzania.shp"))


#  mutate(mutate(mutate(
#    across(
#      .cols = c("subvillage", "region", "lga", "ward", "basin","funder","installer","wpt_name"),
#      .fns = list(
#        cleaned = ~str_to_lower(str_squish(.))
#      ),
#      .names = "{.col}_clean"
#    )
#  )


## Use Latitude/Longitude to check for spatial outliers within each subvillage
lat_long_check <- admin_division_check %>% 
  group_by(subvillage_clean) %>% 
  summarise(
    lat_sd = sd(latitude, na.rm = TRUE),
    long_sd = sd(longitude, na.rm = TRUE),
    n = n()
  ) %>% 
  filter(lat_sd > threshold | long_sd > threshold)



# --------------------------
# Finalize Data Cleaning
# --------------------------
## Clean
tz_final_data <- merged_data %>% 
  # Drop variables no longer needed
  select(-c("wpt_name", "funder", "installer")) %>% 
  mutate(
    # Create *_cleaned variables
    across(
      .cols = c("region", "lga", "ward", "basin"),
      .fns = list(
        cleaned = ~str_to_lower(str_trim(.))
      ),
      .names = "{.col}_cleaned"
    ),
    # Create pump age
    pump_age = as.numeric(difftime(ymd(date_recorded), ymd(installation_date), units = "days") / 365.25),
    # Convert categoricals to factors
    across()
    ) %>% 
  
  

## Add additional features

# Need to determine which char vars to drop, which to keep based on correlation with outcome of interest
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
