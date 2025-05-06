# 02_exploratory_data_analysis.R
# Purpose: Exploratory Data Analysis for Tanzania water pump functionality prediction
# Author: Jarret Angbazo
# Date: r format(Sys.Date(), '%B %d, %Y')`


# --------------------------
# Setup
# --------------------------
# Load libraries
library(tidyverse)
library(here)
library(skimr)
library(patchwork)
library(GGally)
library(plotly)
library(htmlwidgets)
#renv::snapshot()

# Set logs
log_file <- here("logs", "data_cleaning.log")
sink(log_file, append = FALSE, split = TRUE)

cat("=== EXPLORATORY DATA ANALYSIS STARTED ===\n")
start_time <- Sys.time()


# --------------------------
# Load Cleaned Data
# --------------------------
cat("\n--- Loading Processed Data ---\n")
cleaned_data <- read_csv(
  here("data", "processed", "cleaned_pump_data.csv"),
  col_types = cols(
    status_group = col_factor(),
    region = col_factor(),
    .default = col_character()
  )
) 

# Validate data existence
if(nrow(cleaned_data) == 0) stop("No data available for selected parameters")


# --------------------------
# Create Output Directories
# --------------------------
dir.create(here("eda", "plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("eda", "tables"), showWarnings = FALSE)

# --------------------------
# Core Analysis Functions
# --------------------------
save_plot <- function(plot_obj, filename, width = 10, height = 7) {
  ggsave(
    here("eda", "plots", filename),
    plot_obj,
    width = width,
    height = height,
    dpi = 300
  )
}

# --------------------------
# Feature Analysis
# --------------------------
# Generate EDA report
# - visualize individual features independently
# - correlations between categorical features (cramer's v, uncertainty coefficient)
# - correlations between numeric features and categorical features (correlation ratio)
# - correlations between numeric features (pearson's R)
# - create histograms, heatmaps, joint plots, and pairplots

cat("\n--- Analyzing Features ---\n")


## 1. Target Variable Distribution
target_dist <- cleaned_data %>%
  ggplot(aes(status_group, fill = status_group)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Target Variable Distribution",
       subtitle = paste("Region:", params$region),
       x = "Pump Status", y = "Count") +
  theme_minimal()

save_plot(target_dist, "01_target_distribution.png")

## 2. Numerical Features Analysis
num_features <- c("pump_age", "amount_tsh", "gps_height", "population")

num_dist_plots <- cleaned_data %>%
  select(all_of(num_features)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Numerical Features Distribution") +
  theme_minimal()

save_plot(num_dist_plots, "02_numerical_distributions.png", width = 12, height = 8)

## 3. Categorical Features Analysis
cat_features <- c("water_quality", "source_type", "waterpoint_type")

cat_dist_plots <- cleaned_data %>%
  select(all_of(cat_features)) %>%
  pivot_longer(everything()) %>%
  count(name, value) %>%
  ggplot(aes(reorder(value, n), n, fill = name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Categorical Features Distribution", 
       x = NULL, y = "Count") +
  theme_minimal()

save_plot(cat_dist_plots, "03_categorical_distributions.png", width = 12, height = 8)

# --------------------------
# Temporal Analysis
# --------------------------
if("installation_date" %in% names(cleaned_data)) {
  temporal_analysis <- cleaned_data %>%
    mutate(install_year = year(installation_date)) %>%
    count(install_year, status_group) %>%
    ggplot(aes(install_year, n, fill = status_group)) +
    geom_area(position = "stack") +
    labs(title = "Pump Installation Trends",
         x = "Installation Year", y = "Count") +
    theme_minimal()
  
  save_plot(temporal_analysis, "04_temporal_analysis.png")
}

# --------------------------
# Geospatial Analysis
# --------------------------
if(all(c("longitude", "latitude") %in% names(cleaned_data))) {
  geo_plot <- cleaned_data %>%
    ggplot(aes(longitude, latitude, color = status_group)) +
    geom_point(alpha = 0.6, size = 1.5) +
    labs(title = "Pump Locations by Functional Status",
         x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  save_plot(geo_plot, "05_geospatial_distribution.png", width = 12, height = 10)
}

# --------------------------
# Correlation Analysis
# --------------------------
corr_matrix <- cleaned_data %>%
  select(where(is.numeric)) %>%
  ggpairs(
    upper = list(continuous = wrap("cor", size = 3)),
    lower = list(continuous = wrap("smooth", alpha = 0.3))
  ) +
  theme_minimal()

save_plot(corr_matrix, "06_correlation_matrix.png", width = 14, height = 12)

# --------------------------
# Interactive Visualizations
# --------------------------
if(interactive()) {
  interactive_plot <- plot_ly(
    cleaned_data,
    x = ~gps_height,
    y = ~population,
    color = ~status_group,
    type = "scatter",
    mode = "markers"
  ) %>%
    layout(title = "Interactive Pump Status Analysis")
  
  saveWidget(
    interactive_plot,
    here("eda", "plots", "07_interactive_plot.html")
  )
}

# --------------------------
# Summary Statistics
# --------------------------
cat("\n--- Generating Summary Statistics ---\n")

skim_summary <- skim(cleaned_data)
write_csv(skim_summary, here("eda", "tables", "summary_statistics.csv"))

# --------------------------
# Finalize Analysis
# --------------------------
end_time <- Sys.time()
duration <- round(end_time - start_time, 1)

cat("\n=== EDA COMPLETED ===")
cat("\nDuration:", duration, "minutes")
cat("\nOutputs saved to:", here("eda"))

sink()

