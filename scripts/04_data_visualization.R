# 04_data_visualization.R
# Tanzania Water Pump Functionality - Visualization Script
# Author: Jarret Angbazo
# Date: `r format(Sys.Date(), '%B %d, %Y')`

# --------------------------
# Setup
# --------------------------
library(tidyverse)
library(here)
library(ggthemes)
library(patchwork)
library(viridis)
library(tidymodels)
library(vip)
library(sf)
library(ggrepel)

# Create output directory
dir.create(here("viz"), showWarnings = FALSE)

# --------------------------
# Load Data and Models
# --------------------------
# Processed data and model results
cleaned_data <- read_csv(here("data", "processed", "cleaned_pump_data.csv"))
model_performance <- read_csv(here("models", "model_performance.csv"))
final_rf <- readRDS(here("models", "final_rf_model.rds"))

# If you have geospatial data for regions/districts, load as sf object
# tanzania_map <- st_read(here("data", "raw", "tanzania_regions.geojson"))

# --------------------------
# Functional Status by Region
# --------------------------
status_by_region <- cleaned_data %>%
  count(region, status_group) %>%
  group_by(region) %>%
  mutate(prop = n / sum(n))

p1 <- ggplot(status_by_region, aes(x = region, y = prop, fill = status_group)) +
  geom_col(position = "fill") +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Proportion of Pump Functional Status by Region",
       x = "Region", y = "Proportion", fill = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("viz", "status_by_region.png"), p1, width = 10, height = 6)

# --------------------------
# Feature Importance (Random Forest)
# --------------------------
p2 <- vip(final_rf, num_features = 15) +
  labs(title = "Top 15 Feature Importances (Random Forest Model)") +
  theme_minimal()

ggsave(here("viz", "feature_importance_rf.png"), p2, width = 8, height = 6)

# --------------------------
# Confusion Matrix Visualization
# --------------------------
# If you saved a confusion matrix plot, you can skip this section.
# Otherwise, generate from predictions (assuming you have test_data and predictions):

# test_data <- read_csv(here("data", "processed", "test_data.csv"))
# predictions <- predict(final_rf, test_data)
# conf_mat <- conf_mat(test_data, truth = status_group, estimate = predictions$.pred_class)
# p3 <- autoplot(conf_mat) + labs(title = "Confusion Matrix (Random Forest)")
# ggsave(here("viz", "confusion_matrix_rf.png"), p3, width = 6, height = 6)

# --------------------------
# ROC Curves (if multiclass, use one-vs-all)
# --------------------------
# If you have test_data and predicted probabilities:
# roc_data <- roc_curve(test_data, truth = status_group, .pred_functional, .pred_non_functional, .pred_needs_repair)
# p4 <- autoplot(roc_data) + labs(title = "ROC Curves (Random Forest)")
# ggsave(here("viz", "roc_curves_rf.png"), p4, width = 8, height = 6)

# --------------------------
# Geospatial Visualization (optional, if you have shapefiles)
# --------------------------
# If you have a Tanzania regions shapefile and longitude/latitude in your data:
# pumps_sf <- st_as_sf(cleaned_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
# p5 <- ggplot() +
#   geom_sf(data = tanzania_map, fill = "gray90", color = "white") +
#   geom_sf(data = pumps_sf, aes(color = status_group), alpha = 0.6, size = 1) +
#   scale_color_viridis_d(option = "D") +
#   labs(title = "Pump Locations by Functional Status") +
#   theme_map()
# ggsave(here("viz", "pump_locations_map.png"), p5, width = 10, height = 10)

# --------------------------
# Model Performance Summary Table
# --------------------------
# Save as CSV for reporting
write_csv(model_performance, here("viz", "model_performance_summary.csv"))

# --------------------------
# Combine Key Plots for Report
# --------------------------
# Combine status by region and feature importance for a summary figure
summary_plot <- p1 / p2 + plot_layout(heights = c(2, 1))
ggsave(here("viz", "summary_visualization.png"), summary_plot, width = 12, height = 10)

# --------------------------
# 10. Finalize
# --------------------------
cat("Visualization outputs saved to 'viz/' directory.\n")
