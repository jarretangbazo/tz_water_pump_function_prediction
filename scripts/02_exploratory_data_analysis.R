library(tidyverse)
library(skimr)
library(here)

cleaned_data <- read_csv(here("data", "processed", "cleaned_data.csv"))

# Generate EDA report
skim(cleaned_data) %>% 
  write_rds(here("eda", "skim_report.rds"))

# Create basic distribution plots
cleaned_data %>%
  ggplot(aes(pump_age)) +
  geom_histogram() +
  ggsave(here("eda", "age_distribution.png"))
