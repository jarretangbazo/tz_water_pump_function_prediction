library(tidyverse)
library(here)

model <- read_rds(here("models", "pump_status_model.rds"))

# Create publication-quality plot
model %>% 
  tidy() %>% 
  ggplot(aes(term, estimate)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  ggsave(here("viz", "feature_importance.png"), width = 8, height = 6)