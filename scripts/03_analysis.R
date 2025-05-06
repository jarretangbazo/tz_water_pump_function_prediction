library(tidymodels)
library(here)

cleaned_data <- read_csv(here("data", "processed", "cleaned_data.csv"))

# Split data
set.seed(1234)
split <- initial_split(cleaned_data, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

# Model pipeline
model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(functional_status ~ ., data = train_data)

# Save model artifacts
write_rds(model, here("models", "pump_status_model.rds"))