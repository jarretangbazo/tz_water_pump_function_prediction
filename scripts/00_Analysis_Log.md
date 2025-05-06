# Analysis Log

This log documents the key steps, decisions, and changes made during the analysis of the Tanzania Water Pump Function Prediction project.

---

## Project Start

**Date:** 2025-05-05  
**Analyst:** Jarret Angbazo

---

### Step 1: Data Import and Cleaning
- **Date:** 2025-05-05
- **Script:** `scripts/01_data_import_cleaning.R`
- **Actions:**
  - Imported raw data from `data/raw/water_pumps.csv`.
  - Removed empty strings and standardized missing values.
  - Dropped redundant columns: `redundant_column`.
  - Saved cleaned data to `data/processed/cleaned_data.csv`.

---

### Step 2: Exploratory Data Analysis (EDA)
- **Date:** 2025-05-06
- **Script:** `scripts/02_exploratory_analysis.R`
- **Actions:**
  - Generated summary statistics using `skimr`.
  - Plotted distribution of `pump_age`.
  - Identified outliers in `amount_tsh`.

---

### Step 3: Modeling and Analysis
- **Date:** 2025-05-07
- **Script:** `scripts/03_modeling_analysis.R`
- **Actions:**
  - Split data into training (80%) and testing (20%) sets.
  - Built logistic regression model for `functional_status`.
  - Saved model object to `models/pump_status_model.rds`.

---

### Step 4: Visualization
- **Date:** 2025-05-08
- **Script:** `scripts/04_visualization.R`
- **Actions:**
  - Created feature importance plot.
  - Saved plot to `viz/feature_importance.png`.

---

### Step 5: Reporting
- **Date:** 2025-05-09
- **Script:** `05_report.Rmd`
- **Actions:**
  - Compiled results and visualizations into final report.
  - Rendered report as HTML.

---

## Notes & Decisions

- Decided to use `tidymodels` for modeling due to its tidy workflow.
- Chose to drop columns with >30% missing data.
- All scripts use `here::here()` for file paths.

---

## Next Steps

- Experiment with random forest models.
- Conduct feature engineering for geospatial variables.
