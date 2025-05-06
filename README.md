# Pump It Up Data Mining the Water Table
This repository contains a data science project to predict the functional status of water pumps in Tanzania, based on open data from [DrivenData](https://www.drivendata.org/competitions/7/pump-it-up-data-mining-the-water-table/). The workflow is fully reproducible and organized for clarity and collaboration.

## Project Organization

```
├── README.md          <- Project documentation
├── data
│   ├── processed      <- Cleaned data files
│   └── raw            <- Original data files
├── eda                <- EDA outputs
├── viz                <- Final visualizations
├── models             <- Saved models
├── references         <- Data dictionaries, manuals, and all other explanatory materials.
├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
├── scripts            <- R scripts
```


## Workflow Overview

1. **Data Import and Cleaning**  
   - Script: `scripts/01_data_import_cleaning.R`  
   - Loads raw data, applies cleaning steps, and saves processed data.

2. **Exploratory Data Analysis (EDA)**  
   - Script: `scripts/02_exploratory_analysis.R`  
   - Generates summary statistics and initial data visualizations.

3. **Modeling and Analysis**  
   - Script: `scripts/03_analysis.R`  
   - Builds predictive models and saves model objects.

4. **Visualization**  
   - Script: `scripts/04_data_visualization.R`  
   - Creates and saves publication-quality plots.

5. **Reporting**  
   - R Markdown: `05_tz_water_pump_report.Rmd`  
   - Integrates results, visualizations, and interpretations into a final report.

**Analysis Logging**  
   - Script: `scripts/Analysis_Log.md`  
   - Documents key steps, decisions, and changes in data analysis.



## How to Run

1. **Clone the repository**
    ```
    git clone https://github.com/jarretangbazo/tz_water_pump_function_prediction.git
    cd tz_water_pump_function_prediction
    ```

2. **Open the RStudio project file**  
   Open `tz_water_pump_function_prediction.Rproj` in RStudio.

3. **Install required packages**  
   Use the following in your R console:
    ```
    install.packages(c("tidyverse", "here", "skimr", "tidymodels", "knitr"))
    ```

4. **Run scripts in order**  
   - Run each script in `scripts/` sequentially, or source them from the R console.
   - NOTE: `Analysis_Log.md` is updated after all analysis is run.

5. **Render the report**  
   - Knit `05_tz_water_pump_report.Rmd` to generate the final HTML report.

## Reproducibility

- All paths use the [`here`](https://CRAN.R-project.org/package=here) package for consistency.
- For package management, consider using [`renv`](https://rstudio.github.io/renv/).
- All scripts are modular and can be run independently.

## Data

- **Source:** [DrivenData Water Pump Dataset](https://www.drivendata.org/competitions/7/pump-it-up-data-mining-the-water-table/data/)
- **License:** See data provider’s terms.

## Authors

- Jarret Angbazo

## License

This project is licensed under the MIT License. See `LICENSE` for details.


--------

