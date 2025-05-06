# 06_render_reports.R
# Author: Jarret Angbazo
# Purpose: Render parameterized water pump reports for Tanzania

# --------------------------
# Setup
# --------------------------
library(rmarkdown)
library(here)
library(logger)
library(argparse)

# Configure logging
log_dir <- here("logs", "rendering")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
log_appender(appender_tee(file.path(log_dir, "render_log.txt")))

# --------------------------
# Reporting Functions
# --------------------------

render_report <- function(region = "All Regions", start_year = 2010) {
  tryCatch({
    output_file <- paste0(
      "water_pump_report_",
      tolower(gsub(" ", "_", region)), "_",
      format(Sys.Date(), "%Y%m%d"),
      ".html"
    )
    
    log_info("Rendering report for {region} (since {start_year})")
    
    rmarkdown::render(
      input = here("water_pump_report.Rmd"),
      output_file = here("reports", output_file),
      params = list(
        region = region,
        start_year = start_year
      ),
      envir = new.env()
    )
    
    log_info("Successfully generated: {output_file}")
    
  }, error = function(e) {
    log_error("Failed to render {region}: {e$message}")
  })
}

# --------------------------
# Command Line Interface
# --------------------------
parser <- ArgumentParser(description = "Water Pump Report Generator")
parser$add_argument("--type", choices = c("overall", "single", "batch"), 
                    default = "overall", help = "Report type")
parser$add_argument("--region", help = "Single region name")
parser$add_argument("--regions", nargs="+", help = "Multiple regions for batch processing")
parser$add_argument("--start_year", type="integer", default=2010, 
                    help = "Start year filter")

args <- parser$parse_args(commandArgs(trailingOnly = TRUE))

# --------------------------
# Reporting Logic
# --------------------------
log_info("\n=== REPORT GENERATION STARTED ===")
log_info("System time: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")

# Create reports directory
dir.create(here("reports"), showWarnings = FALSE, recursive = TRUE)

if(args$type == "overall") {
  # Full national report
  render_report("All Regions", args$start_year)
  
} else if(args$type == "single") {
  # Single region report
  if(is.null(args$region)) stop("Must specify --region for single report")
  render_report(args$region, args$start_year)
  
} else if(args$type == "batch") {
  # Batch processing
  regions <- if(!is.null(args$regions)) {
    args$regions
  } else {
    # Default regions if none specified
    c("Dodoma", "Arusha", "Dar es Salaam", "Mwanza", "Mbeya")
  }
  
  log_info("Batch processing {length(regions)} regions")
  purrr::walk(regions, ~ render_report(.x, args$start_year))
}

log_info("=== REPORT GENERATION COMPLETED ===")


# To generate national report (from command line)
# Rscript 06_render_reports.R -- type overall

# To generate region-specific report
# Rscript 06_render_reports.R -- type single --region "[region name]" --start_year [year]

# To batch process custom regions
# Rscript 06_render_reports.R -- type batch --regions [region name] --start_year [year]