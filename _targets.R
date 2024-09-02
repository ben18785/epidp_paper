# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "ggplot2",
               "data.table",
               "stringi")
)

# all functions used
tar_source("R/get_colombia_covid_data.R")

# Replace the target list below with your own:
list(
  tar_target(
    df_covid_large,
    get_colombia_covid_large_data(
      initial_date=as.Date("2020-01-01"),
      final_date=as.Date("2021-12-31")
    )
  )
)
