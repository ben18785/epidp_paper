# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble",
               "dplyr",
               "ggplot2",
               "data.table",
               "stringi",
               "magrittr",
               "purrr",
               "tidyr")
)

# all functions used
tar_source("R/get_colombia_covid_data.R")
tar_source("R/join_mobility_data.R")
tar_source("R/create_epidemic_onset_curves.R")

# Replace the target list below with your own:
list(

  # covid case + deaths data for Bogota + Medellin
  tar_target(
    filename_covid_large,
    "data/raw/covid19_col_updated_individual.csv",
    format = "file"
  ),
  tar_target(df_covid_large,
             read.csv(filename_covid_large)),
  tar_target(df_covid_colombia_cases,
             create_case_incidence(df_covid_large)
             ),
  tar_target(df_covid_colombia_deaths,
             create_death_incidence(df_covid_large)
  ),
  tar_target(df_covid_colombia_cases_deaths, {
    df_covid_colombia_cases %>%
      rename(cases=n,
             date=onset) %>%
      left_join(
        df_covid_colombia_deaths %>%
          rename(
            deaths=n,
            date=death
            ),
        by = join_by(city, date)
      ) %>%
      mutate(
        n_deaths=if_else(is.na(deaths), 0, deaths)
      ) %>%
      mutate(date=as.Date(date))
  }),
  tar_target(
    plot_cases_deaths,
    df_covid_colombia_cases_deaths %>%
      pivot_longer(c(cases, deaths)) %>%
      ggplot(aes(x=date, y=value)) +
      geom_line(aes(colour=name)) +
      scale_colour_brewer(palette="Dark2") +
      facet_wrap(~city)
  ),


  # mobility data
  tar_target(
    mobility_all_years,
    join_mobility_data(
      filename1="data/raw/2020_CO_Region_Mobility_Report.csv",
      filename2="data/raw/2021_CO_Region_Mobility_Report.csv",
      filename3="data/raw/2022_CO_Region_Mobility_Report.csv"
    )
  )
)
