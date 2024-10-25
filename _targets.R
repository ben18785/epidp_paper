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
               "tidyr",
               "epidp"
               )
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
    tmp <- df_covid_colombia_cases %>%
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
        deaths=if_else(is.na(deaths), 0, deaths)
      ) %>%
      mutate(date=as.Date(date))

    date_min <- min(tmp$date)
    date_max <- max(tmp$date)
    dates_sequence <- seq(date_min, date_max, by = "day")
    cities <- unique(tmp$city)
    complete_combinations <- crossing(date = dates_sequence, city = cities)

    complete_combinations %>%
      left_join(tmp, by = c("date", "city")) %>%
      arrange(date, city) %>%
      mutate(
        deaths=if_else(is.na(deaths), 0, deaths),
        cases=if_else(is.na(cases), 0, cases)
      )
  }),
  tar_target(
    plot_cases_deaths_normalised,
    df_covid_colombia_cases_deaths %>%
      pivot_longer(c(cases, deaths)) %>%
      group_by(name) %>%
      mutate(
        value=value / max(value, na.rm = TRUE)
      ) %>%
      ggplot(aes(x=date, y=value)) +
      geom_line(aes(colour=name)) +
      scale_colour_brewer("Series", palette="Dark2") +
      facet_wrap(~city, scales="free")
  ),
  tar_target(
    plot_cases_deaths,
    df_covid_colombia_cases_deaths %>%
      pivot_longer(c(cases, deaths)) %>%
      ggplot(aes(x=date, y=value)) +
      geom_line() +
      facet_grid(vars(name), vars(city),
                 scales="free")
  ),


  # mobility data
  tar_target(
    mobility_all_years,
    join_mobility_data(
      filename1="data/raw/2020_CO_Region_Mobility_Report.csv",
      filename2="data/raw/2021_CO_Region_Mobility_Report.csv",
      filename3="data/raw/2022_CO_Region_Mobility_Report.csv"
    )
  ),
  tar_target(df_mobility, {
    mobility_all_years %>%
      mutate(date=as.Date(date)) %>%
      rename(city=name) %>%
      select(
        date,
        city,
        retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline
      ) %>%
      pivot_longer(-c(date, city)) %>%
      mutate(
        name=gsub("_percent_change_from_baseline", "", name)
      ) %>%
      mutate(name=case_when(
        name=="grocery_and_pharmacy"~"grocery and\npharmacy",
        name=="retail_and_recreation"~"retail and\nrecreation",
        name=="transit_stations"~"transit\nstations",
        TRUE~name
      )) %>%
      arrange(city, date) %>%
      pivot_wider(names_from = name, values_from = value,
                  id_cols=c(date, city))
  }),

  # overlay cases + deaths data with mobility measures
  tar_target(df_covid_colombia_cases_deaths_mobility, {
    df_covid_colombia_cases_deaths %>%
      left_join(df_mobility, by=c("city", "date"))
  }),
  tar_target(plot_cases_mobility, {
    df_covid_colombia_cases_deaths_mobility %>%
      select(-deaths) %>%
      pivot_longer(-c(date, city, cases)) %>%
      group_by(city, name) %>%
      mutate(value=max(cases)*(value - min(value)) / (max(value) - min(value))) %>%
      ggplot(aes(x=date, y=value)) +
      geom_line(aes(y=cases)) +
      geom_line(aes(y=value), colour="orange") +
      facet_grid(vars(name), vars(city), scales="free") +
      theme(
        strip.text.y = element_text(size=8)
      ) +
      ylab("Cases") +
      xlab("Date")
  }),
  tar_target(plot_deaths_mobility, {
    df_covid_colombia_cases_deaths_mobility %>%
      select(-cases) %>%
      pivot_longer(-c(date, city, deaths)) %>%
      group_by(city, name) %>%
      mutate(value=max(deaths)*(value - min(value)) / (max(value) - min(value))) %>%
      ggplot(aes(x=date, y=value)) +
      geom_line(aes(y=deaths)) +
      geom_line(aes(y=value), colour="orange") +
      facet_grid(vars(name), vars(city), scales="free") +
      theme(
        strip.text.y = element_text(size=8)
      ) +
      ylab("Deaths") +
      xlab("Date")
  }),

  # estimate Rt from cases and deaths for Bogota
  tar_target(df_covid_bogota_cases_deaths_mobility, {
    df_covid_colombia_cases_deaths_mobility %>%
      filter(city == "Bogota")
  }),
  tar_target(serial_interval_covid19, {
    # generate serial interval for COVID-19 based on reasonable mean, sd
    mean_si <- 6.5
    sd_si <- 4.03
    w <- epidp::generate_vector_serial(nrow(df_covid_bogota_cases_deaths_mobility), mean_si, sd_si)
    w
  }),

  ## no covariates
  tar_target(fit_bogota_no_covariates, {
    options(mc.cores=4)
    fit_epifilter(
      N = nrow(df_covid_bogota_cases_deaths_mobility),
      C = df_covid_bogota_cases_deaths_mobility$cases,
      w = serial_interval_covid19,
      is_sampling = TRUE,
      iter = 200,
      chains = 4
    )
  }),

  ## mobility
  tar_target(fit_bogota_mobility, {

    X <- tibble(
      cons = rep(1, nrow(df_covid_bogota_cases_deaths_mobility)),
      m = df_covid_bogota_cases_deaths_mobility$workplaces
    ) %>%
      mutate(
        m = scale(m)[, 1]
      ) %>%
      as.matrix()

    fit <- fit_epifilter_covariates(
      N = nrow(df_covid_bogota_cases_deaths_mobility),
      C = df_covid_bogota_cases_deaths_mobility$cases,
      w = serial_interval_covid19,
      X = X,
      is_sampling = FALSE,
      as_vector = FALSE
    )
  }),

  ## combine with transactions data
  tar_target(df_transactions_bogota, {
    dates <- read.csv("data/raw/tdates.csv", header = FALSE)$V1
    total_no_trans <- read.csv("data/raw/totNoTrans.csv", header = FALSE)$V1
    total_spend <- read.csv("data/raw/totSpend.csv", header = FALSE)$V1
    tibble(
      date=dates,
      transactions_count=total_no_trans,
      transactions_spend=total_spend
    )
  }),
  tar_target(df_covid_bogota_cases_deaths_mobility_transactions, {
    -1
  })


)
