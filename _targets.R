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
               "epidp",
               "loo",
               "corrplot",
               "RColorBrewer",
               "forcats",
               "patchwork",
               "latex2exp"
               )
)

# all functions used
tar_source("R/get_colombia_covid_data.R")
tar_source("R/join_mobility_data.R")
tar_source("R/create_epidemic_onset_curves.R")
tar_source("R/fit_models.R")

f_extract_posterior_r_quantiles <- function(fit, dates) {

  r <- rstan::extract(fit, "R")[[1]]
  lower <- apply(r, 2, function(x) quantile(x, 0.025))
  upper <- apply(r, 2, function(x) quantile(x, 0.975))
  middle <- apply(r, 2, function(x) quantile(x, 0.5))

  data.frame(
    lower,
    middle,
    upper
  ) %>%
    mutate(date=dates)
}

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
                 scales="free") +
      xlab("Date") +
      theme_classic() +
      ylab("")
  ),
  tar_target(file_plot_cases_deaths, {
    filename <- "outputs/plot_cases_deaths.pdf"
    ggsave(filename, plot_cases_deaths, width = 8, height = 6);
    filename
  }),


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

    options(mc.cores=4)
    fit <- fit_epifilter_covariates(
      N = nrow(df_covid_bogota_cases_deaths_mobility),
      C = df_covid_bogota_cases_deaths_mobility$cases,
      w = serial_interval_covid19,
      X = X,
      is_sampling = TRUE,
      iter=200
    )
  }),
  tar_target(loo_compare_bogota, {

    log_likelihood_no_covs <- rstan::extract(fit_bogota_no_covariates, "log_likelihood")[[1]]
    log_likelihood_mobility <- rstan::extract(fit_bogota_mobility, "log_likelihood")[[1]]

    loo_compare(
      loo(log_likelihood_no_covs[, 2:ncol(log_likelihood_no_covs)]),
      loo(log_likelihood_mobility[, 2:ncol(log_likelihood_mobility)])
    )
  }),
  tar_target(plot_rt_comparison, {

    r <- rstan::extract(fit_bogota_no_covariates, "R")[[1]]
    lower <- apply(r, 2, function(x) quantile(x, 0.025))
    upper <- apply(r, 2, function(x) quantile(x, 0.975))
    middle <- apply(r, 2, function(x) quantile(x, 0.5))

    dates <- df_covid_bogota_cases_deaths_mobility$date

    df_no_covs <- data.frame(
      lower,
      middle,
      upper
    ) %>%
      mutate(covariates="none") %>%
      mutate(date=dates)

    r <- rstan::extract(fit_bogota_mobility, "R")[[1]]
    lower <- apply(r, 2, function(x) quantile(x, 0.025))
    upper <- apply(r, 2, function(x) quantile(x, 0.975))
    middle <- apply(r, 2, function(x) quantile(x, 0.5))

    df_mobility <- data.frame(
      lower,
      middle,
      upper
    ) %>%
      mutate(covariates="mobility") %>%
      mutate(date=dates)

    df_no_covs %>%
      bind_rows(df_mobility) %>%
      filter(date <= "2020-12-31") %>%
      ggplot(aes(x=date, y=middle)) +
      geom_ribbon(aes(ymin=lower, ymax=upper, fill=covariates),
                  alpha=0.8) +
      geom_line(aes(colour=covariates)) +
      scale_fill_brewer("Covariates", palette = "Dark2") +
      scale_colour_brewer("Covariates", palette = "Dark2") +
      ylab("R_t") +
      xlab("Date") +
      geom_hline(yintercept = 1, linetype=2) +
      scale_y_log10()
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
  tar_target(df_transactions_bogota_interpolated, {
    -1
  }),
  tar_target(df_covid_bogota_cases_deaths_mobility_transactions, {
    -1
  }),

  # include open DP data
  tar_target(df_covid_bogota_cases_deaths_mobility_opendp_transactions, {

    df <- read.csv("data/raw/dp_data.csv") %>%
      mutate(date=as.Date(endofweek)) %>%
      filter(
        date>=min(df_covid_bogota_cases_deaths_mobility$date),
        date<=max(df_covid_bogota_cases_deaths_mobility$date)
      )

    daily_dates <- seq(min(df$date), max(df$date), by = "day")
    spendamountusd_interpolated <- approx(df$date, df$spendamountusd, xout = daily_dates)$y
    countamountusd_interpolated <- approx(df$date, df$countamountusd, xout = daily_dates)$y

    df_daily <- tibble(
      date=daily_dates,
      spendamountusd=spendamountusd_interpolated,
      countamountusd=countamountusd_interpolated
    )

    # smooth mobility using weekly centered window
    window_size <- 7
    weights <- rep(1 / window_size, window_size) # equal weights for 7-day window
    df_smooth <- df_covid_bogota_cases_deaths_mobility %>%
      mutate_at(vars("retail and\nrecreation":"residential"),
                ~as.vector(stats::filter(., filter = weights, sides = 2)))

    df_comb <- df_smooth %>%
      left_join(df_daily)

    df_comb %>%
      filter(!is.na(spendamountusd)) %>%
      filter(!is.na(parks)) %>%
      rename(
        spend_average=spendamountusd,
        count_transactions=countamountusd
        )
  }),
  tar_target(plot_opendp_transactions_mobility, {

    df_covid_bogota_cases_deaths_mobility_opendp_transactions %>%
      select(-c(cases, deaths)) %>%
      pivot_longer(-c(date, city, spend_average, count_transactions)) %>%
      rename(
        mobility_category=name,
        mobility_value=value
        ) %>%
      pivot_longer(-c(date, city, mobility_category, mobility_value)) %>%
      group_by(city, mobility_category, name) %>%
      mutate(
        value=(value - min(value)) / (max(value) - min(value)),
        mobility_value= (mobility_value-min(mobility_value)) / (max(mobility_value) - min(mobility_value))
      ) %>%
      ungroup() %>%
      mutate(name=if_else(
        name=="count_transactions", "num. of transactions",
        if_else(
          name=="spend_average", "avg. transaction", name
        )
      )) %>%
      ggplot(aes(x=date, y=value)) +
      geom_line() +
      geom_line(aes(y=mobility_value), colour="orange") +
      facet_grid(vars(mobility_category), vars(name)) +
      theme(
        strip.text.y = element_text(size=8),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      theme_classic() +
      ylab("Normalised value") +
      xlab("Date")
  }),
  tar_target(file_plot_opendp_transactions_mobility, {
    filename <- "outputs/plot_opendp_transactions_mobility.pdf"
    ggsave(filename, plot_opendp_transactions_mobility, width = 8, height = 6);
    filename
  }),
  tar_target(plot_correlations, {

    X <- df_covid_bogota_cases_deaths_mobility_opendp_transactions %>%
      select(-c("date", "city", "deaths"))
    M <- cor(X)
    corrplot(M, type="upper", order="hclust",
             col=brewer.pal(n=8, name="RdYlBu"))
  }),
  tar_target(fit_no_covariates, {
    options(mc.cores=4)
    fit_epifilter(
      N = nrow(df_covid_bogota_cases_deaths_mobility_opendp_transactions),
      C = df_covid_bogota_cases_deaths_mobility_opendp_transactions$cases,
      w = serial_interval_covid19,
      is_sampling = TRUE,
      iter = 200,
      chains = 4
    )
  }),
  tar_target(fit_spend, fit_covariate_model(
    "spend_average",
    df_covid_bogota_cases_deaths_mobility_opendp_transactions,
    serial_interval_covid19
    )),
  tar_target(fit_transaction_count, fit_covariate_model(
    "count_transactions",
    df_covid_bogota_cases_deaths_mobility_opendp_transactions,
    serial_interval_covid19
  )),
  tar_target(fit_workplaces, fit_covariate_model(
    "workplaces",
    df_covid_bogota_cases_deaths_mobility_opendp_transactions,
    serial_interval_covid19
  )),
  tar_target(fit_residential, fit_covariate_model(
    "residential",
    df_covid_bogota_cases_deaths_mobility_opendp_transactions,
    serial_interval_covid19
  )),
  tar_target(fits, {
    list(
      transaction_count=fit_transaction_count,
      spend=fit_spend,
      no_covs=fit_no_covariates,
      workplaces=fit_workplaces,
      residential=fit_residential
         )
  }),
  tar_target(loo_compare_opendp, {

    log_likelihood_no_covs <- rstan::extract(fits$no_covs, "log_likelihood")[[1]]
    log_likelihood_transaction <- rstan::extract(fits$transaction_count, "log_likelihood")[[1]]
    log_likelihood_spend <- rstan::extract(fits$spend, "log_likelihood")[[1]]
    log_likelihood_workplaces <- rstan::extract(fits$workplaces, "log_likelihood")[[1]]
    log_likelihood_residential <- rstan::extract(fits$residential, "log_likelihood")[[1]]

    loo_compare(
      loo(log_likelihood_no_covs[, 2:ncol(log_likelihood_no_covs)]),
      loo(log_likelihood_transaction[, 2:ncol(log_likelihood_transaction)]),
      loo(log_likelihood_spend[, 2:ncol(log_likelihood_spend)]),
      loo(log_likelihood_workplaces[, 2:ncol(log_likelihood_workplaces)]),
      loo(log_likelihood_residential[, 2:ncol(log_likelihood_residential)])
    )
  }),
  tar_target(plot_rt_comparison_opendp, {

    dates <- df_covid_bogota_cases_deaths_mobility_opendp_transactions$date
    df_no_covs <- f_extract_posterior_r_quantiles(fits$no_covs, dates) %>%
      mutate(covariates="no covariates")
    df_transaction <- f_extract_posterior_r_quantiles(fits$transaction_count, dates) %>%
      mutate(covariates="num. of transactions")
    df_spend <- f_extract_posterior_r_quantiles(fits$spend, dates) %>%
      mutate(covariates="avg. spend")

    df_no_covs %>%
      bind_rows(
        df_transaction,
        df_spend
        ) %>%
      mutate(covariates=as.factor(covariates)) %>%
      mutate(covariates=fct_relevel(covariates, "no covariates", "avg. spend", "num. of transactions")) %>%
      ggplot(aes(x=date, y=middle)) +
      geom_ribbon(aes(ymin=lower, ymax=upper, fill=covariates),
                  alpha=0.8) +
      geom_line(aes(colour=covariates)) +
      scale_fill_brewer("Covariates", palette = "Dark2") +
      scale_colour_brewer("Covariates", palette = "Dark2") +
      ylab(TeX("$R_t$")) +
      xlab("Date") +
      geom_hline(yintercept = 1, linetype=2) +
      theme_classic() +
      theme(
        legend.position = c(0.5, 0.6)
      ) +
      scale_y_log10()
  }),
  tar_target(plot_rt_comparison_opendp_mobility, {

    dates <- df_covid_bogota_cases_deaths_mobility_opendp_transactions$date
    df_no_covs <- f_extract_posterior_r_quantiles(fits$no_covs, dates) %>%
      mutate(covariates="no covariates")
    df_transaction <- f_extract_posterior_r_quantiles(fits$transaction_count, dates) %>%
      mutate(covariates="workplace mobility")
    df_spend <- f_extract_posterior_r_quantiles(fits$spend, dates) %>%
      mutate(covariates="residential mobility")

    df_no_covs %>%
      bind_rows(
        df_transaction,
        df_spend
      ) %>%
      mutate(covariates=as.factor(covariates)) %>%
      mutate(covariates=fct_relevel(covariates, "no covariates", "workplace mobility", "residential mobility")) %>%
      ggplot(aes(x=date, y=middle)) +
      geom_ribbon(aes(ymin=lower, ymax=upper, fill=covariates),
                  alpha=0.8) +
      geom_line(aes(colour=covariates)) +
      scale_fill_brewer("Covariates", palette = "Dark2") +
      scale_colour_brewer("Covariates", palette = "Dark2") +
      ylab(TeX("$R_t$")) +
      xlab("Date") +
      theme_classic() +
      theme(
        legend.position = c(0.5, 0.6)
      ) +
      geom_hline(yintercept = 1, linetype=2) +
      scale_y_log10()
  }),
  tar_target(plot_rt_comparison_opendp_both, {
    plot_rt_comparison_opendp + plot_rt_comparison_opendp_mobility
  }),
  tar_target(file_plot_rt_comparison_opendp_both, {
    filename <- "outputs/plot_rt_comparison_opendp_both.pdf"
    ggsave(filename, plot_rt_comparison_opendp_both, width = 8, height = 6);
    filename
  })

)
