
fit_covariate_model <- function(
    covariate_name, df,
    serial_interval_covid19, iterations=200) {

  X <- tibble(
    cons = rep(1, nrow(df)),
    m = df[, covariate_name]
  ) %>%
    mutate(
      m = scale(m)[, 1]
    ) %>%
    as.matrix()

  options(mc.cores=4)
  fit <- fit_epifilter_covariates(
    N = nrow(df),
    C = df$cases,
    w = serial_interval_covid19,
    X = X,
    is_sampling = TRUE,
    iter=iterations
  )

  fit
}
