
create_case_incidence <- function(df_covid_large) {

  df_covid_large %>%
    group_by(city, onset) %>%
   count()
}

create_death_incidence <- function(df_covid_large) {

  df_covid_large %>%
    group_by(city, death) %>%
    summarise(
      n=sum(is.na(death)==FALSE)
    )
}
