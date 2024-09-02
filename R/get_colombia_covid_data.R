
get_colombia_covid_large_data <- function(
    initial_date,
    final_date
    ) {

  url <- "https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD"

  df_covid19_col_full <- data.table::fread(url)
  colnames(df_covid19_col_full) <- tolower(stri_trans_general(colnames(df_covid19_col_full), "Latin-ASCII"))
  df_covid19_col_full <- df_covid19_col_full %>%
    rename(
      notification = "fecha de notificacion",
      onset = "fecha de inicio de sintomas",
      death = "fecha de muerte",
      city = "codigo divipola municipio"
    ) %>%
    filter(
      (as.Date(onset) >= initial_date) & (as.Date(onset) <= final_date),
      city %in% c(5001, 11001)
    ) %>%
    mutate(
      country = "Colombia",
      city = case_when(
        city == 5001 ~ "Medellin",
        city == 11001 ~ "Bogota",
        TRUE ~ "NA"
      )
    )
  df_covid19_col_full
}
