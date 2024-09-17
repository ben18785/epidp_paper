
join_mobility_data <- function(filename1, filename2, filename3) {

  df_1 <- read.csv(filename1)
  df_2 <- read.csv(filename2)
  df_3 <- read.csv(filename3)

  df_full <- df_1 %>%
    bind_rows(
      df_2,
      df_3
    )

  df_bogota <- df_full %>%
    filter(
      sub_region_1%in%c("Bogota")
    ) %>%
    mutate(name="Bogota")
  df_medellin <- df_full %>%
    filter(
      sub_region_2%in%c("Medellin")
    ) %>%
    mutate(name="Medellin")

  df_bogota %>%
    bind_rows(
      df_medellin
    )
}
