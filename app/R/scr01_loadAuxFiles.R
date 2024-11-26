azmetStations <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
) |>
  dplyr::filter(
    meta_station_name %in% c(
      "Roll", "Wellton ETo", "Yuma N.Gila", "Yuma South", "Yuma Valley"
    )
  ) |>
  dplyr::arrange(meta_station_name)
