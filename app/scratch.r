# source("fxn_lw15min.R")
# source("fxn_latestConditionsData.R")
# source("fxn_c_to_f.R")
# source("fxn_mps_to_mph.R")

lw15min <- 
  fxn_lw15min() |> 
  fxn_latestConditionsData()


# gt

lw15min |>
  gt::gt(
    groupname_col = c("meta_station_name", "datetime"),
    row_group_as_column = TRUE
  )
