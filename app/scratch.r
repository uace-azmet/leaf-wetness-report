source("./app/R/fxn_lw15min.R", local = TRUE)
source("./app/R/fxn_latestConditionsData.R", local = TRUE)
source("./app/R/fxn_c_to_f.R", local = TRUE)
source("./app/R/fxn_mps_to_mph.R", local = TRUE)

lw15min <- 
  fxn_lw15min() |> 
  fxn_latestConditionsData()

# lw15min <- lw15min |>
#   dplyr::mutate(dummyVar = seq(1:nrow(lw15min)))
# 
# plotly::plot_ly(
#   data = lw15min,
#   x = ~mean_mV,
#   y = ~dummyVar,
#   type = "bar",
#   orientation = "h"
# ) |> 
#   plotly::layout(
#     #margin = list(l = 50, r = 10, b = 10, t = 10),
#     xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
#     yaxis = list(title = "", showgrid = FALSE),
#     autosize = TRUE
#   ) |> 
#   plotly::config(displayModeBar = FALSE) # Hide the Plotly toolbar
  


# gt

lw15min |>
  gt::gt(
    groupname_col = c("meta_station_name", "datetime"),
    row_group_as_column = TRUE
  )

lw15min |>
  gt::gt(
    groupname_col = "meta_station_name",
    row_group_as_column = TRUE
  )

lw15min |> # https://stackoverflow.com/questions/78275267/add-a-second-groupname-col-in-gt-table-without-concatenating-the-column-valu
  dplyr::mutate(meta_station_name = factor(meta_station_name, levels = unique(meta_station_name))) |>
  dplyr::arrange(meta_station_name, datetime) |>
  dplyr::mutate(
    datetime = ifelse(dplyr::row_number() == 1, as.character(datetime), ""), .by = c(meta_station_name, datetime)
  ) |>
  # dplyr::mutate(
  #   meta_station_name = ifelse(dplyr::row_number() == 1, meta_station_name, ""), .by = meta_station_name
  # ) |>
  # gt::gt()
  gt::gt(
    groupname_col = "meta_station_name",
    row_group_as_column = TRUE
  ) |>
  gtExtras::gt_plt_bullet(column = mean_mV, temp_air_30cm_meanF)
