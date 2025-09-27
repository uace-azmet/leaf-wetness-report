# source("fxn_lw15min.R")
# source("fxn_latestConditionsData.R")
# source("fxn_c_to_f.R")
# source("fxn_mps_to_mph.R")

lw15min <- 
  fxn_lw15min() |> 
  fxn_latestConditionsData()

lw15min <- lw15min |>
  dplyr::mutate(dummyVar = seq(1:nrow(lw15min)))

plotly::plot_ly(
  data = lw15min,
  x = ~mean_mV,
  y = ~dummyVar,
  type = "bar",
  orientation = "h"
) |> 
  plotly::layout(
    #margin = list(l = 50, r = 10, b = 10, t = 10),
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE),
    autosize = TRUE
  ) |> 
  plotly::config(displayModeBar = FALSE) # Hide the Plotly toolbar
  


# gt

# lw15min |>
#   gt::gt(
#     groupname_col = c("meta_station_name", "datetime"),
#     row_group_as_column = TRUE
#   )
