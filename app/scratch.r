source("./app/R/fxn_lw15min.R", local = TRUE)
source("./app/R/fxn_latestConditionsData.R", local = TRUE)
source("./app/R/fxn_c_to_f.R", local = TRUE)
source("./app/R/fxn_mps_to_mph.R", local = TRUE)



lw15min <- 
  azmetr::az_lw15min(
    start_date_time = 
      lubridate::now(tzone = "America/Phoenix") - lubridate::hours(25)
  )

plotly::plot_ly(
  data = lw15min,
  x = ~datetime,
  y = ~relative_humidity_30cm_mean,
  type = "scatter",
  mode = "lines+markers"
)





lw15min <- 
  fxn_lw15min() |> 
  fxn_latestConditionsData()


# reactable ----------

lw15min |>
  reactable::reactable()

fxn_plotHorizontalBar <- function(value) {
  test <- lw15min |>
    dplyr::filter(row_number == value) |>
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = mean_mV_range, y = as.factor(row_number)), color = "#e3e3e3", fill = "#FFFFFF", linewidth = 0.3) +
    ggplot2::geom_col(ggplot2::aes(x = mean_mV_adj, y = as.factor(row_number)), fill = "#c9c9c9", width = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = (273 - 200) / 200, xend = (273 - 200) / 200, y = 0.5, yend = 1.5), color = "#191919", linetype = "dotted", linewidth = 0.3) +
    ggplot2::geom_segment(ggplot2::aes(x = (284 - 200) / 200, xend = (284 - 200) / 200, y = 0.5, yend = 1.5), color = "#191919", linetype = "dotted", linewidth = 0.3) +
    ggplot2::annotate("text", x = (((273 - 200) / 2) / 200), y = 1.0 - 0.05, color = "#191919", label = "DRY", size = 2.5) +
    ggplot2::annotate("text", x = (((284 - ((284 - 273) / 2)) - 200) / 200), y = 1.0 - 0.05, color = "#191919", label = "T", size = 2.5) +
    ggplot2::annotate("text", x = (((400 - ((400 - 284) / 2)) - 200) / 200), y = 1.0 - 0.05, color = "#191919", label = "WET", size = 2.5) +
    ggplot2::xlim(0, 1) +
    ggplot2::theme_void()
  
  test <- test |>
    plotly::ggplotly(height = 36, width = 256, tooltip = NULL) |>
    plotly::config(displaylogo = FALSE, displayModeBar = FALSE) |>
    plotly::layout(
      margin = list(
        l = 0,
        r = 0,
        b = 0,
        t = 0,
        pad = 0
      ),
      xaxis = list(
        fixedrange = TRUE
      ),
      yaxis = list(
        fixedrange = TRUE
      )
    ) |>
    plotly::style(hoverinfo = "none")
  
  return(test)
}

lw15min |>
  reactable::reactable(
    columns = list(
      mean_mV = reactable::colDef(
        name =
          htmltools::HTML(
            paste0(
              "mean_mV<sup>",
              htmltools::span(style = "font-weight: normal", "1"),
              "</sup><br>",
              htmltools::span(style = "font-weight: normal; font-size: 0.8rem", "(mV)")
            )
          ),
        format = reactable::colFormat(digits = 0),
        html = TRUE,
        na = "NA",
        rowHeader = TRUE,
        align = "left",
        minWidth = 160,
      ),
      
      row_number = reactable::colDef(
        name = "Row Number",
        cell = function(value) {
          fxn_plotHorizontalBar(value = value)
        },
        width = 256,
        style = "overflow:visible;"
      ),
      
      lw_sensor = reactable::colDef(
        name = "Sensor Status",
        html = TRUE,
        na = "NA",
        rowHeader = TRUE,
        align = "left",
        width = 120
      )
    )
  )


# plotly ----------

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


# gt ----------

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
