#' `fxn_latestConditionsTableBarGraph.R` - Build latest conditions table
#' 
#' @param inData - Most recent 15-minute leaf wetness data from `fxn_latestConditionsData.R`
#' @return `latestConditionsTable` - Latest conditions table, reactable format


fxn_latestConditionsTableBarGraph <- function(value) {
  test <- #inData |>
  #   dplyr::filter(row_number == value) |>
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = mean_mV_range, y = as.factor(row_number)), color = "#e3e3e3", fill = "#FFFFFF", linewidth = 0.3) +
    ggplot2::geom_col(ggplot2::aes(x = mean_mV_adj, y = as.factor(row_number)), fill = "#c9c9c9", width = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = (273 - 200) / 200, xend = (273 - 200) / 200, y = 0.5, yend = 1.5), color = "#191919", linetype = "dotted", linewidth = 0.3) +
    ggplot2::geom_segment(ggplot2::aes(x = (284 - 200) / 200, xend = (284 - 200) / 200, y = 0.5, yend = 1.5), color = "#191919", linetype = "dotted", linewidth = 0.3) +
    ggplot2::annotate("text", x = (((273 - 200) / 2) / 200), y = 1.0 - 0.05, color = "#191919", label = "DRY", size = 2.5) +
    ggplot2::annotate("text", x = (((284 - ((284 - 273) / 2)) - 200) / 200), y = 1.0 - 0.05, color = "#191919", label = "T", size = 2.5) +
    ggplot2::annotate("text", x = (((400 - ((400 - 284) / 2)) - 200) / 200), y = 1.0 - 0.05, color = "#191919", label = "WET", size = 2.5) +
    ggplot2::xlim(0, 1) +
    ggplot2::theme_void() |>
  
  # test <- test |>
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