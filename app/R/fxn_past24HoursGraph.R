#' `fxn_past24HoursGraph.R` generates Plotly line graphs of 30-cm conditions based on selected station
#' 
#' @param inData - data table from `fxn_lw15min()`
#' @param azmetStation - AZMet station selection by user
#' @return `past24HoursGraph` - Plotly line graphs of 30-cm conditions based on selected station

# https://plotly-r.com/ 
# https://plotly.com/r/
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_past24HoursGraph <- function(inData, azmetStation) {
  
  
  # Variables ----------
  
  inData <- inData %>%
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))

  dataOtherStations <- inData %>%
    dplyr::filter(meta_station_name != azmetStation) %>% 
    dplyr::group_by(meta_station_name)

  dataSelectedStation <- inData %>%
    dplyr::filter(meta_station_name == azmetStation)
  
  layoutFontFamily <- "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  
  
  # Graphs ----------
  
  # Leaf wetness -----
  
  # "lw1_mean_mV"
  # "lw2_mean_mV"
  
  dielectricConstant <-
    plotly::plot_ly(
      data = dataOtherStations,
      x = ~datetime,
      y = ~lw1_mean_mV,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(201, 201, 201, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(201, 201, 201, 1.0)",
        size = 2
      ),
      name = "other stations",
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>DC<sub>sensor 1</sub></b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", lw1_mean_mV,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    ) %>%
    
    plotly::add_trace(
      inherit = FALSE,
      data = dataSelectedStation,
      x = ~datetime,
      y = ~lw2_mean_mV,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(19, 19, 19, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(19, 19, 19, 1.0)",
        size = 2
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>DC<sub>sensor 2</sub></b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", lw2_mean_mV,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    ) %>% 
    
    plotly::add_trace(
      inherit = FALSE,
      data = dataSelectedStation,
      x = ~datetime,
      y = ~lw1_mean_mV,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(19, 19, 19, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(19, 19, 19, 1.0)",
        size = 2
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>DC<sub>sensor 1</sub></b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", lw1_mean_mV,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    ) %>% 
    
    plotly::add_trace(
      inherit = FALSE,
      data = dataSelectedStation,
      x = ~datetime,
      y = ~lw2_mean_mV,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(19, 19, 19, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(19, 19, 19, 1.0)",
        size = 2
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>DC<sub>sensor 2</sub></b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", lw2_mean_mV,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    )
  
  # Air temperature -----
  
  airTemperature <-
    plotly::plot_ly(
      data = dataOtherStations,
      x = ~datetime,
      y = ~temp_air_30cm_meanF,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(201, 201, 201, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(201, 201, 201, 1.0)",
        size = 2
      ),
      name = "other stations",
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>T<sub>air</sub></b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", temp_air_30cm_meanF,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    ) %>%
    
    plotly::add_trace(
      inherit = FALSE,
      data = dataSelectedStation,
      x = ~datetime,
      y = ~temp_air_30cm_meanF,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(19, 19, 19, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(19, 19, 19, 1.0)",
        size = 2
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>T<sub>air</sub></b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", temp_air_30cm_meanF,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    )
  
  # Dew point temperature -----
  
  dewPointTemperature <-
    plotly::plot_ly(
      data = dataOtherStations,
      x = ~datetime,
      y = ~dwpt_30cm_meanF,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(201, 201, 201, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(201, 201, 201, 1.0)",
        size = 2
      ),
      name = "other stations",
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>T<sub>dew point</sub></b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", dwpt_30cm_meanF,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    ) %>%
    
    plotly::add_trace(
      inherit = FALSE,
      data = dataSelectedStation,
      x = ~datetime,
      y = ~dwpt_30cm_meanF,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(19, 19, 19, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(19, 19, 19, 1.0)",
        size = 2
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>T<sub>dew point</sub></b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", dwpt_30cm_meanF,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    )
  
  # Relative humidity -----
  
  relativeHumidity <-
    plotly::plot_ly(
      data = dataOtherStations,
      x = ~datetime,
      y = ~relative_humidity_30cm_mean,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(201, 201, 201, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(201, 201, 201, 1.0)",
        size = 2
      ),
      name = "other stations",
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>RH</b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", relative_humidity_30cm_mean,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    ) %>%
    
    plotly::add_trace(
      inherit = FALSE,
      data = dataSelectedStation,
      x = ~datetime,
      y = ~relative_humidity_30cm_mean,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      line = list(
        color = "rgba(19, 19, 19, 1.0)",
        width = 1.0
      ),
      marker = list(
        color = "rgba(19, 19, 19, 1.0)",
        size = 2
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      text = ~paste0(
        "<br><b>RH</b><sup>", tags$span(style = "font-weight: normal", "1"), "</sup><b>:</b>  ", relative_humidity_30cm_mean,
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      )
    )
  
  
  # Stacked subplots ----------
  
  past24HoursGraph <-
    plotly::subplot(
      dielectricConstant,
      airTemperature,
      dewPointTemperature,
      relativeHumidity,
      #heights = c(1/3, 1/3, 1/3),
      margin = 0.05,
      nrows = 4,
      shareX = TRUE,
      shareY = FALSE,
      titleX = FALSE,
      titleY = FALSE,
      widths = 1
    ) %>%

    plotly::config(
      displaylogo = FALSE,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "lasso2d",
        "select"
      ),
      scrollZoom = FALSE,
      toImageButtonOptions = list(
        format = "png", # Either png, svg, jpeg, or webp
        filename = "AZMet-leaf-wetness",
        height = 400,
        width = 700,
        scale = 5
      )
    ) %>%

    plotly::layout(
  # #     annotations = list(
  # #       list( # No Heat Stress
  # #         align = "left",
  # #         font = list(
  # #           color = "#3b3b3b",
  # #           family = layoutFontFamily,
  # #           size = 12
  # #         ),
  # #         showarrow = FALSE,
  # #         text = "LEVEL 2 HEAT STRESS",
  # #         x = 0,
  # #         xanchor = "left",
  # #         xref = "paper",
  # #         xshift = 24,
  # #         y = max(dataCountsAllYears$None, na.rm = TRUE),
  # #         yanchor = "top",
  # #         yref = "y1"  
  # #       ),
  # #       list( # No Heat Stress
  # #         align = "left",
  # #         font = list(
  # #           color = "#3b3b3b",
  # #           family = layoutFontFamily,
  # #           size = 12
  # #         ),
  # #         showarrow = FALSE,
  # #         text = "LEVEL 1 HEAT STRESS",
  # #         x = 0,
  # #         xanchor = "left",
  # #         xref = "paper",
  # #         xshift = 24,
  # #         y = max(dataCountsAllYears$None, na.rm = TRUE),
  # #         yanchor = "top",
  # #         yref = "y2"  
  # #       ),
  # #       list( # No Heat Stress
  # #         align = "left",
  # #         font = list(
  # #           color = "#3b3b3b",
  # #           family = layoutFontFamily,
  # #           size = 12
  # #         ),
  # #         showarrow = FALSE,
  # #         text = "NO HEAT STRESS",
  # #         x = 0,
  # #         xanchor = "left",
  # #         xref = "paper",
  # #         xshift = 24,
  # #         y = max(dataCountsAllYears$None, na.rm = TRUE),
  # #         yanchor = "top",
  # #         yref = "y3"  
  # #       )
  # #     ),
      font = list(
        color = "#191919",
        family = layoutFontFamily,
        size = 13
      ),
      # hoverdistance = 1,
      # hoverlabel = list(
      #   bgcolor = "rgba(255, 255, 255, 0.75)",
      #   bordercolor = "transparent",
      #   font = list(
      #     color = "#191919",
      #     family = layoutFontFamily,
      #     size = 14
      #   )
      # ),
      # hovermode = "x unified",
      legend = list(
        orientation = "h",
        traceorder = "reversed",
        x = 0.00,
        xanchor = "left",
        xref = "container",
        y = 1.05,
        yanchor = "bottom",
        yref = "container"
      ),
      margin = list(
        l = 0,
        r = 50, # For space between plot and modebar
        b = 80, # For space between x-axis title and caption or figure help text
        t = 0,
        pad = 3
      ),
      modebar = list(
        bgcolor = "#FFFFFF",
        orientation = "v"
      )#,
      # spikedistance = 1#,
  # #     xaxis = list(
  # #       range = list(
  # #         min(dataCountsAllYears$pseudo_date) - 0.5, 
  # #         max(dataCountsAllYears$pseudo_date) + 0.5
  # #       ),
  # #       spikecolor = "#a6a6a6",
  # #       spikedash = "dot",
  # #       spikemode = "across+marker",
  # #       spikesnap = "hovered data",
  # #       spikethickness = "-2",
  # #       tickformat = format("%b %e"),
  # #       title = list(
  # #         font = list(size = 14),
  # #         standoff = 25,
  # #         text = "Month and Day"
  # #       ),
  # #       zeroline = FALSE
  # #     ),
  # #     yaxis = list(
  # #       fixedrange = TRUE,
  # #       range = list(
  # #         0 - 0.1, 
  # #         max(dataCountsAllYears$None, na.rm = TRUE) + 0.1
  # #       ),
  # #       zeroline = TRUE,
  # #       zerolinecolor = "#eee",
  # #       zerolinewidth = 0.5
  # #     ),
  # #     yaxis2 = list(
  # #       fixedrange = TRUE,
  # #       range = list(
  # #         0 - 0.1, 
  # #         max(dataCountsAllYears$None, na.rm = TRUE) + 0.1
  # #       ),
  # #       title = list(
  # #         font = list(size = 14),
  # #         standoff = 25,
  # #         text = "Number of Times in Level (Frequency)"
  # #       ),
  # #       zeroline = TRUE,
  # #       zerolinecolor = "#eee",
  # #       zerolinewidth = 0.5
  # #     ),
  # #     yaxis3 = list(
  # #       fixedrange = TRUE,
  # #       range = list(
  # #         0 - 0.1, 
  # #         max(dataCountsAllYears$None, na.rm = TRUE) + 0.1
  # #       ),
  # #       zeroline = TRUE,
  # #       zerolinecolor = "#eee",
  # #       zerolinewidth = 0.5
  # #     )
    )
  
  return(past24HoursGraph)
}
