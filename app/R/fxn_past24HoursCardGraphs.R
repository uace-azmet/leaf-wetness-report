#' `fxn_past24HoursCardGraphs.R` Generate time series graphs for cards on Past 24 Hours tab based on user input
#' 
#' @param azmetStation - user-specified AZMet station
#' @param inData - AZMet 15-minute leaf wetness data from `fxn_lw15min.R`
#' @return `past24HoursCardGraphs` - `list` of time series graphs for cards on Past 24 Hours tab based on user input

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_past24HoursCardGraphs <- function(azmetStation, inData) {
  
  
  # Variables ----------
  
  inData <- inData %>% 
    # dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))
  
  dataOtherStations <- inData %>%
    dplyr::filter(meta_station_name != azmetStation) %>% 
    dplyr::group_by(meta_station_name)
  
  dataSelectedStation <- inData %>%
    dplyr::filter(meta_station_name == azmetStation)
  
  hoverlabelFontColor = "#FFFFFF"
  hoverlabelFontSize = 14
  layoutFontColor = "#707070"
  layoutFontFamily = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  layoutFontSize = 11
  layoutMargin = 6
  layoutPadding = 0
  traceLineColor = "rgba(201, 201, 201, 1.0)"#"#808080"
  traceLineWidth = 1
  traceMarkerColor = "rgba(201, 201, 201, 1.0)"#"#808080"
  traceMarkerSize = 3
  
  
  # Graphs ----------
  
  past24HoursCardGraphs <- list(
    
    
    # `relative_humidity_30cm_mean` -----
    
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~relative_humidity_30cm_mean, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
        "<br><b>RH:</b>  ", format(relative_humidity_30cm_mean, nsmall = 0), " %"
      )
    ) %>%
      plotly::add_trace(
        data = dataSelectedStation, 
        x = ~datetime, 
        y = ~relative_humidity_30cm_mean, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "#737373", width = traceLineWidth),
        marker = list(color = "#737373", size = traceMarkerSize),#marker = list(color = traceMarkerColor, size = traceMarkerSize),
        hoverinfo = "text",
        text = ~paste0(
          "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
          "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
          "<br><b>RH:</b>  ", format(relative_humidity_30cm_mean, nsmall = 0), " %"
        )
      ) %>% 
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "%", 
          zeroline = FALSE
        )
      ),
    
    
    # `temp_air_30cm_meanF` -----
    
    plotly::plot_ly(
      data = inData, 
      x = ~datetime, 
      y = ~temp_air_30cm_meanF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>air</sub>:</b>  ", format(temp_air_30cm_meanF, nsmall = 1), " °F"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".1f",
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `dwpt_30cm_meanF` -----
    
    plotly::plot_ly(
      data = inData, 
      x = ~datetime, 
      y = ~dwpt_30cm_meanF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>dew point</sub>:</b>  ", format(dwpt_30cm_meanF, nsmall = 1), " °F"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".1f",
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `lw*_mean_mV` -----
    
    plotly::plot_ly(
      data = inData, 
      x = ~datetime, 
      y = ~lw1_mean_mV, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
        "<br><b>Sensor:</b>  Sensor 1",
        "<br><b>DC:</b>  ", format(lw1_mean_mV, nsmall = 0), " mV"
      )
    ) %>%
      plotly::add_trace(
        data = inData, 
        x = ~datetime, 
        y = ~lw2_mean_mV, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = traceLineColor, width = traceLineWidth),
        marker = list(color = traceMarkerColor, size = traceMarkerSize),
        hoverinfo = "text",
        text = ~paste0(
          "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
          "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
          "<br><b>Sensor:</b>  Sensor 2",
          "<br><b>DC:</b> ", format(lw2_mean_mV, nsmall = 0), " mV"
        )
      ) %>% 
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "mV", 
          zeroline = FALSE
        )
      )
  ) # `past24HoursCardGraphs`
  
  return(past24HoursCardGraphs)
}
