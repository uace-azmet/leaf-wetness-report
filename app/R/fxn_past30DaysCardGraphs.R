#' `fxn_past30DaysCardGraphs.R` Generate time series graphs for cards on Past 30 Days tab based on user input
#' 
#' @param azmetStation - user-specified AZMet station
#' @param inData - AZMet daily leaf wetness data from `fxn_lwdaily.R`
#' @return `past30DaysCardGraphs` - `list` of time series graphs for cards on Past 30 Days tab based on user input

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_past30DaysCardGraphs <- function(azmetStation, inData) {
  
  
  # Variables ----------
  
  
  inData <- inData %>% 
    dplyr::mutate(date = lubridate::ymd(date))
  
  dataOtherStations <- inData %>%
    dplyr::filter(meta_station_name != azmetStation) %>% 
    dplyr::group_by(meta_station_name)
  
  dataSelectedStation <- inData %>%
    dplyr::filter(meta_station_name == azmetStation)
  
  hoverlabelFontSize = 14
  layoutFontColor = "#707070"
  layoutFontFamily = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  layoutFontSize = 11
  layoutMargin = 6
  layoutPadding = 0
  traceLineColorOtherStations = "#bfbfbf"#"rgba(201, 201, 201, 1.0)"
  traceLineColorSelectedStations = "#191919"
  traceLineWidth = 1
  traceMarkerColorOtherStations = "#bfbfbf"#"rgba(201, 201, 201, 1.0)"
  traceMarkerColorSelectedStation = "#191919"
  traceMarkerSize = 3
  
  
  # Graphs - Individual Subplots ----------
  
  
  # `relative_humidity_30cm_*` -----
  
  relative_humidity_30cm_max <- 
    
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~relative_humidity_30cm_max, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>RH<sub>max</sub>:</b>  ", format(relative_humidity_30cm_max, nsmall = 0), " %"
      ),
      name = "other stations",
      showlegend = TRUE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~relative_humidity_30cm_max, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>RH<sub>max</sub>:</b>  ", format(relative_humidity_30cm_max, nsmall = 0), " %"
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
        title = "", 
        zeroline = FALSE
      )
    )
  
  
  relative_humidity_30cm_mean <- 
    
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~relative_humidity_30cm_mean, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>RH<sub>avg</sub>:</b>  ", format(relative_humidity_30cm_mean, nsmall = 0), " %"
      ),
      name = "other stations",
      showlegend = FALSE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~relative_humidity_30cm_mean, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>RH<sub>avg</sub>:</b>  ", format(relative_humidity_30cm_mean, nsmall = 0), " %"
      ),
      name = ~meta_station_name,
      showlegend = FALSE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
        title = "%", 
        zeroline = FALSE
      )
    )
  
  
  relative_humidity_30cm_min <- 
    
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~relative_humidity_30cm_min, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>RH<sub>min</sub>:</b>  ", format(relative_humidity_30cm_min, nsmall = 0), " %"
      ),
      name = "other stations",
      showlegend = FALSE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~relative_humidity_30cm_min, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>RH<sub>min</sub>:</b>  ", format(relative_humidity_30cm_min, nsmall = 0), " %"
      ),
      name = ~meta_station_name,
      showlegend = FALSE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
        title = "", 
        zeroline = FALSE
      )
    )
 
  
  # `temp_air_30cm_*F` -----
  
  temp_air_30cm_maxF <- 
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~temp_air_30cm_maxF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>air max</sub>:</b>  ", format(temp_air_30cm_maxF, nsmall = 1), " °F"
      ),
      name = "other stations",
      showlegend = TRUE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~temp_air_30cm_maxF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>air max</sub>:</b>  ", format(temp_air_30cm_maxF, nsmall = 1), " °F"
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
        title = "", 
        zeroline = FALSE
      )
    )
  
  temp_air_30cm_meanF <- 
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~temp_air_30cm_meanF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>air avg</sub>:</b>  ", format(temp_air_30cm_meanF, nsmall = 1), " °F"
      ),
      name = "other stations",
      showlegend = FALSE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~temp_air_30cm_meanF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>air avg</sub>:</b>  ", format(temp_air_30cm_meanF, nsmall = 1), " °F"
      ),
      name = ~meta_station_name,
      showlegend = FALSE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
        title = "°F", 
        zeroline = FALSE
      )
    )
  
  temp_air_30cm_minF <- 
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~temp_air_30cm_minF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>air min</sub>:</b>  ", format(temp_air_30cm_minF, nsmall = 1), " °F"
      ),
      name = "other stations",
      showlegend = FALSE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~temp_air_30cm_minF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>air min</sub>:</b>  ", format(temp_air_30cm_minF, nsmall = 1), " °F"
      ),
      name = ~meta_station_name,
      showlegend = FALSE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
        title = "", 
        zeroline = FALSE
      )
    )
  
  
  # `dwpt_30cm_*F` -----
  
  dwpt_30cm_maxF <- 
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~dwpt_30cm_maxF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>dew point max</sub>:</b>  ", format(dwpt_30cm_maxF, nsmall = 1), " °F"
      ),
      name = "other stations",
      showlegend = TRUE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~dwpt_30cm_maxF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>dew point max</sub>:</b>  ", format(dwpt_30cm_maxF, nsmall = 1), " °F"
      ),
      name = ~meta_station_name,
      showlegend = TRUE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
        title = "", 
        zeroline = FALSE
      )
    )
  
  dwpt_30cm_meanF <- 
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~dwpt_30cm_meanF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>dew point avg</sub>:</b>  ", format(dwpt_30cm_meanF, nsmall = 1), " °F"
      ),
      name = "other stations",
      showlegend = FALSE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~dwpt_30cm_meanF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>dew point avg</sub>:</b>  ", format(dwpt_30cm_meanF, nsmall = 1), " °F"
      ),
      name = ~meta_station_name,
      showlegend = FALSE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
        title = "°F", 
        zeroline = FALSE
      )
    )
  
  dwpt_30cm_minF <- 
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~dwpt_30cm_minF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>dew point min</sub>:</b>  ", format(dwpt_30cm_minF, nsmall = 1), " °F"
      ),
      name = "other stations",
      showlegend = FALSE,
      legendgroup = "dataOtherStations"
    ) %>%
    
    plotly::add_trace(
      data = dataSelectedStation, 
      x = ~datetime, 
      y = ~dwpt_30cm_minF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceMarkerColorSelectedStation, width = traceLineWidth),
      marker = list(color = traceMarkerColorSelectedStation, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>T<sub>dew point min</sub>:</b>  ", format(dwpt_30cm_minF, nsmall = 1), " °F"
      ),
      name = ~meta_station_name,
      showlegend = FALSE,
      legendgroup = "dataSelectedStation"
    ) %>% 
    
    plotly::layout(
      xaxis = list(
        # fixedrange = TRUE,
        range = list(
          ~(min(datetime) - 86400), # unix time
          ~(max(datetime) + 86400)
        )#,
        # showgrid = TRUE,
        # showticklabels = TRUE,
        # title = FALSE,
        # zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
        title = "", 
        zeroline = FALSE
      )
    )
  
  
  # Graphs - List ----------
  
  
  past30DaysCardGraphs <- list(
    
    
    # `relative_humidity_30cm_*` -----
    
    plotly::subplot(
      relative_humidity_30cm_max, 
      relative_humidity_30cm_mean, 
      relative_humidity_30cm_min,
      # heights = c(1/3, 1/3, 1/3),
      margin = 0.02,
      nrows = 3, 
      shareX = TRUE,
      shareY = TRUE,
      titleX = FALSE,
      titleY = TRUE,
      widths = 1
    ) %>% 
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      
      plotly::layout(
        annotations = list(
          list( 
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "MAXIMUM",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$relative_humidity_30cm_max, na.rm = TRUE),
            yanchor = "center",
            yref = "y1"  
          ),
          list(
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "AVERAGE",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$relative_humidity_30cm_mean, na.rm = TRUE),
            yanchor = "center",
            yref = "y2"  
          ),
          list( # No Heat Stress
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "MINIMUM",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$relative_humidity_30cm_min, na.rm = TRUE),
            yanchor = "center",
            yref = "y3"  
          )
        ),
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          font = list(
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
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
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          # range = list(
          #   ~(min(datetime) - 86400), # unix time
          #   ~(max(datetime) + 86400)
          # ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        )#,
        # yaxis = list(
        #   fixedrange = TRUE,
        #   rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
        #   title = "%", 
        #   zeroline = FALSE
        # )
      ),
    
    
    # `temp_air_30cm_*F` -----
    
    plotly::subplot(
      temp_air_30cm_maxF, 
      temp_air_30cm_meanF, 
      temp_air_30cm_minF,
      # heights = c(1/3, 1/3, 1/3),
      margin = 0.02,
      nrows = 3, 
      shareX = TRUE,
      shareY = TRUE,
      titleX = FALSE,
      titleY = TRUE,
      widths = 1
    ) %>% 
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      
      plotly::layout(
        annotations = list(
          list( 
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "MAXIMUM",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$temp_air_30cm_maxF, na.rm = TRUE),
            yanchor = "center",
            yref = "y1"  
          ),
          list(
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "AVERAGE",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$temp_air_30cm_meanF, na.rm = TRUE),
            yanchor = "center",
            yref = "y2"  
          ),
          list( # No Heat Stress
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "MINIMUM",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$temp_air_30cm_minF, na.rm = TRUE),
            yanchor = "center",
            yref = "y3"  
          )
        ),
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          font = list(
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
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
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          # range = list(
          #   ~(min(datetime) - 86400), # unix time
          #   ~(max(datetime) + 86400)
          # ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        )#,
        # yaxis = list(
        #   fixedrange = TRUE,
        #   rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
        #   title = "%", 
        #   zeroline = FALSE
        # )
      ),
    
    
    # `dwpt_30cm_*F` -----
    
    plotly::subplot(
      dwpt_30cm_maxF, 
      dwpt_30cm_meanF, 
      dwpt_30cm_minF,
      # heights = c(1/3, 1/3, 1/3),
      margin = 0.02,
      nrows = 3, 
      shareX = TRUE,
      shareY = TRUE,
      titleX = FALSE,
      titleY = TRUE,
      widths = 1
    ) %>% 
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      
      plotly::layout(
        annotations = list(
          list( 
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "MAXIMUM",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$dwpt_30cm_maxF, na.rm = TRUE),
            yanchor = "center",
            yref = "y1"  
          ),
          list(
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "AVERAGE",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$dwpt_30cm_meanF, na.rm = TRUE),
            yanchor = "center",
            yref = "y2"  
          ),
          list( # No Heat Stress
            align = "left",
            font = list(
              color = layoutFontColor,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "MINIMUM",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 10,
            y = max(inData$dwpt_30cm_minF, na.rm = TRUE),
            yanchor = "center",
            yref = "y3"  
          )
        ),
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          font = list(
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
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
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          # range = list(
          #   ~(min(datetime) - 86400), # unix time
          #   ~(max(datetime) + 86400)
          # ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        )#,
        # yaxis = list(
        #   fixedrange = TRUE,
        #   rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
        #   title = "%", 
        #   zeroline = FALSE
        # )
      )#,
    
    
    # `lw*_mean_mV` -----
    
    # plotly::plot_ly(
    #   data = dataOtherStations, 
    #   x = ~datetime, 
    #   y = ~lw1_mean_mV, 
    #   type = "scatter", 
    #   mode = "lines+markers",
    #   line = list(color = traceLineColorOtherStations, width = traceLineWidth),
    #   marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
    #   hoverinfo = "text",
    #   text = ~paste0(
    #     "<br><b>AZMet Station:</b>  ", meta_station_name,
    #     "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
    #     "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
    #     "<br><b>Sensor:</b>  Sensor 1",
    #     "<br><b>Condition:</b>  ", lw1_condition,
    #     "<br><b>DC:</b>  ", format(lw1_mean_mV, nsmall = 0), " mV"
    #   ),
    #   name = "other stations",
    #   showlegend = TRUE,
    #   legendgroup = "dataOtherStations"
    # ) %>%
    #   
    #   plotly::add_trace(
    #     data = dataOtherStations, 
    #     x = ~datetime, 
    #     y = ~lw2_mean_mV, 
    #     type = "scatter", 
    #     mode = "lines+markers",
    #     line = list(color = traceLineColorOtherStations, width = traceLineWidth),
    #     marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
    #     hoverinfo = "text",
    #     text = ~paste0(
    #       "<br><b>AZMet Station:</b>  ", meta_station_name,
    #       "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
    #       "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
    #       "<br><b>Sensor:</b>  Sensor 2",
    #       "<br><b>Condition:</b>  ", lw2_condition,
    #       "<br><b>DC:</b> ", format(lw2_mean_mV, nsmall = 0), " mV"
    #     ),
    #     name = "other stations",
    #     showlegend = FALSE,
    #     legendgroup = "dataOtherStations"
    #   ) %>% 
    #   
    #   plotly::add_trace(
    #     data = dataSelectedStation, 
    #     x = ~datetime, 
    #     y = ~lw1_mean_mV, 
    #     type = "scatter", 
    #     mode = "lines+markers",
    #     line = list(color = traceLineColorSelectedStations, width = traceLineWidth),
    #     marker = list(color = traceLineColorSelectedStations, size = traceMarkerSize),
    #     hoverinfo = "text",
    #     text = ~paste0(
    #       "<br><b>AZMet Station:</b>  ", meta_station_name,
    #       "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
    #       "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
    #       "<br><b>Sensor:</b>  Sensor 1",
    #       "<br><b>Condition:</b>  ", lw1_condition,
    #       "<br><b>DC:</b> ", format(lw1_mean_mV, nsmall = 0), " mV"
    #     ),
    #     name = ~meta_station_name,
    #     showlegend = TRUE,
    #     legendgroup = "dataSelectedStation"
    #   ) %>% 
    #   
    #   plotly::add_trace(
    #     data = dataSelectedStation, 
    #     x = ~datetime, 
    #     y = ~lw2_mean_mV, 
    #     type = "scatter", 
    #     mode = "lines+markers",
    #     line = list(color = traceLineColorSelectedStations, width = traceLineWidth),
    #     marker = list(color = traceLineColorSelectedStations, size = traceMarkerSize),
    #     hoverinfo = "text",
    #     text = ~paste0(
    #       "<br><b>AZMet Station:</b>  ", meta_station_name,
    #       "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
    #       "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
    #       "<br><b>Sensor:</b>  Sensor 2",
    #       "<br><b>Condition:</b>  ", lw2_condition,
    #       "<br><b>DC:</b> ", format(lw2_mean_mV, nsmall = 0), " mV"
    #     ),
    #     name = ~meta_station_name,
    #     showlegend = FALSE,
    #     legendgroup = "dataSelectedStation"
    #   ) %>% 
    #   
    #   plotly::config(
    #     displaylogo = FALSE,
    #     displayModeBar = FALSE
    #   ) %>% 
    #   
    #   plotly::layout(
    #     annotations = list(
    #       list( # Wet
    #         align = "left",
    #         font = list(
    #           color = "#378dbd",
    #           family = layoutFontFamily,
    #           size = layoutFontSize
    #         ),
    #         showarrow = FALSE,
    #         text = "Wet",
    #         x = 0,
    #         xanchor = "left",
    #         xref = "paper",
    #         xshift = 5,
    #         y = thresholdMeanMVWet,
    #         yanchor = "bottom",
    #         yref = "y"
    #       ),
    #       list( # Transition
    #         align = "left",
    #         font = list(
    #           color = "#81d3eb",
    #           family = layoutFontFamily,
    #           size = layoutFontSize
    #         ),
    #         showarrow = FALSE,
    #         text = "Transition",
    #         x = 0,
    #         xanchor = "left",
    #         xref = "paper",
    #         xshift = 5,
    #         # y = mean(c(thresholdMeanMVDry, thresholdMeanMVWet)),
    #         y = thresholdMeanMVDry,
    #         yanchor = "bottom",
    #         yref = "y"
    #       ),
    #       list( # Dry
    #         align = "left",
    #         font = list(
    #           color = "#989898",
    #           family = layoutFontFamily,
    #           size = layoutFontSize
    #         ),
    #         showarrow = FALSE,
    #         text = "Dry",
    #         x = 0,
    #         xanchor = "left",
    #         xref = "paper",
    #         xshift = 5,
    #         # y = thresholdMeanMVDry,
    #         # yanchor = "top",
    #         y = thresholdMeanMVDry - (thresholdMeanMVWet - thresholdMeanMVDry),
    #         yanchor = "bottom",
    #         yref = "y"
    #       )
    #     ),
    #     font = list(
    #       color = layoutFontColor,
    #       family = layoutFontFamily,
    #       size = layoutFontSize
    #     ),
    #     hoverlabel = list(
    #       # bordercolor = "rgba(0, 0, 0, 0)",
    #       font = list(
    #         # color = hoverlabelFontColor,
    #         family = layoutFontFamily,
    #         size = hoverlabelFontSize
    #       )
    #     ),
    #     legend = list(
    #       orientation = "h",
    #       traceorder = "reversed",
    #       x = 0.00,
    #       xanchor = "left",
    #       xref = "container",
    #       y = 1.05,
    #       yanchor = "bottom",
    #       yref = "container"
    #     ),
    #     margin = list(
    #       l = layoutMargin,
    #       r = layoutMargin,
    #       b = layoutMargin,
    #       t = layoutMargin,
    #       pad = layoutPadding
    #     ),
    #     shapes = 
    #       list(
    #         list( # Wet minimum threshold
    #           layer = "below",
    #           line = list(color = "#378dbd"),
    #           showlegend = FALSE,
    #           type = "line",
    #           x0 = 0,
    #           x1 = 1,
    #           xref = "paper",
    #           y0 = thresholdMeanMVWet,
    #           y1 = thresholdMeanMVWet,
    #           yref = "y"
    #         ),
    #         list( # Transition minimum threshold
    #           layer = "below",
    #           line = list(color = "#81d3eb"),
    #           showlegend = FALSE,
    #           type = "line",
    #           x0 = 0,
    #           x1 = 1,
    #           xref = "paper",
    #           y0 = thresholdMeanMVDry,
    #           y1 = thresholdMeanMVDry,
    #           yref = "y"
    #         )
    #       ),
    #     xaxis = list(
    #       fixedrange = TRUE,
    #       range = list(
    #         ~(min(datetime) - 3000), # unix time
    #         ~(max(datetime) + 3000)
    #       ),
    #       ticktext = list(
    #         ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
    #       ),
    #       tickvals = list(
    #         ~(lubridate::ymd_hms(
    #           paste0(as.Date(max(datetime)), " 00:00:00"), 
    #           tz = "America/Phoenix"
    #         ))
    #       ),
    #       showgrid = TRUE,
    #       showticklabels = TRUE,
    #       title = FALSE,
    #       zeroline = FALSE
    #     ),
    #     yaxis = list(
    #       fixedrange = TRUE,
    #       rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
    #       title = "mV", 
    #       zeroline = FALSE
    #     )
    #   )
  ) # `past30DaysCardGraphs`
  
  
  return(past30DaysCardGraphs)
}
