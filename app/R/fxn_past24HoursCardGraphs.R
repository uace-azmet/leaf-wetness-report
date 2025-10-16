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
    dplyr::mutate(
      datetime = lubridate::ymd_hms(datetime),
      lw1_condition = dplyr::case_when(
        lw1_mean_mV <= thresholdMeanMVDry ~ "Dry",
        lw1_mean_mV >= thresholdMeanMVWet ~ "Wet",
        TRUE ~ "Transition"
      ),
      lw2_condition = dplyr::case_when(
        lw2_mean_mV <= thresholdMeanMVDry ~ "Dry",
        lw2_mean_mV >= thresholdMeanMVWet ~ "Wet",
        TRUE ~ "Transition"
      )
    )
  
  dataOtherStations <- inData %>%
    dplyr::filter(meta_station_name != azmetStation) %>% 
    dplyr::group_by(meta_station_name)
  
  dataSelectedStation <- inData %>%
    dplyr::filter(meta_station_name == azmetStation)
  
  hoverlabelFontColor = "#FFFFFF"
  hoverlabelFontSize = 14
  layoutFontColor = "#707070"
  layoutFontColorAnnotation = "#989898"
  layoutFontFamily = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  layoutFontSize = 11
  layoutMargin = 6
  layoutPadding = 0
  traceLineColorOtherStations = "rgba(201, 201, 201, 1.0)"
  traceLineColorSelectedStations = "#191919"
  traceLineWidth = 1
  traceMarkerColorOtherStations = "rgba(201, 201, 201, 1.0)"
  traceMarkerColorSelectedStation = "#191919"
  traceMarkerSize = 3
  
  # Scale Y axis for showing dry-transition-wet conditions
  maxMeanMVObs <- 
    max(
      c(max(inData$lw1_mean_mV, na.rm = TRUE), max(inData$lw2_mean_mV, na.rm = TRUE)), 
      na.rm = TRUE
    )
  
  if (maxMeanMVObs < thresholdMeanMVWet + 2) {
    maxMeanMVObs <- thresholdMeanMVWet + 2
  } else {
    maxMeanMVObs <- maxMeanMVObs + 2
  }
  
  minMeanMVObs <- 
    min(
      c(min(inData$lw1_mean_mV, na.rm = TRUE), min(inData$lw2_mean_mV, na.rm = TRUE)), 
      na.rm = TRUE
    )
  
  if (minMeanMVObs > thresholdMeanMVWet - 2) {
    minMeanMVObs <- thresholdMeanMVWet - 2
  } else {
    minMeanMVObs <- minMeanMVObs - 2
  }
  
  
  # Graphs ----------
  
  past24HoursCardGraphs <- list(
    
    
    # `relative_humidity_30cm_mean` -----
    
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
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
        "<br><b>RH:</b>  ", format(relative_humidity_30cm_mean, nsmall = 0), " %"
      ),
      name = "other stations",
      showlegend = TRUE,
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
          "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
          "<br><b>RH:</b>  ", format(relative_humidity_30cm_mean, nsmall = 0), " %"
        ),
        name = ~meta_station_name,
        showlegend = TRUE,
        legendgroup = "dataSelectedStation"
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
          title = "%", 
          zeroline = FALSE
        )
      ),
    
    
    # `temp_air_30cm_meanF` -----
    
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
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>air</sub>:</b>  ", format(temp_air_30cm_meanF, nsmall = 1), " °F"
      ),
      name = "other stations",
      showlegend = TRUE,
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
          "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
          "<br><b>T<sub>air</sub>:</b>  ", format(temp_air_30cm_meanF, nsmall = 1), " °F"
        ),
        name = ~meta_station_name,
        showlegend = TRUE,
        legendgroup = "dataSelectedStation"
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
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `dwpt_30cm_meanF` -----
    
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
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>dew point</sub>:</b>  ", format(dwpt_30cm_meanF, nsmall = 1), " °F"
      ),
      name = "other stations",
      showlegend = TRUE,
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
          "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
          "<br><b>T<sub>dew point</sub>:</b>  ", format(dwpt_30cm_meanF, nsmall = 1), " °F"
        ),
        name = ~meta_station_name,
        showlegend = TRUE,
        legendgroup = "dataSelectedStation"
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
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `lw*_mean_mV` -----
    
    plotly::plot_ly(
      data = dataOtherStations, 
      x = ~datetime, 
      y = ~lw1_mean_mV, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColorOtherStations, width = traceLineWidth),
      marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b>  ", format(datetime, "%H:%M:%S"),
        "<br><b>Sensor:</b>  Sensor 1",
        "<br><b>Condition:</b>  ", lw1_condition,
        "<br><b>DC:</b>  ", format(lw1_mean_mV, nsmall = 0), " mV"
      ),
      name = "other stations",
      showlegend = TRUE,
      legendgroup = "dataOtherStations"
    ) %>%
      
      plotly::add_trace(
        data = dataOtherStations, 
        x = ~datetime, 
        y = ~lw2_mean_mV, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = traceLineColorOtherStations, width = traceLineWidth),
        marker = list(color = traceMarkerColorOtherStations, size = traceMarkerSize),
        hoverinfo = "text",
        text = ~paste0(
          "<br><b>AZMet Station:</b>  ", meta_station_name,
          "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
          "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
          "<br><b>Sensor:</b>  Sensor 2",
          "<br><b>Condition:</b>  ", lw2_condition,
          "<br><b>DC:</b> ", format(lw2_mean_mV, nsmall = 0), " mV"
        ),
        name = "other stations",
        showlegend = FALSE,
        legendgroup = "dataOtherStations"
      ) %>% 
      
      plotly::add_trace(
        data = dataSelectedStation, 
        x = ~datetime, 
        y = ~lw1_mean_mV, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = traceLineColorSelectedStations, width = traceLineWidth),
        marker = list(color = traceLineColorSelectedStations, size = traceMarkerSize),
        hoverinfo = "text",
        text = ~paste0(
          "<br><b>AZMet Station:</b>  ", meta_station_name,
          "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
          "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
          "<br><b>Sensor:</b>  Sensor 1",
          "<br><b>Condition:</b>  ", lw1_condition,
          "<br><b>DC:</b> ", format(lw1_mean_mV, nsmall = 0), " mV"
        ),
        name = ~meta_station_name,
        showlegend = TRUE,
        legendgroup = "dataSelectedStation"
      ) %>% 
      
      plotly::add_trace(
        data = dataSelectedStation, 
        x = ~datetime, 
        y = ~lw2_mean_mV, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = traceLineColorSelectedStations, width = traceLineWidth),
        marker = list(color = traceLineColorSelectedStations, size = traceMarkerSize),
        hoverinfo = "text",
        text = ~paste0(
          "<br><b>AZMet Station:</b>  ", meta_station_name,
          "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
          "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
          "<br><b>Sensor:</b>  Sensor 2",
          "<br><b>Condition:</b>  ", lw2_condition,
          "<br><b>DC:</b> ", format(lw2_mean_mV, nsmall = 0), " mV"
        ),
        name = ~meta_station_name,
        showlegend = FALSE,
        legendgroup = "dataSelectedStation"
      ) %>% 
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      
      plotly::layout(
        annotations = list(
          list( # Transition
            align = "left",
            font = list(
              color = "#add8e6",
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "Transition between Dry and Wet",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 5,
            y = thresholdMeanMVDry + 1,
            yanchor = "bottom",
            yref = "y"
          ),
          list( # Wet
            align = "left",
            font = list(
              color = "#378dbd",
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "Wet",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 5,
            # y = mean(c(maxMeanMVObs, thresholdMeanMVWet)),
            y = thresholdMeanMVWet + 1,
            yanchor = "bottom",
            yref = "y"
          ),
          list( # Dry
            align = "left",
            font = list(
              color = layoutFontColorAnnotation,
              family = layoutFontFamily,
              size = layoutFontSize
            ),
            showarrow = FALSE,
            text = "Dry",
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 5,
            y = mean(c(minMeanMVObs, thresholdMeanMVDry)),
            yanchor = "center",
            yref = "y"
          )
        ),
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
        shapes = 
          list(
            list(
              # fillcolor = "#add8e6",
              layer = "below",
              line = list(color = "#add8e6"),
              # opacity = 0.5,
              showlegend = FALSE,
              # type = "rect",
              type = "line",
              x0 = 0,
              x1 = 1,
              xref = "paper",
              y0 = thresholdMeanMVDry, # Transition min
              y1 = thresholdMeanMVDry, # Transition max
              # y1 = thresholdMeanMVWet, # Transition max
              yref = "y"
            ),
            list(
              # fillcolor = "#378dbd",
              layer = "below",
              line = list(color = "#378dbd"),
              # opacity = 0.5,
              showlegend = FALSE,
              type = "line",
              x0 = 0,
              x1 = 1,
              xref = "paper",
              y0 = thresholdMeanMVWet, # Wet min
              y1 = thresholdMeanMVWet, # Wet max
              yref = "y"
            )#,
            # list(
            #   fillcolor = "#eeeeee",
            #   layer = "below",
            #   line = list(width = 0),
            #   opacity = 1.0,
            #   showlegend = FALSE,
            #   type = "rect",
            #   x0 = 0,
            #   x1 = 1,
            #   xref = "paper",
            #   y0 = minMeanMVObs, # Dry min
            #   y1 = thresholdMeanMVDry, #maxMeanMV, # Dry max
            #   yref = "y"
            # )
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
          title = "mV", 
          zeroline = FALSE
        )
      )
  ) # `past24HoursCardGraphs`
  
  return(past24HoursCardGraphs)
}
