#' `fxn_past24HoursCardLayout.R` - Generate list of cards with 15-minute variable time series of 30-cm conditions based on selected station
#' 
#' @param azmetStation - user-specified AZMet station
#' @param inData - AZMet 15-minute leaf wetness data from `fxn_lw15min.R`
#' @param past24HoursCardGraphs - `list` of plotly graphs from `fxn_past24HoursCardGraphs.R`
#' @return `past24HoursCardLayout` - `list` of cards with 15-minute variable time series of 30-cm conditions based on selected station


fxn_past24HoursCardLayout <- function(inData, azmetStation, past24HoursCardGraphs) {
  
  
  # Variables ----------
  
  
  inData <- inData %>%
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) %>% 
    dplyr::group_by(meta_station_name) %>% 
    dplyr::filter(datetime == max(datetime)) %>% 
    dplyr::ungroup()
  
  cardHeight = "240px"
  classHeader <- "d-flex justify-content-between p-1 past-24-hours-card-header"
  styleHeaderHelpText <- "color: #989898; font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderSub <- "font-family: monospace; font-weight: bold;"
  styleHeaderSup <- "font-family: monospace; font-weight: normal;"
  styleHeaderValue <- "font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderVariable <- "font-family: monospace; font-weight: bold; font-size: 0.9rem;"
  
  
  # Cards ----------
  
  
  # `relative_humidity_30cm_mean` -----
  
  card_RH <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "RH"),
            "<sup>", tags$span(style = styleHeaderSup, "1"), "</sup>"
          )
        )
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue,
              paste0(format(inData$relative_humidity_30cm_mean, nsmall = 0)," %")
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(past24HoursCardGraphs[[1]], class = "p-0"),
    
    class = "past-24-hours-card",
    fill = TRUE,
    full_screen = TRUE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `temp_air_30cm_meanF` -----
  
  card_T <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "T"),
            "<sub>", tags$span(style = styleHeaderSub, "air"), "</sub>",
            "<sup>", tags$span(style = styleHeaderSup, "1"), "</sup>"
          )
        )
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue,
              htmltools::HTML(paste0(format(inData$temp_air_30cm_meanF, nsmall = 1), " °F"))
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(past24HoursCardGraphs[[2]], class = "p-0"),
    
    class = "past-24-hours-card",
    fill = TRUE,
    full_screen = TRUE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `dwpt_30cm_meanF` -----
  
  card_Tdewpoint <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "T"),
            "<sub>", tags$span(style = styleHeaderSub, "dew point"), "</sub>",
            "<sup>", tags$span(style = styleHeaderSup, "1"), "</sup>"
          )
        )
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue,
              paste0(format(inData$dwpt_30cm_meanF, nsmall = 1)," °F")
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(past24HoursCardGraphs[[3]], class = "p-0"),
    
    class = "past-24-hours-card",
    fill = TRUE,
    full_screen = TRUE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `lw*_mean_mV` -----
  
  card_DC <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "DC"),
            "<sup>", tags$span(style = styleHeaderSup, "1,2"), "</sup>"
          )
        )
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue,
              paste0(format(inData$lw1_mean_mV, nsmall = 0), ", ", format(inData$lw2_mean_mV, nsmall = 0), " mV")
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(past24HoursCardGraphs[[4]], class = "p-0"),
    
    class = "past-24-hours-card",
    fill = TRUE,
    full_screen = TRUE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  ) 
  
  
  # Card layout list ----------
  
  
  past24HoursCardLayout <- list(
    card_RH, 
    card_T, 
    card_Tdewpoint,
    card_DC
  )
  
  
  return(past24HoursCardLayout)
}
