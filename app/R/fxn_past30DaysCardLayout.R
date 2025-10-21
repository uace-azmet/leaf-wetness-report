#' `fxn_past30DaysCardLayout.R` - Generate list of cards with daily variable time series of 30-cm conditions based on selected station
#' 
#' @param azmetStation - user-specified AZMet station
#' @param inData - AZMet daily leaf wetness data from `fxn_lwdaily.R`
#' @param past30DaysCardGraphs - `list` of plotly graphs from `fxn_past30DaysCardGraphs.R`
#' @return `past30DaysCardLayout` - `list` of cards with daily variable time series of 30-cm conditions based on selected station


fxn_past30DaysCardLayout <- function(inData, azmetStation, past30DaysCardGraphs) {
  
  
  # Variables ----------
  
  inData <- inData %>%
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(date = lubridate::ymd(date)) %>% 
    dplyr::group_by(meta_station_name) %>% 
    dplyr::filter(date == max(date)) %>% 
    dplyr::ungroup()
  
  cardHeight = "360px"
  classHeader <- "d-flex justify-content-between p-1 past-30-days-card-header"
  styleHeaderHelpText <- "color: #989898; font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderSub <- "font-family: monospace; font-weight: bold;"
  styleHeaderSup <- "font-family: monospace; font-weight: normal;"
  styleHeaderValue <- "font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderVariable <- "font-family: monospace; font-weight: bold; font-size: 0.9rem;"
  
  
  # Cards ----------
  
  # `relative_humidity_30cm_*` -----
  
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
      
      class = classHeader
    ),
    
    bslib::card_body(past30DaysCardGraphs[[1]], class = "p-0"),
    
    class = "past-30-days-card",
    fill = TRUE,
    full_screen = TRUE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `temp_air_30cm_*` -----
  
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
      
      class = classHeader
    ),
    
    bslib::card_body(past30DaysCardGraphs[[2]], class = "p-0"),
    
    class = "past-30-days-card",
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
      
      class = classHeader
    ),
    
    bslib::card_body(past30DaysCardGraphs[[3]], class = "p-0"),
    
    class = "past-30-days-card",
    fill = TRUE,
    full_screen = TRUE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `lw*_total_***_hrs` -----
  
  card_condition_hrs <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "Condition"),
            "<sup>", tags$span(style = styleHeaderSup, "1"), "</sup>"
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(past30DaysCardGraphs[[4]], class = "p-0"),
    
    class = "past-30-days-card",
    fill = TRUE,
    full_screen = TRUE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # Card layout list ----------
  
  past30DaysCardLayout <- list(
    card_RH, 
    card_T, 
    card_Tdewpoint,
    card_condition_hrs
  )
  
  
  return(past30DaysCardLayout)
}
