#' `fxn_past24HoursLatestDataUpdate.R` - Build text for latest update information
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - AZMet 15-minute leaf wetness data from `fxn_lw15min.R`
#' @return `past24HoursLatestDataUpdate` - Text for latest update information


fxn_past24HoursLatestDataUpdate <- function(azmetStation, inData) {
  inData <- inData %>% 
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::filter(datetime == max(datetime))
  
  past24HoursLatestDataUpdate <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "<strong>Latest Update</strong><br>",
          dplyr::filter(inData, meta_station_name == azmetStation)$datetime
        )
      ),
      
      class = "past-24-hours-latest-data-update"
    )
  
  return(past24HoursLatestDataUpdate)
}
