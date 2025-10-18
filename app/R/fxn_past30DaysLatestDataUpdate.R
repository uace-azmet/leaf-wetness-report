#' `fxn_past30DaysLatestDataUpdate.R` - Build text for latest update information on Past 30 Days tab
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - AZMet daily leaf wetness data from `fxn_lwdaily.R`
#' @return `past30DaysLatestDataUpdate` - Text for latest update information on Past 30 Days tab


fxn_past30DaysLatestDataUpdate <- function(azmetStation, inData) {
  inData <- inData %>% 
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::filter(date == max(date))
  
  past30DaysLatestDataUpdate <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "<strong>Latest Update</strong><br>",
          dplyr::filter(inData, meta_station_name == azmetStation)$date
        )
      ),
      
      class = "past-30-days-latest-data-update"
    )
  
  return(past30DaysLatestDataUpdate)
}
