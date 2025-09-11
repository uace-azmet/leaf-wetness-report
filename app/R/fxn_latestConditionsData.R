#' `fxn_latestConditionsData.R` - Filter 15-minute leaf wetness data for the most recent report from each station
#' 
#' @param inData - 15-minute leaf wetness data from `fxn_lw15min.R`
#' @return `latestConditionsData` - Most recent leaf wetness data from each station, tibble format


fxn_latestConditionsData <- function(inData) {
  latestConditionsData <- inData |>
    dplyr::select(
      meta_station_name,
      datetime,
      lw1_mean_mV,
      lw2_mean_mV,
      temp_wetbulb_meanF
    ) |>
    
    dplyr::group_by(meta_station_name) |>
    dplyr::filter(datetime == max(datetime)) |>
    dplyr::ungroup()
    
  return(latestConditionsData)
}
