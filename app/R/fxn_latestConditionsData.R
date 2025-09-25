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
      temp_air_30cm_meanF,
      dwpt_30cm_meanF
    ) |>
    
    dplyr::group_by(meta_station_name) |>
    dplyr::filter(datetime == max(datetime)) |>
    dplyr::ungroup() |>
    
    reshape2::melt(
      id.vars = c("meta_station_name", "datetime", "temp_air_30cm_meanF", "dwpt_30cm_meanF"),
      measure.vars = c("lw1_mean_mV", "lw2_mean_mV"),
      variable.name = "lwSensor",
      value.name = "mean_mV",
      na.rm = FALSE
    ) |>
    
    # dplyr::mutate(
    #   lwSensor = 
    #     dplyr::if_else(lwSensor == "lw1_mean_mv", "Sensor 1", "Sensor 2")
    # ) |>
    
    dplyr::select(
      meta_station_name, 
      datetime, 
      temp_air_30cm_meanF, 
      dwpt_30cm_meanF, 
      mean_mV, 
      lwSensor
    ) |>
    dplyr::arrange(meta_station_name, lwSensor)
    
  return(latestConditionsData)
}
