#' `fxn_latestConditionsData.R` - Filter 15-minute leaf wetness data for the most recent report from each station
#' 
#' @param inData - 15-minute leaf wetness data from `fxn_lw15min.R`
#' @return `latestConditionsData` - Most recent leaf wetness data from each station, tibble format


fxn_latestConditionsData <- function(inData) {
  latestConditionsData <- inData |>
    dplyr::group_by(meta_station_name) |>
    dplyr::filter(datetime == max(datetime)) |>
    dplyr::ungroup() |>
    
    reshape2::melt(
      id.vars = c("meta_station_name", "datetime", "temp_air_30cm_meanF", "dwpt_30cm_meanF"),
      measure.vars = c("lw1_mean_mV", "lw2_mean_mV"),
      variable.name = "lw_sensor",
      value.name = "mean_mV",
      na.rm = FALSE
    ) |>
    
    dplyr::arrange(meta_station_name, lw_sensor) |>
    
    # https://stackoverflow.com/questions/78275267/add-a-second-groupname-col-in-gt-table-without-concatenating-the-column-valu
    dplyr::mutate(
      datetime = ifelse(dplyr::row_number() == 1, datetime, ""),
      temp_air_30cm_meanF = ifelse(dplyr::row_number() == 1, temp_air_30cm_meanF, NA),
      dwpt_30cm_meanF = ifelse(dplyr::row_number() == 1, dwpt_30cm_meanF, NA), 
      .by = meta_station_name
    ) |>
    
    dplyr::mutate(
      meta_station_name =
        dplyr::if_else(
          lw_sensor == "lw1_mean_mV", meta_station_name, NA
        ),
      lw_sensor =
        dplyr::if_else(
          lw_sensor == "lw1_mean_mV", "Sensor 1", "Sensor 2"
        )
      # graph_count = seq(1:nrow(inData))
    ) |>
    
    dplyr::select(
      meta_station_name,
      datetime,
      temp_air_30cm_meanF,
      dwpt_30cm_meanF,
      mean_mV,
      lw_sensor
    )
    
    # latestConditionsData <- latestConditionsData |>
    #   dplyr::mutate(graph_count = seq(1:nrow(latestConditionsData)))
    
  return(latestConditionsData)
}
