#' `fxn_lw15min.R` Download AZMet 15-minute leaf wetness data, transform variables, and return to app
#' 
#' @return `lw15min` - Transformed AZMet 15-minute leaf wetness data over previous 24 hours, tibble format


fxn_lw15min <- function() {
  # idRetrievingData <- shiny::showNotification(
  #   ui = "Retrieving the latest data . . .",
  #   action = NULL,
  #   duration = NULL,
  #   closeButton = FALSE,
  #   id = "idRetrievingData",
  #   type = "message"
  # )
  
  lw15min <- 
    azmetr::az_lw15min(
      start_date_time = 
        lubridate::now(tzone = "America/Phoenix") - lubridate::hours(25)
    ) |> 
    
    dplyr::filter(
      meta_station_name %in% c(
        "Roll", 
        "Wellton ETo", 
        "Yuma N.Gila", 
        "Yuma South", 
        "Yuma Valley"
      )
    ) |> 
    
    dplyr::mutate(
      datetime = format(datetime, format = "%Y-%m-%d %H:%M:%S"),
      dwpt_30cm_meanF = fxn_c_to_f(dwpt_30cm_mean),
      temp_air_30cm_meanF = fxn_c_to_f(temp_air_30cm_meanC),
      temp_air_meanF = fxn_c_to_f(temp_air_meanC),
      temp_wetbulb_meanF = fxn_c_to_f(temp_wetbulb_meanC),
      wind_spd_max_mph = fxn_mps_to_mph(wind_spd_max_mps),
      wind_spd_mean_mph = fxn_mps_to_mph(wind_spd_mean_mps),
      wind_spd_min_mph = fxn_mps_to_mph(wind_spd_min_mps)
    ) |>
    
    dplyr::mutate(
      dplyr::across(
        c(
          "lw1_mean_mV",
          "lw1_total_con_mins",
          "lw1_total_dry_mins",
          "lw1_total_wet_mins",
          "lw2_mean_mV",
          "lw2_total_con_mins",
          "lw2_total_dry_mins",
          "lw2_total_wet_mins",
          "relative_humidity_30cm_mean",
          "relative_humidity_mean"
        ),
        \(x) round(x, digits = 0)
      )
    ) |>
    
    dplyr::mutate(
      dplyr::across(
        c(
          "dwpt_30cm_meanF",
          "temp_air_30cm_meanF",
          "temp_air_meanF",
          "temp_wetbulb_meanF",
          "wind_spd_max_mph",
          "wind_spd_mean_mph",
          "wind_spd_min_mph"
        ),
        \(x) round(x, digits = 1)
      )
    ) |>
    
    dplyr::arrange(meta_station_name)
  
  # on.exit(shiny::removeNotification(id = idRetrievingData), add = TRUE)
  
  return(lw15min)
}
