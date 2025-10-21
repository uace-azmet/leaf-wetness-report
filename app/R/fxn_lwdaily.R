#' `fxn_lwdaily.R` Download AZMet daily leaf wetness data, transform variables, and return to app
#' 
#' @return `lwdaily` - Transformed AZMet daily leaf wetness data over previous 24 hours, tibble format


fxn_lwdaily <- function() {
  idRetrievingData <- shiny::showNotification(
    ui = "Retrieving the latest data . . .",
    action = NULL,
    duration = NULL,
    closeButton = FALSE,
    id = "idRetrievingData",
    type = "message"
  )
  
  lwdaily <- 
    azmetr::az_lwdaily(
      start_date = 
        lubridate::today(tzone = "America/Phoenix") - lubridate::days(30)
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
      date = format(date, format = "%Y-%m-%d"),
      dwpt_30cm_maxF = fxn_c_to_f(dwpt_30cm_max),
      dwpt_30cm_meanF = fxn_c_to_f(dwpt_30cm_mean),
      dwpt_30cm_minF = fxn_c_to_f(dwpt_30cm_min),
      lw1_total_con_hrs = lw1_total_con_mins / 60,
      lw1_total_dry_hrs = lw1_total_dry_mins / 60,
      lw1_total_wet_hrs = lw1_total_wet_mins / 60,
      lw2_total_con_hrs = lw2_total_con_mins / 60,
      lw2_total_dry_hrs = lw2_total_dry_mins / 60,
      lw2_total_wet_hrs = lw2_total_wet_mins / 60,
      temp_air_30cm_maxF = fxn_c_to_f(temp_air_30cm_maxC),
      temp_air_30cm_meanF = fxn_c_to_f(temp_air_30cm_meanC),
      temp_air_30cm_minF = fxn_c_to_f(temp_air_30cm_minC)
    ) |>
    
    dplyr::mutate(
      dplyr::across(
        c(
          "relative_humidity_30cm_max",
          "relative_humidity_30cm_mean",
          "relative_humidity_30cm_min"
        ),
        \(x) round(x, digits = 0)
      )
    ) |>
    
    dplyr::mutate(
      dplyr::across(
        c(
          "dwpt_30cm_maxF",
          "dwpt_30cm_meanF",
          "dwpt_30cm_minF",
          "lw1_total_wet_hrs",
          "lw1_total_con_hrs",
          "lw1_total_dry_hrs",
          "lw2_total_wet_hrs",
          "lw2_total_con_hrs",
          "lw2_total_dry_hrs",
          "temp_air_30cm_maxF",
          "temp_air_30cm_meanF",
          "temp_air_30cm_minF"
        ),
        \(x) round(x, digits = 1)
      )
    ) |>
    
    dplyr::arrange(meta_station_name)
  
  on.exit(shiny::removeNotification(id = idRetrievingData), add = TRUE)
  
  return(lwdaily)
}
