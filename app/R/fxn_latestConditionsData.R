#' `fxn_latestConditionsData.R` - Transform most recent 15-minute leaf wetness data from each station for table
#' 
#' @param inData - 15-minute leaf wetness data from `fxn_lw15min.R`
#' @return `latestConditionsData` - Most recent 15-minute leaf wetness data from each station, transformed for table


fxn_latestConditionsData <- function(inData) {
  
  latestConditionsData <- inData %>%
    dplyr::group_by(meta_station_name) %>%
    dplyr::filter(datetime == max(datetime)) %>%
    dplyr::ungroup() %>%
    
    reshape2::melt(
      id.vars = c("meta_station_name", "datetime", "temp_air_30cm_meanF", "dwpt_30cm_meanF"),
      measure.vars = c("lw1_mean_mV", "lw2_mean_mV"),
      variable.name = "lw_sensor",
      value.name = "mean_mV",
      na.rm = FALSE
    ) %>%
    
    dplyr::arrange(meta_station_name, lw_sensor) %>%
    
    # https://stackoverflow.com/questions/78275267/add-a-second-groupname-col-in-gt-table-without-concatenating-the-column-valu
    dplyr::mutate(
      meta_station_name = ifelse(dplyr::row_number() == 1, meta_station_name, ""),
      datetime = ifelse(dplyr::row_number() == 1, datetime, ""),
      temp_air_30cm_meanF = ifelse(dplyr::row_number() == 1, temp_air_30cm_meanF, NA),
      dwpt_30cm_meanF = ifelse(dplyr::row_number() == 1, dwpt_30cm_meanF, NA),
      lw_sensor = ifelse(dplyr::row_number() == 1, "&nbsp;&nbsp;&nbsp;Sensor 1", "&nbsp;&nbsp;&nbsp;Sensor 2"),
      .by = meta_station_name
    ) %>%
    
    dplyr::mutate(
      meta_station_name = factor(meta_station_name, levels = unique(meta_station_name)),
      row_number = seq(1:nrow(.)),
      mean_mV_adj = (mean_mV - minMeanMV) / rangeMeanMV,
      condition = dplyr::case_when(
        mean_mV <= thresholdMeanMVDry ~ "Dry",
        mean_mV >= thresholdMeanMVWet ~ "Wet",
        TRUE ~ "Transition between Wet and Dry"
      ),
      temp_air_color = dplyr::case_when(
        temp_air_30cm_meanF <= thresholdTempAir ~ "#f19e1f",
        TRUE ~ "#FFFFFF"
      ),
      dwpt_color = dplyr::case_when(
        dwpt_30cm_meanF >= temp_air_30cm_meanF ~ "#f19e1f",
        TRUE ~ "#FFFFFF"
      ),
      bar_color = dplyr::case_when(
        mean_mV <= thresholdMeanMVDry ~ "#bfbfbf",
        mean_mV >= thresholdMeanMVWet ~ "#378dbd",
        TRUE ~ "add8e6" # --azmet-light-blue
      )
    ) %>%
    
    dplyr::select(
      meta_station_name,
      datetime,
      temp_air_30cm_meanF,
      dwpt_30cm_meanF,
      lw_sensor,
      row_number,
      mean_mV,
      mean_mV_adj,
      condition,
      temp_air_color,
      dwpt_color,
      bar_color
    )
    
  return(latestConditionsData)
}
