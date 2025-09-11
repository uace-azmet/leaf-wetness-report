#' `fxn_latestConditionsTableFooter.R` - Build footer for latest conditions table
#' 
#' @return `latestConditionsTableFooter` - Footer for latest conditions table


fxn_latestConditionsTableFooter <- function() {
  latestConditionsTableFooter <- 
    htmltools::p(
      htmltools::HTML(
        "<sup>1</sup> Since midnight local time&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Since the top of the hour<br><br>Variable key: <strong>Batt</strong> meta_bat_volt; <strong>P</strong><sup>1</sup> precip_total_in; <strong>RH</strong> relative_humidity; <strong>SR</strong> sol_rad_Wm2; <strong>T</strong> temp_airF; <strong>T<sub>max</sub></strong><sup>1</sup> temp_air_maxF; <strong>T<sub>min</sub></strong><sup>1</sup> temp_air_minF; <strong>T<sub>panel</sub></strong> temp_panelF; <strong>T<sub>dew point</sub></strong> dwptF;"
      ),
      
      class = "latest-conditions-table-footer"
    )
  
  return(latestConditionsTableFooter)
}
