#' `fxn_past24HoursGraphFooter.R` - Build footer for past 24 hours graph
#' 
#' @return `past24HoursGraphFooter` - Footer for past 24 hours graph


fxn_past24HoursGraphFooter <- function() {
  past24HoursGraphFooter <- 
    htmltools::p(
      htmltools::HTML(
        "<sup>1</sup> Measurements taken 12 inches above ground
        
        <br><br>DC values less than or equal to 273 mV suggest dry conditions, between 273 and 284 mV the transition between dry and wet conditions, and equal to or greater than 284 mV wet conditions. Higher DC values indicate greater leaf wetness. Variable key: <strong>RH</strong> relative humidity in percent; <strong>T<sub>air</sub></strong> air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit; <strong>DC</strong> dielectric constant in millivolts, proportional to the amount of water or ice on the upper surface of a Campbell Science leaf wetness sensor"
      ),
      
      class = "past-24-hours-graph-footer"
    )
  
  return(past24HoursGraphFooter)
}
