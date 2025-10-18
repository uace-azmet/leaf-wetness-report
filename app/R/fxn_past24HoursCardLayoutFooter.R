#' `fxn_past24HoursCardLayoutFooter.R` - Build footer for past 24 hours card layout
#' 
#' @return `past24HoursCardLayoutFooter` - Footer for past 24 hours card layout


fxn_past24HoursCardLayoutFooter <- function() {
  past24HoursCardLayoutFooter <- 
    htmltools::p(
      htmltools::HTML(
        "<sup>1</sup> Values based on measurements taken 12 inches above ground&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Latest Update values are from Sensor 1 and Sensor 2, respectively
        
        <br><br>DC values less than or equal to 273 mV suggest dry conditions, between 273 and 284 mV the transition between dry and wet conditions, and equal to or greater than 284 mV wet conditions. Higher DC values indicate greater leaf wetness. Variable key: <strong>RH</strong> relative humidity in percent; <strong>T<sub>air</sub></strong> air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit; <strong>DC</strong> dielectric constant in millivolts, proportional to the amount of water or ice on the upper surface of a leaf wetness sensor"
      ),
      
      class = "past-24-hours-card-layout-footer"
    )
  
  return(past24HoursCardLayoutFooter)
}
