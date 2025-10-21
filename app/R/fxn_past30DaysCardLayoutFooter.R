#' `fxn_past30DaysCardLayoutFooter.R` - Build footer for past 30 days card layout
#' 
#' @return `past30DaysCardLayoutFooter` - Footer for past 30 days card layout


fxn_past30DaysCardLayoutFooter <- function() {
  past30DaysCardLayoutFooter <- 
    htmltools::p(
      htmltools::HTML(
        "<sup>1</sup> Values based on measurements taken 12 inches above ground 
        
        <br><br>DC values less than or equal to 273 mV suggest dry conditions, between 273 and 284 mV the transition between dry and wet conditions, and equal to or greater than 284 mV wet conditions. Higher DC values indicate greater leaf wetness. Values of 'NA' denote no data. Variable key: <strong>T<sub>air</sub></strong> air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit; <strong>DC</strong> dielectric constant in millivolts, proportional to the amount of water or ice on the upper surface of a leaf wetness sensor"
      ),
      
      class = "past-30-days-card-layout-footer"
    )
  
  return(past30DaysCardLayoutFooter)
}
