#' `fxn_past30DaysGraphFooter.R` - Build footer for past 30 days graph
#' 
#' @return `past30DaysGraphFooter` - Footer for past 30 days graph


fxn_past30DaysGraphFooter <- function() {
  past30DaysGraphFooter <- 
    htmltools::p(
      htmltools::HTML(
        "<sup>1</sup> Measurements taken 12 inches above ground&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Values highlighted in orange when at or below 32.0 °F&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>3</sup> Values highlighted in orange when at or above air temperature&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>4</sup> Bars colored gray for dry conditions (DRY), light blue for the transition between dry and wet conditions (T), and blue for wet conditions (WET) 
        
        <br><br>DC values less than or equal to 273 mV suggest dry conditions, between 273 and 284 mV the transition between dry and wet conditions, and equal to or greater than 284 mV wet conditions. Higher DC values indicate greater leaf wetness. Values of 'NA' denote no data. Variable key: <strong>T</strong> air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit; <strong>DC</strong> dielectric constant in millivolts, proportional to the amount of water or ice on the upper surface of a Campbell Science leaf wetness sensor"
      ),
      
      class = "past-30-days-graph-footer"
    )
  
  return(past30DaysGraphFooter)
}
