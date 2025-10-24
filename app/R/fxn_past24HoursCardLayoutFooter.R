#' `fxn_past24HoursCardLayoutFooter.R` - Build footer for past 24 hours card layout
#' 
#' @return `past24HoursCardLayoutFooter` - Footer for past 24 hours card layout


fxn_past24HoursCardLayoutFooter <- function() {
  past24HoursCardLayoutFooter <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "<sup>1</sup> Values based on measurements taken 12 inches above ground&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Latest Update values are from Sensor 1 and Sensor 2, respectively
        
        <br><br>Values of 'NA' denote no data. Variable key: <strong>RH</strong> relative humidity in percent; <strong>T<sub>air</sub></strong> air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit; <strong>DC</strong> dielectric constant in millivolts, proportional to the amount of water or ice on the upper surface of a leaf wetness sensor. DC values less than or equal to ", thresholdMeanMVDry, " mV indicate dry conditions, between ", thresholdMeanMVDry, " and ", thresholdMeanMVWet, " mV the transition between dry and wet conditions, and equal to or greater than ", thresholdMeanMVWet, " mV wet conditions. Higher DC values indicate greater leaf wetness."
        )
      ),
      
      class = "past-24-hours-card-layout-footer"
    )
  
  return(past24HoursCardLayoutFooter)
}
