#' `fxn_latestConditionsTableFooter.R` - Build footer for latest conditions table
#' 
#' @return `latestConditionsTableFooter` - Footer for latest conditions table


fxn_latestConditionsTableFooter <- function() {
  latestConditionsTableFooter <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "<sup>1</sup> Values based on measurements taken 12 inches above ground&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Values highlighted in orange when at or below ", format(thresholdTempAir, nsmall = 1), " °F&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>3</sup> Values highlighted in orange when at or above air temperature&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>4</sup> Bars colored gray for dry conditions (DRY), light blue for the transition between dry and wet conditions (T), and blue for wet conditions (WET) 
        
        <br><br>DC values less than or equal to ", thresholdMeanMVDry, " mV indicate dry conditions, between ", thresholdMeanMVDry, " and ", thresholdMeanMVWet, " mV the transition between dry and wet conditions, and equal to or greater than ", thresholdMeanMVWet, " mV wet conditions. Higher DC values indicate greater leaf wetness. Values of 'NA' denote no data. Variable key: <strong>RH</strong> relative humidity in percent; <strong>T<sub>air</sub></strong> air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit; <strong>DC</strong> dielectric constant in millivolts, proportional to the amount of water or ice on the upper surface of a leaf wetness sensor"
        )
      ),
      
      class = "latest-conditions-table-footer"
    )
  
  return(latestConditionsTableFooter)
}
