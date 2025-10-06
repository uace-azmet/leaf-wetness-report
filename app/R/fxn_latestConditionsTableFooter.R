#' `fxn_latestConditionsTableFooter.R` - Build footer for latest conditions table
#' 
#' @return `latestConditionsTableFooter` - Footer for latest conditions table


fxn_latestConditionsTableFooter <- function() {
  latestConditionsTableFooter <- 
    htmltools::p(
      htmltools::HTML(
        "<sup>1</sup> Measurements taken 12 inches above ground&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Values highlighted in orange when at or below 32.0 °F&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>3</sup> Values highlighted in orange when at or above air temperature
        
        <br><br>Values of 'NA' denote no data. Variable key: <strong>T</strong> air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit;"
      ),
      
      class = "latest-conditions-table-footer"
    )
  
  return(latestConditionsTableFooter)
}
