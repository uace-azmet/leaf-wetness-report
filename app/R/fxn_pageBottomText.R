#' `fxn_pageBottomText.R` - Build supporting text for page
#' 
#' @param activeTab - currently active tab in user session
#' @return `pageBottomText` - Supporting text for page


fxn_pageBottomText <- function(activeTab) {
  
  
  # Define inputs -----
  
  if (activeTab == "latest-conditions" | activeTab == "past-24-hours") {
    apiURL <- a(
      "api.azmet.arizona.edu", 
      href="https://api.azmet.arizona.edu/v1/observations/lw15min",
      target="_blank"
    )
    
    timeStep <- "15-minute"
  } else { # activeTab == "past-30-days"
    apiURL <- a(
      "api.azmet.arizona.edu", 
      href="https://api.azmet.arizona.edu/v1/observations/lwdaily",
      target="_blank"
    )
    
    timeStep <- "daily"
  }
  
  azmetrURL <- a(
    "azmetr", 
    href="https://uace-azmet.github.io/azmetr/",
    target="_blank"
  )
  
  todayDate <- gsub(" 0", " ", format(lubridate::today(), "%B %d, %Y"))
  
  todayYear <- lubridate::year(lubridate::today())
  
  webpageCode <- a(
    "GitHub page", 
    href="https://github.com/uace-azmet/leaf-wetness-report", 
    target="_blank"
  )
  
  webpageDataVariables <- a(
    "data variables", 
    href="https://azmet.arizona.edu/about/data-variables", 
    target="_blank"
  )
  
  webpageNetworkMap <- a(
    "station locations", 
    href="https://azmet.arizona.edu/about/network-map", 
    target="_blank"
  )
  
  webpageStationMetadata <- a(
    "station metadata", 
    href="https://azmet.arizona.edu/about/station-metadata", 
    target="_blank"
  )
  
  webpageAZMet <- a(
    "AZMet website", 
    href="https://azmet.arizona.edu/", 
    target="_blank"
  )
  
  
  # Build text -----
  
  pageBottomText <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "AZMet ", timeStep, " leaf wetness data are from ", apiURL, " and accessed using the ", azmetrURL, " R package. Values from recent dates may be based on provisional data. More information about ", webpageDataVariables, ", ", webpageNetworkMap, ", and ", webpageStationMetadata, " is available on the ", webpageAZMet, ". Users of AZMet data and related information assume all risks of its use.",
          htmltools::br(), htmltools::br(),
          "To cite the above AZMet data, please use: 'Arizona Meteorological Network (", todayYear, ") Arizona Meteorological Network (AZMet) Data. https://azmet.arizona.edu. Accessed ", todayDate, "', along with 'Arizona Meteorological Network (", todayYear, ") Battery Voltage Viewer. https://viz.datascience.arizona.edu/azmet/battery-voltage-viewer. Accessed ", todayDate, "'.",
          htmltools::br(), htmltools::br(),
          "For information on how this webpage is put together, please visit the ", webpageCode, " for this tool."
        )
      ),
      
      class = "page-bottom-text"
    )
  
  return(pageBottomText)
}
