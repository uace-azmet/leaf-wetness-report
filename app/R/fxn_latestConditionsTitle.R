#' `fxn_latestConditionsTitle.R` - Build title for latest conditions tab
#' 
#' @return `latestConditionsTitle` - Title for latest conditions tab


fxn_latestConditionsTitle <- function() {
  latestConditionsTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("table"), 
          htmltools::HTML("&nbsp;&nbsp;"),
          toupper("The latest leaf wetness conditions from across Yuma County"),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Scroll or swipe over the table to view additional columns on narrow browser windows. Hover over bar graphs for wetness conditions.",
            id = "infolatestConditionsTitle",
            placement = "right"
          )
        )
      ),
      
      class = "latest-conditions-title"
    )
  
  return(latestConditionsTitle)
}
