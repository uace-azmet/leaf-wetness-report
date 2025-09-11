#' `fxn_latestConditionsTableTitle.R` - Build title for latest conditions table
#' 
#' @return `latestConditionsTableTitle` - Title for latest conditions table


fxn_latestConditionsTableTitle <- function() {
  latestConditionsTableTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("table"), 
          htmltools::HTML("&nbsp;&nbsp;"),
          toupper("The latest leaf wetness conditions from across Yuma County")
        )
      ),
      htmltools::HTML("&nbsp;&nbsp;"),
      bslib::tooltip(
        bsicons::bs_icon("info-circle"),
        "Scroll or swipe over the table to view additional rows and columns.",
        id = "infolatestConditionsTableTitle",
        placement = "right"
      ),
      
      class = "latest-conditions-table-title"
    )
  
  return(latestConditionsTableTitle)
}
