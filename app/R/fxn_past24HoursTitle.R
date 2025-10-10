#' `fxn_past24HoursTitle.R` - Build title for past 24 hours tab
#' 
#' @return `past24HoursTitle` - Title for past 24 hours tab


fxn_past24HoursTitle <- function() {
  past24HoursTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper(
            paste0(
              "Leaf wetness conditions over the past 24 hours from across Yuma County"
            )
          ),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Click or tap and drag to zoom into an area of interest. Hover over data for variable values. Click or tap on legend items to toggle data visibility. Select from the icons to the right of the graph for additional functionality.",
            id = "infoPast24HoursTitle",
            placement = "right"
          )
        ),
      ),
      
      class = "past-24-hours-title"
    )
  
  return(past24HoursTitle)
}
