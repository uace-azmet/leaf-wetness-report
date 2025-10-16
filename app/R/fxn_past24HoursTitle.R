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
            "Hover over data for variable values. Click or tap on the 'Expand' button to the lower right of the graphs to increase the viewing area.",
            id = "infoPast24HoursTitle",
            placement = "right"
          )
        ),
      ),
      
      class = "past-24-hours-title"
    )
  
  return(past24HoursTitle)
}
