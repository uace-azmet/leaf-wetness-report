#' `fxn_past30DaysTitle.R` - Build title for past 30 days tab
#' 
#' @return `past30DaysTitle` - Title for past 30 days tab


fxn_past30DaysTitle <- function() {
  past30DaysTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper(
            paste0(
              "Leaf wetness conditions over the past 30 days from across Yuma County"
            )
          ),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Hover over data for variable values. Click or tap on the 'Expand' button to the lower right of the graphs to increase the viewing area.",
            id = "infoPast30DaysTitle",
            placement = "right"
          )
        ),
      ),
      
      class = "past-30-days-title"
    )
  
  return(past30DaysTitle)
}
