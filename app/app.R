# Current conditions and recent data of leaf wetness estimates in Yuma County

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->
# 'azmet-shiny-template.html': <!-- CSS specific to this AZMet Shiny app -->

# Libraries
#library(azmetr)
library(bslib)
library(dplyr)
library(DT)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)

# Functions
#source("./R/fxnABC.R", local = TRUE)

# Scripts
#source("./R/scr##DEF.R", local = TRUE)


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  filename = "azmet-shiny-template.html",
  
  pageNavbar= bslib::page_navbar(
    bslib::nav_panel(
      title = "Current Conditions", 
      bslib::layout_columns(
        cardsCurrentConditions[[1]], 
        cardsCurrentConditions[[2]], 
        cardsCurrentConditions[[3]], 
        cardsCurrentConditions[[4]], 
        cardsCurrentConditions[[5]],
        
        col_widths = breakpoints(sm = c(12), md = c(6, 6), lg = c(4, 4, 4))
      ),
      value = "Current Conditions"
    ),
    bslib::nav_panel(
      title = "Past 48 Hours", 
      htmltools::p("Second tab content."),
      value = "Past 48 Hours"
    ),
    bslib::nav_panel(
      title = "Past 30 Days", 
      htmltools::p("Third tab content"),
      DT::datatable(
        iris, extensions = 'FixedHeader',
        options = list(pageLength = 50, fixedHeader = TRUE)
      ),
      value = "Past 30 Days"
    ),
    
    collapsible = FALSE,
    fillable = TRUE,
    fillable_mobile = FALSE,
    footer = shiny::htmlOutput(outputId = "reportPageText"),
    id = "pageNavbar",
    selected = "Past 48 Hours",
    sidebar = NULL,
    theme = theme, # `scr03_theme.R`
    title = "NULL",
    #underline = TRUE,
    #fluid = TRUE,
    #window_title = "Leaf Wetness Report"
  #)
  
  #bslib::page_navbar(
  #  title = "Page NavBar",
  #  bslib::nav_panel(
  #    title = "Current Conditions",
  #    p("First tab content.")
  #bslib::layout_columns(
  #  cardsCurrentConditions[[1]], 
  #  cardsCurrentConditions[[2]], 
  #  cardsCurrentConditions[[3]], 
  #  cardsCurrentConditions[[4]], 
  #  cardsCurrentConditions[[5]]
  #)
  #  ),
  #  bslib::nav_panel(title = "Recent Data", p("Second tab content.")),
  #  bslib::nav_panel(title = "Data Variables", p("Third tab content."))
    
  # `scr0x_navsetTab.R`
  #),
  
  #shiny::htmlOutput(outputId = "figureHelpText"),
  #htmltools::br(),
  #htmltools::br(),
  #htmltools::br(),
  #shiny::htmlOutput(outputId = "reportPageText")
  
  #fillable = TRUE,
  #fillable_mobile = FALSE,
  #theme = theme, # `scr03_theme.R`
  #lang = NULL,
  #window_title = NA
  )
)


# Server --------------------

server <- function(input, output, session) {
  
  # Observables -----
  
  # Reactives -----
  
  # Outputs -----
  
  output$reportPageText <- renderUI({
    htmltools::p(htmltools::HTML("FOOTER: AZMet data are from "))
  })
}


# Run --------------------

shiny::shinyApp(ui = ui, server = server)
