# Current conditions and recent data of leaf wetness estimates in Yuma County

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->
# 'azmet-shiny-template.html': <!-- CSS specific to this AZMet Shiny app -->

# Libraries
#library(azmetr)
library(bslib)
library(dplyr)
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
  
  pageNavbar = bslib::page_navbar(
    bslib::nav_panel(
      #p("First tab content."), 
      title = "Current Conditions",
      #bslib::layout_columns(
        cardsCurrentConditions[[1]], 
        cardsCurrentConditions[[2]], 
        cardsCurrentConditions[[3]], 
        cardsCurrentConditions[[4]], 
        cardsCurrentConditions[[5]]
      #)
    ),
    bslib::nav_panel(title = "Recent Data", p("Second tab content.")),
    bslib::nav_panel(title = "Data Variables", p("Third tab content."))
    
    # `scr0x_navsetTab.R`
  )#,
  
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


# Server --------------------

server <- function(input, output, session) {
  
  # Observables -----
  
  # Reactives -----
  
  # Outputs -----
  
  output$reportPageText <- renderUI({
    htmltools::p(htmltools::HTML("AZMet data are from "))
  })
}


# Run --------------------

shiny::shinyApp(ui = ui, server = server)
