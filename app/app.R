# Current conditions and recent data of leaf wetness estimates in Yuma County


# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->
# 'azmet-shiny-template.html': <!-- CSS specific to this AZMet Shiny app -->


# UI --------------------

ui <- htmltools::htmlTemplate(
  filename = "azmet-shiny-template.html",
  
  # Apparent bug in `bslib`, see: https://github.com/rstudio/bslib/issues/834
  #pageNavbar = bslib::page_navbar(
  #navsetTab = bslib::navset_tab(
  #navsetCardTab = bslib::navset_card_tab(
  
  # Work-around by placing the navset in `bslib::page()`, which correctly renders tabs on webpage
  navsetCardTab = bslib::page(
    
    title = NULL,
    theme = theme, # `scr##_theme.R`
    #lang = "en",
    
    bslib::navset_card_tab(
      id = "navsetCardTab",
      selected = "latest-conditions",
      title = NULL,
      sidebar = NULL,
      header = NULL,
      footer = NULL,
      #height = 600,
      full_screen = TRUE,
      wrapper = card_body,
      
      
      # Latest Conditions -----
      
      bslib::nav_panel(
        title = "Latest Conditions",
        
        # shiny::htmlOutput(outputId = "nwsTableTitle"),
        # reactable::reactableOutput(outputId = "nwsTable"),
        # shiny::htmlOutput(outputId = "nwsTableFooter"),
        
        value = "latest-conditions"
      ),
      
      # Past 24 Hours -----
      
      bslib::nav_panel(
        title = "Past 24 Hours",
        
        # bslib::layout_sidebar(
        #   # sidebar = slsSidebar, # `scr##_slsSidebar.R`
        #   
        #   # shiny::htmlOutput(outputId = "slsGraphTitle"),
        #   # plotly::plotlyOutput(outputId = "slsGraph"),
        #   # shiny::htmlOutput(outputId = "slsGraphFooter"),
        #   
        #   #fillable = TRUE,
        #   #fill = TRUE,
        #   #bg = NULL,
        #   #fg = NULL,
        #   #border = NULL,
        #   #border_radius = NULL,
        #   #border_color = NULL,
        #   #padding = NULL,
        #   #gap = NULL,
        #   #height = NULL
        # ),
        
        value = "past-24-hours"
      ),
      
      
      # Past 30 Days -----
      
      bslib::nav_panel(
        title = "Past 30 Days",
        
        # bslib::layout_sidebar(
        #   # sidebar = slsSidebar, # `scr##_slsSidebar.R`
        #   
        #   # shiny::htmlOutput(outputId = "slsGraphTitle"),
        #   # plotly::plotlyOutput(outputId = "slsGraph"),
        #   # shiny::htmlOutput(outputId = "slsGraphFooter"),
        #   
        #   #fillable = TRUE,
        #   #fill = TRUE,
        #   #bg = NULL,
        #   #fg = NULL,
        #   #border = NULL,
        #   #border_radius = NULL,
        #   #border_color = NULL,
        #   #padding = NULL,
        #   #gap = NULL,
        #   #height = NULL
        # ),
        
        value = "past-30-days"
      )
    ) |>
      htmltools::tagAppendAttributes(
        #https://getbootstrap.com/docs/5.0/utilities/api/
        class = "border-0 rounded-0 shadow-none"
      ),

    # htmltools::div(
    #   shiny::uiOutput(outputId = "refreshDataButton"), # Common, regardless of card tab
    #   htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
    #   shiny::uiOutput(outputId = "refreshDataInfo"), # Common, regardless of card tab
    #   
    #   style = "display: flex; align-items: top; gap: 0px;", # Flexbox styling
    # ),
    
    # shiny::htmlOutput(outputId = "downloadButtonsDiv"),
    shiny::htmlOutput(outputId = "pageBottomText") # Common, regardless of card tab
  )
)


# Server --------------------

server <- function(input, output, session) {
  
  # Observables -----
  
  # Reactives -----
  
  # Outputs -----
  
  output$pageBottomText <- shiny::renderUI({
    #shiny::req(dataETL())
    fxn_pageBottomText(activeTab = input$navsetCardTab)
  })
}


# Run --------------------

shiny::shinyApp(ui = ui, server = server)
