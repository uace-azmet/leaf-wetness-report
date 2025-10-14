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
        
        shiny::htmlOutput(outputId = "latestConditionsTitle"),
        reactable::reactableOutput(outputId = "latestConditionsTable"),
        shiny::htmlOutput(outputId = "latestConditionsTableFooter"),
        
        value = "latest-conditions"
      ),
      
      # Past 24 Hours -----
      
      bslib::nav_panel(
        title = "Past 24 Hours",
        
        bslib::layout_sidebar(
          sidebar = past24HoursSidebar, # `scr##_past24HoursSidebar.R`
        #   
            shiny::htmlOutput(outputId = "past24HoursTitle"),
        #   # plotly::plotlyOutput(outputId = "slsGraph"),
            shiny::htmlOutput(outputId = "past24HoursGraphFooter"),
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
        ),
        
        value = "past-24-hours"
      ),
      
      
      # Past 30 Days -----
      
      bslib::nav_panel(
        title = "Past 30 Days",
        
        bslib::layout_sidebar(
        #   # sidebar = slsSidebar, # `scr##_slsSidebar.R`
        #   
            shiny::htmlOutput(outputId = "past30DaysTitle"),
        #   # plotly::plotlyOutput(outputId = "slsGraph"),
            shiny::htmlOutput(outputId = "past30DaysGraphFooter"),
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
        ),
        
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
    htmltools::div(
      shiny::uiOutput(outputId = "refreshDataButton"), # Common, regardless of card tab
      htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
      shiny::uiOutput(outputId = "refreshDataInfo"), # Common, regardless of card tab
      
      style = "display: flex; align-items: top; gap: 0px;", # Flexbox styling
    ),
    
    shiny::htmlOutput(outputId = "pageBottomText") # Common, regardless of card tab
  )
)


# Server --------------------

server <- function(input, output, session) {
  shinyjs::useShinyjs(html = TRUE)
  shinyjs::hideElement("pageBottomText")
  shinyjs::hideElement("refreshDataButton") # Needs to be 'present' on page for `dataETL <- shiny::reactive({})` statement to work on initial page load
  shinyjs::hideElement("refreshDataInfo")
  
  
  # Observables -----
  
  shiny::observeEvent(lw15min(), {
    shinyjs::showElement("pageBottomText")
    shinyjs::showElement("refreshDataButton")
    shinyjs::showElement("refreshDataInfo")
  })
  
  
  # Reactives -----
  
  lw15min <- shiny::reactive({
    fxn_lw15min()
  }) %>% 
    shiny::bindEvent(
      input$refreshDataButton,
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
  
  # Filter and format latest conditions data for the most recent report from each station
  latestConditionsData <- shiny::eventReactive(lw15min(), {
    fxn_latestConditionsData(inData = lw15min())
  })
  
  
  # Outputs -----
  
  output$latestConditionsTable <- reactable::renderReactable({
    fxn_latestConditionsTable(inData = latestConditionsData())
  })
  
  output$latestConditionsTableFooter <- shiny::renderUI({
    shiny::req(lw15min())
    fxn_latestConditionsTableFooter()
  })
  
  output$latestConditionsTitle <- shiny::renderUI({
    shiny::req(lw15min())
    fxn_latestConditionsTitle()
  })
  
  output$pageBottomText <- shiny::renderUI({
    #shiny::req(dataETL())
    fxn_pageBottomText(activeTab = input$navsetCardTab)
  })
  
  output$past24HoursGraphFooter <- shiny::renderUI({
    shiny::req(lw15min())
    fxn_past24HoursGraphFooter()
  })
  
  output$past24HoursTitle <- shiny::renderUI({
    shiny::req(lw15min()) # Need to change to `lwdaily()`
    fxn_past24HoursTitle()
  })
  
  output$past30DaysGraphFooter <- shiny::renderUI({
    shiny::req(lw15min())
    fxn_past30DaysGraphFooter()
  })
  
  output$past30DaysTitle <- shiny::renderUI({
    shiny::req(lw15min()) # Need to change to `lwdaily()`
    fxn_past30DaysTitle()
  })
  
  output$refreshDataButton <- shiny::renderUI({
    #shiny::req(dataETL())
    shiny::actionButton(
      inputId = "refreshDataButton", 
      label = "REFRESH DATA",
      icon = shiny::icon(name = "rotate-right", lib = "font-awesome"),
      class = "btn btn-block btn-blue"
    )
  })
  
  output$refreshDataInfo <- shiny::renderUI({
    #req(dataETL())
    bslib::tooltip(
      bsicons::bs_icon("info-circle"),
      "Click or tap to refresh the above table with the latest leaf wetness conditions.",
      id = "refreshDataInfo",
      placement = "right"
    )
  })
}


# Run --------------------

shiny::shinyApp(ui = ui, server = server)
