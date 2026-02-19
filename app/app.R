# The latest conditions and recent data of leaf wetness estimates from stations in Yuma County


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
            
          shiny::htmlOutput(outputId = "past24HoursTitle"),
          shiny::htmlOutput(outputId = "past24HoursLatestDataUpdate"),
          shiny::htmlOutput(outputId = "past24HoursCardLayout"),
          shiny::htmlOutput(outputId = "past24HoursCardLayoutFooter"),
             
          #fillable = TRUE,
          #fill = TRUE,
          #bg = NULL,
          #fg = NULL,
          #border = NULL,
          #border_radius = NULL,
          #border_color = NULL,
          #padding = NULL,
          #gap = NULL,
          #height = NULL
        ),
        
        value = "past-24-hours"
      ),
      
      
      # Past 30 Days -----
      
      bslib::nav_panel(
        title = "Past 30 Days",
        
        bslib::layout_sidebar(
          sidebar = past30DaysSidebar, # `scr##_past30DaysSidebar.R`
          
          shiny::htmlOutput(outputId = "past30DaysTitle"),
          shiny::htmlOutput(outputId = "past30DaysLatestDataUpdate"),
          shiny::htmlOutput(outputId = "past30DaysCardLayout"),
          shiny::htmlOutput(outputId = "past30DaysCardLayoutFooter"),

          #fillable = TRUE,
          #fill = TRUE,
          #bg = NULL,
          #fg = NULL,
          #border = NULL,
          #border_radius = NULL,
          #border_color = NULL,
          #padding = NULL,
          #gap = NULL,
          #height = NULL
        ),
        
        value = "past-30-days"
      )
    ) |>
      htmltools::tagAppendAttributes(
        #https://getbootstrap.com/docs/5.0/utilities/api/
        class = "border-0 rounded-0 shadow-none"
      ),

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
  shinyjs::hideElement("refreshDataButton")
  shinyjs::hideElement("refreshDataInfo")
  
  
  # Observables -----
  
  shiny::observeEvent(lw15min(), {
    shinyjs::showElement("pageBottomText")
    shinyjs::showElement("refreshDataButton")
    shinyjs::showElement("refreshDataInfo")
  })
  
  shiny::observeEvent(input$azmetStationPast30Days, { # Keep selected station between tabs
    azmetStationPast24Hours(input$azmetStationPast30Days) # See `_global.R`
    shiny::updateSelectInput(
      inputId = "azmetStationPast24Hours",
      label = "AZMet Station",
      # choices = sort(unique(azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name)),
      selected = azmetStationPast24Hours() # See `_global.R`
    )
  },
  
  ignoreInit = TRUE
  )

 shiny::observeEvent(input$azmetStationPast24Hours, { # Keep selected station between tabs
    azmetStationPast30Days(input$azmetStationPast24Hours) # See `_global.R`
    shiny::updateSelectInput(
      inputId = "azmetStationPast30Days",
      label = "AZMet Station",
      # choices = sort(unique(azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name)),
      selected = azmetStationPast30Days() # See `_global.R`
    )
  },
  
  ignoreInit = TRUE
  )
  
  
  # Reactives -----
  
  latestConditionsData <- shiny::eventReactive(lw15min(), {
    fxn_latestConditionsData(
      inData = lw15min(),
      meanMVStats = latestConditionsMeanMVStats()
    )
  })
  
  # latestConditionsMeanMVStats <- shiny::eventReactive(lw15min(), {
  #   fxn_latestConditionsMeanMVStats(inData = lw15min())
  # })
  
  latestConditionsMeanMVStats <- shiny::reactive({
    fxn_latestConditionsMeanMVStats(inData = lw15min())
  }) %>% 
    shiny::bindEvent(lw15min())
  
  lw15min <- shiny::reactive({
    fxn_lw15min()
  }) %>% 
    shiny::bindEvent(
      input$refreshDataButton,
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
  
  lwdaily <- shiny::eventReactive(lw15min(), {
    fxn_lwdaily()
  })
  
  past24HoursCardGraphs <- shiny::eventReactive(
    c(input$azmetStationPast24Hours, lw15min()), {
      fxn_past24HoursCardGraphs(
        azmetStation = input$azmetStationPast24Hours,
        inData = lw15min()
      )
    }
  )
  
  past24HoursCardLayout <- shiny::eventReactive(
    c(input$azmetStationPast24Hours, lw15min(), past24HoursCardGraphs()), {
      fxn_past24HoursCardLayout(
        azmetStation = input$azmetStationPast24Hours,
        inData = lw15min(),
        past24HoursCardGraphs = past24HoursCardGraphs()
      )
    }
  )
  
  past30DaysCardGraphs <- shiny::eventReactive(
    c(input$azmetStationPast30Days, lwdaily()), {
      fxn_past30DaysCardGraphs(
        azmetStation = input$azmetStationPast30Days,
        inData = lwdaily()
      )
    }
  )
  
  past30DaysCardLayout <- shiny::eventReactive(
    c(input$azmetStationPast30Days, lwdaily(), past30DaysCardGraphs()), {
      fxn_past30DaysCardLayout(
        azmetStation = input$azmetStationPast30Days,
        inData = lwdaily(),
        past30DaysCardGraphs = past30DaysCardGraphs()
      )
    }
  )
  
  
  # Outputs -----
  
  output$latestConditionsTable <- reactable::renderReactable({
    fxn_latestConditionsTable(
      inData = latestConditionsData(),
      meanMVStats = latestConditionsMeanMVStats()
    )
  })
  
  output$latestConditionsTableFooter <- shiny::renderUI({
    shiny::req(latestConditionsData())
    fxn_latestConditionsTableFooter()
  })
  
  output$latestConditionsTitle <- shiny::renderUI({
    shiny::req(latestConditionsData())
    fxn_latestConditionsTitle()
  })
  
  output$pageBottomText <- shiny::renderUI({
    #shiny::req(lw15min())
    fxn_pageBottomText(activeTab = input$navsetCardTab)
  })
  
  output$past24HoursCardLayout <- shiny::renderUI({
    bslib::layout_column_wrap(
      !!!past24HoursCardLayout(),
      #class = ,
      fill = TRUE,
      fillable = TRUE,
      fixed_width = FALSE,
      #gap = "200px",
      #height = "200px",
      heights_equal = c("all", "row"),
      height_mobile = NULL,
      max_height = NULL,
      min_height = NULL,
      width = "300px"
    )
  })
  
  output$past24HoursCardLayoutFooter <- shiny::renderUI({
    shiny::req(lw15min())
    fxn_past24HoursCardLayoutFooter()
  })
  
  output$past24HoursLatestDataUpdate <- shiny::renderUI({
    # shiny::req(lw15min())
    fxn_past24HoursLatestDataUpdate(
      azmetStation = input$azmetStationPast24Hours,
      inData = lw15min()
    )
  })
  
  output$past24HoursTitle <- shiny::renderUI({
    shiny::req(lw15min())
    fxn_past24HoursTitle()
  })
  
  output$past30DaysCardLayout <- shiny::renderUI({
    bslib::layout_column_wrap(
      !!!past30DaysCardLayout(),
      #class = ,
      fill = TRUE,
      fillable = TRUE,
      fixed_width = FALSE,
      #gap = "200px",
      #height = "200px",
      heights_equal = c("all", "row"),
      height_mobile = NULL,
      max_height = NULL,
      min_height = NULL,
      width = "300px"
    )
  })
  
  output$past30DaysCardLayoutFooter <- shiny::renderUI({
    shiny::req(lwdaily())
    fxn_past30DaysCardLayoutFooter()
  })
  
  output$past30DaysLatestDataUpdate <- shiny::renderUI({
    # shiny::req(lwdaily())
    fxn_past30DaysLatestDataUpdate(
      azmetStation = input$azmetStationPast30Days,
      inData = lwdaily()
    )
  })
  
  output$past30DaysTitle <- shiny::renderUI({
    shiny::req(lwdaily())
    fxn_past30DaysTitle()
  })
  
  output$refreshDataButton <- shiny::renderUI({
    #shiny::req(lw15min())
    shiny::actionButton(
      inputId = "refreshDataButton", 
      label = "REFRESH DATA",
      icon = shiny::icon(name = "rotate-right", lib = "font-awesome"),
      class = "btn btn-block btn-blue"
    )
  })
  
  output$refreshDataInfo <- shiny::renderUI({
    #req(lw15min())
    activeTab = input$navsetCardTab
    if (activeTab == "latest-conditions") {
      bslib::tooltip(
        bsicons::bs_icon("info-circle"),
        "Click or tap to refresh the above table with the latest leaf wetness conditions.",
        id = "refreshDataInfo",
        placement = "right"
      )
    } else {
      bslib::tooltip(
        bsicons::bs_icon("info-circle"),
        "Click or tap to refresh the above graphs with the latest leaf wetness conditions.",
        id = "refreshDataInfo",
        placement = "right"
      )
    }
  })
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
