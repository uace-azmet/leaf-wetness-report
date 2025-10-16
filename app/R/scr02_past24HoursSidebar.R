past24HoursSidebar <- 
  bslib::sidebar(
    width = 300,
    position = "left",
    open = list(desktop = "open", mobile = "always-above"),
    id = "sidebar",
    title = NULL,
    bg = "#FFFFFF",
    fg = "#191919", # https://www.color-hex.com/color-palette/1041718
    class = NULL,
    max_height_mobile = NULL,
    gap = NULL,
    padding = NULL,
    
    htmltools::p(
      bsicons::bs_icon("sliders"), 
      htmltools::HTML("&nbsp;"), 
      "DATA DISPLAY",
      htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
      bslib::tooltip(
        bsicons::bs_icon("info-circle"),
        "Specify an AZMet station to highlight in the graphs.",
        id = "infoDataOptions",
        placement = "right"
      ),
      
      class = "data-display-title"
    ),
    
    shiny::selectInput(
      inputId = "azmetStation",
      label = "AZMet Station",
      choices = azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name,
      selected = azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name[1]
    )
    
    # shiny::selectInput(
    #   inputId = "stationVariable", 
    #   label = "Station Variable",
    #   choices = NULL, # see `app.R`, shiny::updateSelectInput(inputId = "stationVariable")
    #   selected = NULL # see `app.R`, shiny::updateSelectInput(inputId = "stationVariable")
    # )
  ) # bslib::sidebar()
