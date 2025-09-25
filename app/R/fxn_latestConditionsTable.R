#' `fxn_latestConditionsTable.R` - Build latest conditions table
#' 
#' @param inData - Most recent 15-minute leaf wetness data from `fxn_latestConditionsData.R`
#' @return `latestConditionsTable` - Latest conditions table, reactable format


fxn_latestConditionsTable <- function(inData) {
  inData <- inData %>% 
    dplyr::mutate(
      temp_air_color = dplyr::case_when(
        temp_air_30cm_meanF <= 100 ~ "#EF4056",
        TRUE ~ "#FFFFFF"
      )# ,
      # dwpt_color = dplyr::case_when(
      #   dwpt_30cm_meanF <= 50 ~ "#EF4056",
      #   TRUE ~ "#FFFFFF"
      # )
    )
  
  latestConditionsTable <- inData %>% 
    reactable::reactable(
      columns = list(
        meta_station_name = reactable::colDef(
          name = "Station",
          #aggregate = NULL,
          #sortable = NULL,
          #resizable = NULL,
          #filterable = NULL,
          #searchable = NULL,
          #filterMethod = NULL,
          #show = TRUE,
          #defaultSortOrder = NULL,
          #sortNALast = FALSE,
          #format = NULL,
          #cell = NULL,
          #grouped = NULL,
          #aggregated = NULL,
          #header = NULL,
          #footer = NULL,
          #details = NULL,
          #filterInput = NULL,
          html = TRUE,
          na = "NA",
          rowHeader = FALSE,
          minWidth = 150,
          #maxWidth = NULL,
          #width = NULL,
          #align = NULL,
          #vAlign = NULL,
          #headerVAlign = NULL,
          sticky = "left",
          class = "table-reactable-column-station",
          style = list(
            borderRight = "1px solid #989898",
            boxShadow = "1px 0px 0px 0px #e3e3e3"
          ),
          #headerClass = NULL,
          headerStyle = list(
            borderRight = "1px solid #989898",
            boxShadow = "1px 1px 0px 0px #e3e3e3"
          ),
          #footerClass = NULL,
          #footerStyle = NULL
        ), 
        datetime = reactable::colDef(
          name = "Latest Update",
          html = TRUE,
          minWidth = 180,
          na = "NA",
          rowHeader = TRUE
        ),
        # lw1_mean_mV = reactable::colDef(
        #   name = 
        #     htmltools::HTML(
        #       paste0(
        #         "lw1<sup>", 
        #         tags$span(style = "font-weight: normal", "1"),
        #         "</sup><br>", 
        #         tags$span(style = "font-weight: normal; font-size: 0.8rem", "(mV)")
        #       )
        #     ),
        #   format = reactable::colFormat(digits = 0),
        #   html = TRUE,
        #   na = "NA",
        #   rowHeader = TRUE
        # ),
        # lw2_mean_mV = reactable::colDef(
        #   name = 
        #     htmltools::HTML(
        #       paste0(
        #         "lw2<sup>", 
        #         tags$span(style = "font-weight: normal", "1"),
        #         "</sup><br>", 
        #         tags$span(style = "font-weight: normal; font-size: 0.8rem", "(mV)")
        #       )
        #     ),
        #   format = reactable::colFormat(digits = 0),
        #   html = TRUE,
        #   na = "NA",
        #   rowHeader = TRUE
        # ),
        temp_air_30cm_meanF = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "T<sup>", 
                tags$span(style = "font-weight: normal", "1,2"),
                "</sup><br>", 
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(°F)")
              )
            ),
          cell = 
            reactablefmtr::color_tiles( # https://kcuilla.github.io/reactablefmtr/reference/color_tiles.html
              data = .,
              color_ref = "temp_air_color",
              opacity = 1
            ),
          format = reactable::colFormat(digits = 1),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        dwpt_30cm_meanF = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "T<sub>dew point</sub><sup>", 
                tags$span(style = "font-weight: normal", "1"),
                "</sup><br>", 
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(°F)")
              )
            ),
          # cell = 
          #   reactablefmtr::color_tiles( # https://kcuilla.github.io/reactablefmtr/reference/color_tiles.html
          #     data = .,
          #     color_ref = "dwpt_color",
          #     opacity = 1
          #   ),
          format = reactable::colFormat(digits = 1),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        mean_mV = reactable::colDef(
          name =
            htmltools::HTML(
              paste0(
                "mean_mV<sup>",
                tags$span(style = "font-weight: normal", "1"),
                "</sup><br>",
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(mV)")
              )
            ),
          format = reactable::colFormat(digits = 0),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        lwSensor = reactable::colDef(
          name = "Leaf Wetness Sensor",
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        temp_air_color = reactable::colDef(show = FALSE)# ,
        # dwpt_color = reactable::colDef(show = FALSE)
      ),
      #columnGroups = NULL,
      rownames = FALSE,
      #groupBy = NULL,
      sortable = FALSE,
      resizable = FALSE,
      filterable = FALSE,
      searchable = FALSE,
      searchMethod = NULL,
      #defaultColDef = NULL,
      #defaultColGroup = NULL,
      #defaultSortOrder = "asc",
      #defaultSorted = NULL,
      pagination = FALSE,
      #defaultPageSize = 10,
      showPageSizeOptions = FALSE,
      #pageSizeOptions = c(10, 25, 50, 100),
      #paginationType = "numbers",
      showPagination = NULL,
      showPageInfo = FALSE,
      #minRows = 1,
      #paginateSubRows = FALSE,
      #details = NULL,
      defaultExpanded = TRUE,
      selection = NULL,
      defaultSelected = NULL,
      onClick = NULL,
      highlight = FALSE,
      outlined = FALSE,
      bordered = FALSE,
      borderless = FALSE,
      striped = FALSE,
      compact = TRUE,
      #wrap = TRUE,
      #showSortIcon = TRUE,
      #showSortable = FALSE,
      class = "latest-conditions-table",
      #style = NULL,
      #rowClass = NULL,
      #rowStyle = NULL,
      fullWidth = TRUE,
      width = "auto",
      height = 400,
      theme = 
        reactable::reactableTheme(
          color = NULL,
          backgroundColor = "#FFFFFF",
          borderColor = "#dee2e6",
          borderWidth = "0.5px",
          stripedColor = NULL,
          highlightColor = NULL,
          cellPadding = NULL,
          style = NULL,
          tableStyle = NULL,
          headerStyle = 
            list(
              color = "#191919", 
              fontFamily = "monospace", 
              fontSize = "0.8rem",
              borderBottomColor = rgb(180/255, 180/255, 180/255, 1.0),
              borderBottomWidth = "1px",
              boxShadow = "0px 1px 0px 0px #e3e3e3"
            ),
          groupHeaderStyle = NULL,
          tableBodyStyle = NULL,
          rowGroupStyle = NULL,
          rowStyle = NULL,
          rowStripedStyle = NULL,
          rowHighlightStyle = NULL,
          rowSelectedStyle = NULL,
          cellStyle = list(color = "#191919", fontFamily = "monospace", fontSize = "0.8rem"),
          footerStyle = NULL,
          inputStyle = NULL,
          filterInputStyle = NULL,
          searchInputStyle = NULL,
          selectStyle = NULL,
          paginationStyle = NULL,
          pageButtonStyle = NULL,
          pageButtonHoverStyle = NULL,
          pageButtonActiveStyle = NULL,
          pageButtonCurrentStyle = NULL
        ),
      #language = getOption("reactable.language"),
      #meta = NULL,
      #elementId = NULL,
      #static = getOption("reactable.static", FALSE),
      #selectionId = NULL
    )
  
  return(latestConditionsTable)
}
