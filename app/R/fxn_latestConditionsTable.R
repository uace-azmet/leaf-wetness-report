#' `fxn_latestConditionsTable.R` - Build latest conditions table
#' 
#' @param inData - Most recent 15-minute leaf wetness data from `fxn_latestConditionsData.R`
#' @return `latestConditionsTable` - Latest conditions table, reactable format


fxn_latestConditionsTable <- function(inData) {
  
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
          #na = "NA",
          rowHeader = FALSE,
          #minWidth = 150,
          #maxWidth = NULL,
          width = 100,
          #align = NULL,
          vAlign = "center",
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
          #minWidth = 180,
          #na = "NA",
          rowHeader = TRUE,
          vAlign = "center",
          width = 170
        ),
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
          style = reactablefmtr::color_scales(
            data = .,
            color_ref = "temp_air_color",
            opacity = 1.0
          ),
          format = reactable::colFormat(digits = 1),
          html = TRUE,
          #na = "NA",
          rowHeader = TRUE,
          align = "right",
          vAlign = "center",
          width = 100
        ),
        dwpt_30cm_meanF = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "T<sub>dew point</sub><sup>", 
                tags$span(style = "font-weight: normal", "1,3"),
                "</sup><br>", 
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(°F)")
              )
            ),
          style =
            reactablefmtr::color_scales(
              data = .,
              color_ref = "dwpt_color",
              opacity = 1.0
            ),
          format = reactable::colFormat(digits = 1),
          html = TRUE,
          #na = "NA",
          rowHeader = TRUE,
          align = "right",
          vAlign = "center",
          width = 100
        ),
        lw_sensor = reactable::colDef(
          name = "Sensor",
          html = TRUE,
          na = "NA",
          rowHeader = TRUE,
          align = "left",
          vAlign = "center",
          width = 90
        ),
        row_number = reactable::colDef(
          name = "Row Number",
          cell = 
            function(value) {
              plotly::plot_ly( # Depicts range of 200-400 mV
                data = .[value, ],
                x = 1,
                y = ~as.factor(row_number),
                marker = list(color = "#eeeeee"),
                name = "mV range",
                showlegend = FALSE,
                hoverinfo = "none",
                hovertext = "none",
                type = "bar",
                orientation = "h",
                height = 24
              ) %>%
                
                plotly::add_trace( # Depicts `mean_mV` values
                  inherit = TRUE,
                  x = ~mean_mV_adj,
                  marker = list(color = "#c9c9c9"),
                  name = "mV value"
                ) %>% 
                
                plotly::config(
                  displaylogo = FALSE,
                  displayModeBar = FALSE
                ) %>%
                
                plotly::layout(
                  annotations = list(
                    list( # Dry zone
                      align = "center",
                      showarrow = FALSE,
                      text = "DRY",
                      x = (((273 - 200) / 2) / 200),
                      xanchor = "center",
                      xref = "x",
                      xshift = 0,
                      y = 0.5,
                      yanchor = "center",
                      yref = "paper"
                    ),
                    list( # Transition zone
                      align = "center",
                      showarrow = FALSE,
                      text = "T",
                      x = (((284 - ((284 - 273) / 2)) - 200) / 200),
                      xanchor = "center",
                      xref = "x",
                      xshift = 0,
                      y = 0.5,
                      yanchor = "center",
                      yref = "paper"
                    ),
                    list( # Wet zone
                      align = "center",
                      showarrow = FALSE,
                      text = "WET",
                      x = (((400 - ((400 - 284) / 2)) - 200) / 200),
                      xanchor = "center",
                      xref = "x",
                      xshift = 0,
                      y = 0.5,
                      yanchor = "center",
                      yref = "paper"
                    )
                  ),
                  barmode = "overlay",
                  font = list(
                    color = "#191919",
                    family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
                    size = 12
                  ),
                  margin = list(
                    l = 0,
                    r = 0,
                    b = 0,
                    t = 0,
                    pad = 0
                  ),
                  shapes = 
                    list(
                      list( # Lower transition threshold
                        type = "line",
                        x0 = ((273 - 200) / 200),
                        x1 = ((273 - 200) / 200),
                        y0 = 0,
                        y1 = 1,
                        yref = "paper",
                        line = list(
                          color = "#191919",
                          dash = "solid",
                          width = 1
                        ),
                        layer = "above"
                      ),
                      list( # Upper transition threshold
                        type = "line",
                        x0 = ((284 - 200) / 200),
                        x1 = ((284 - 200) / 200),
                        y0 = 0,
                        y1 = 1,
                        yref = "paper",
                        line = list(
                          ccolor = "#191919",
                          dash = "solid",
                          width = 1
                        ),
                        layer = "above"
                      )
                    ),
                  xaxis = list(
                    fixedrange = TRUE,
                    visible = FALSE
                  ),
                  yaxis = list(
                    fixedrange = TRUE,
                    visible = FALSE
                  )
                )
            },
          html = TRUE,
          na = "NA",
          rowHeader = TRUE,
          align = "left",
          vAlign = "center",
          width = 256
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
          html = TRUE,
          na = "NA",
          rowHeader = TRUE,
          align = "right",
          vAlign = "center",
          width = 100
        ),
        temp_air_color = reactable::colDef(show = FALSE),
        dwpt_color = reactable::colDef(show = FALSE),
        mean_mV_adj = reactable::colDef(show = FALSE)
      ),
      #columnGroups = NULL,
      rownames = FALSE,
      groupBy = NULL,
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
          borderColor = NULL,# "#dee2e6",
          borderWidth = NULL, #"0.5px",
          stripedColor = NULL,
          highlightColor = NULL,
          cellPadding = NULL,
          style = NULL,
          tableStyle = NULL,
          headerStyle = 
            list(
              color = "#191919",
              fontFamily = "monospace",
              fontSize = "0.8rem"
              # borderBottomColor = rgb(180/255, 180/255, 180/255, 1.0),
              # borderBottomWidth = "1px",
              # boxShadow = "0px 1px 0px 0px #e3e3e3"
            ),
          groupHeaderStyle = NULL,
          tableBodyStyle = NULL,
          rowGroupStyle = NULL,
          rowStyle = 
            reactablefmtr::group_border_sort( # but, see https://stackoverflow.com/questions/66946229/insert-borders-underneath-selected-rows-in-reactable-r
              columns = "meta_station_name",
              border_color = "red"
            ),
          # htmlwidgets::JS(
          #   paste0(
          #     "function(rowInfo) {",
          #       "if (rowInfo.index % 2 === 0) {", # Check if the row index is even (0-indexed)
          #         "return { borderBottom: '1px solid #191919' }", # Apply a bottom border to even rows
          #       "}",
          #     "}"
          #   )
          # ),
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
