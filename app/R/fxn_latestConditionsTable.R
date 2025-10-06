#' `fxn_latestConditionsTable.R` - Build latest conditions table
#' 
#' @param inData - Most recent 15-minute leaf wetness data from `fxn_latestConditionsData.R`
#' @return `latestConditionsTable` - Latest conditions table, reactable format


fxn_latestConditionsTable <- function(inData) {
  inData <- inData %>% 
    # https://stackoverflow.com/questions/78275267/add-a-second-groupname-col-in-gt-table-without-concatenating-the-column-valu
    dplyr::mutate(
      meta_station_name = factor(meta_station_name, levels = unique(meta_station_name))
    ) %>% 
    # dplyr::arrange(meta_station_name, datetime) %>% 
    dplyr::mutate(
      datetime = ifelse(dplyr::row_number() == 1, datetime, ""),
      temp_air_30cm_meanF = ifelse(dplyr::row_number() == 1, temp_air_30cm_meanF, NA),
      dwpt_30cm_meanF = ifelse(dplyr::row_number() == 1, dwpt_30cm_meanF, NA), 
      .by = meta_station_name
    ) %>% 
    dplyr::mutate(
      temp_air_color = dplyr::case_when(
        temp_air_30cm_meanF <= 100 ~ "#f19e1f",
        TRUE ~ "#FFFFFF"
      ) ,
      dwpt_color = dplyr::case_when(
        dwpt_30cm_meanF >= temp_air_30cm_meanF ~ "#EF4056",
        TRUE ~ "#FFFFFF"
      )
    )
  
  
  # lw15min |> # https://stackoverflow.com/questions/78275267/add-a-second-groupname-col-in-gt-table-without-concatenating-the-column-valu
  #   dplyr::mutate(meta_station_name = factor(meta_station_name, levels = unique(meta_station_name))) |>
  #   dplyr::arrange(meta_station_name, datetime) |>
  #   dplyr::mutate(
  #     datetime = ifelse(dplyr::row_number() == 1, as.character(datetime), ""), .by = c(meta_station_name, datetime)
  #   )
  
  
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
          #minWidth = 180,
          #na = "NA",
          rowHeader = TRUE,
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
          width = 100
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
          # cell =
          #   reactablefmtr::data_bars(
          #     data = .,
          #     text_position = "outside-end",
          #     min_value = 200,
          #     max_value = 400
          #   ),
          cell = function(value) {
            fxn_latestConditionsTableBarGraph(
              #inData = .,
              value = value
            )
            # horizontalBarPlot <-# insert plotly horizontal bar chart into reactable cell
            #   plotly::plot_ly(
            #     data = .,
            #     x = ~mean_mV,
            #     y = ~graph_count,
            #     type = "bar",
            #     orientation = "h"
            #   ) %>%
            #   plotly::layout(
            #     #margin = list(l = 50, r = 10, b = 10, t = 10),
            #     xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
            #     yaxis = list(title = "", showgrid = FALSE),
            #     autosize = TRUE
            #   ) %>%
            #   plotly::config(displayModeBar = FALSE) # Hide the Plotly toolbar
            # 
            # #htmltools::tagList(horizontalBarPlot)
          },
          format = reactable::colFormat(digits = 0),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE,
          align = "left",
          minWidth = 160,
          # style = function() { # https://stackoverflow.com/questions/36598624/vertical-line-inside-td-using-a-div
          #   dryThreshold <- ((273 - 200) / 200) * 100
          #   wetThreshold <- ((284 - 200) / 200) * 100
          #   
          #   list(
          #     background = paste0(
          #       "linear-gradient(to right, transparent ", dryThreshold, "%, #191919 ", dryThreshold, "%, #191919 ", wetThreshold, "%, transparent ", wetThreshold, "%)"
          #     ),
          #     background_repeat = "no-repeat",
          #     background_size = "100% 100%"
          #   )
          # }
        ),
        lw_sensor = reactable::colDef(
          name = "Sensor Status",
          html = TRUE,
          na = "NA",
          rowHeader = TRUE,
          align = "left",
          width = 120
        ),
        temp_air_color = reactable::colDef(show = FALSE),
        dwpt_color = reactable::colDef(show = FALSE)
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
