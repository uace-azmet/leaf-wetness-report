# Libraries --------------------


library(azmetr)
library(bslib)
library(dataui)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(lubridate)
library(plotly)
library(reactable)
library(reactablefmtr)
library(reshape2)
library(shiny)
library(shinyjs)


# Files --------------------


# Functions. Loaded automatically at app start if in `R` folder
#source("./R/fxn_functionName.R", local = TRUE)

# Scripts. Loaded automatically at app start if in `R` folder
#source("./R/scr_scriptName.R", local = TRUE)

shiny::addResourcePath("shinyjs", system.file("srcjs", package = "shinyjs"))


# Variables --------------------


azmetStationPast24Hours <- shiny::reactiveVal(value = "Roll")
azmetStationPast30Days <- shiny::reactiveVal(value = "Roll")

azmetStationMetadata <- azmetr::station_info |>
  dplyr::filter(
    meta_station_name %in% c(
      "Roll", 
      "Wellton ETo", 
      "Yuma N.Gila", 
      "Yuma South", 
      "Yuma Valley"
    )
  )

maxMeanMV <- 400
minMeanMV <- 200
rangeMeanMV <- maxMeanMV - minMeanMV
thresholdMeanMVDry <- 273#245#273
thresholdMeanMVWet <- 284#260#284
thresholdTempAir <- 84#32 # for `latestConditionsTable` warning cell color
