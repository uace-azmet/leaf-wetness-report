# Libraries --------------------


library(azmetr)
library(bslib)
library(dplyr)
library(htmltools)
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

maxMeanMVInit <- 400
minMeanMVInit <- 200
rangeMeanMVInit <- maxMeanMVInit - minMeanMVInit
thresholdMeanMVDry <- 273
thresholdMeanMVWet <- 284
thresholdTempAir <- 32.0 # for `latestConditionsTable` warning cell color
