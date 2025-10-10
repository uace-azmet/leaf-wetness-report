# Libraries --------------------

library(azmetr)
library(bslib)
library(dataui)
library(dplyr)
library(htmltools)
library(htmlwidgets)
# library(lubridate)
library(plotly)
library(reactable)
library(reactablefmtr)
library(reshape2)
library(shiny)
library(shinyjs)
# library(vroom)


# Files --------------------

# Functions. Loaded automatically at app start if in `R` folder
#source("./R/fxn_functionName.R", local = TRUE)

# Scripts. Loaded automatically at app start if in `R` folder
#source("./R/scr_scriptName.R", local = TRUE)

shiny::addResourcePath("shinyjs", system.file("srcjs", package = "shinyjs"))


# Variables --------------------

maxMeanMV <- 400
minMeanMV <- 200
rangeMeanMV <- 200
thresholdMeanMVDry <- 250#273
thresholdMeanMVWet <- 265#284
thresholdTempAir <- 82#32 # for `latestConditionsTable` warning cell color
