# Libraries --------------------

library(azmetr)
library(bslib)
library(dplyr)
library(htmltools)
# library(lubridate)
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
