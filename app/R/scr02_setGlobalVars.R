#lwStartDate <- as.Date("2024-05-22") # Daily data only

#if (
#  lubridate::now(tzone = "America/Phoenix") > 
#  lubridate::as_datetime(
#    paste(
#      lubridate::today(tzone = "America/Phoenix"), 
#      "01:30:00", sep = " "
#    ),
#    tz = "America/Phoenix"
#  )
#) {
#  initialInputDate <- lubridate::today(tzone = "America/Phoenix")
#} else {
#  initialInputDate <- lubridate::today(tzone = "America/Phoenix") - 1
#}

#timeSteps <- c("Hourly", "Daily")
