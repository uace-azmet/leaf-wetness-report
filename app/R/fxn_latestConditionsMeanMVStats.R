#' `fxn_latestConditionsMeanMVStats.R` - `mean_mV` maxmimum, minimum, and range for scaling horizontal bar graphs
#' 
#' @param inData - 15-minute leaf wetness data from `fxn_lw15min.R`
#' @return `latestConditionsMeanMVStats` - List of `mean_mV` maxmimum, minimum, and range for scaling horizontal bar graphs


fxn_latestConditionsMeanMVStats <- function(inData) {
  
  maxMeanMV <- maxMeanMVInit # See `_global.R`
  minMeanMV <- minMeanMVInit
  rangeMeanMV <- rangeMeanMVInit
  
  maxMeanMVObs <- 
    max(
      c(
        max(inData$lw1_mean_mV, na.rm = TRUE), 
        max(inData$lw2_mean_mV, na.rm = TRUE)
      ), 
      na.rm = TRUE
    )
  
  minMeanMVObs <- 
    min(
      c(
        min(inData$lw1_mean_mV, na.rm = TRUE), 
        min(inData$lw2_mean_mV, na.rm = TRUE)
      ), 
      na.rm = TRUE
    )
  
  if (maxMeanMVObs > maxMeanMV) {
    maxMeanMV <- maxMeanMVObs
    rangeMeanMV <- maxMeanMV - minMeanMV
  }
  
  if (minMeanMVObs < minMeanMV) {
    minMeanMV <- minMeanMVObs
    rangeMeanMV <- maxMeanMV - minMeanMV
  }
  
  latestConditionsMeanMVStats <- 
    list(
      maxMeanMV,
      minMeanMV,
      rangeMeanMV
    )
  
  return(latestConditionsMeanMVStats)
}
