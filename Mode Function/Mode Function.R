
## MODE INFO ORIGINAL FUNCTION
modeInfo <- function(y) {
  modeData <- unique(y)
  modeData[which.max(tabulate(match(y, modeData)))]
}


modeCalc1 <- function(y){
  modeTbl <- table(y)
  names(modeTbl[which.max(modeTbl)])
}