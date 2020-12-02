FindNearestMinima <- function (inSignal, inPeakIndex)
{
  N <- length(inSignal)

  thePreviousValue <- inSignal[inPeakIndex]
  outLeftIndex <- 0
  for (i in seq((inPeakIndex[1]-1), 1, -1)) {
    if (inSignal[i] > thePreviousValue) {
      outLeftIndex <- i+1
      break
    } else {
      thePreviousValue <- inSignal[i]
      outLeftIndex <- i
    }
  }

  thePreviousValue <- inSignal[inPeakIndex]
  outRightIndex <- 0
  for (i in (inPeakIndex[1]+1):N) {
    if (inSignal[i] > thePreviousValue) {
      outRightIndex <- i-1
      break
    } else {
    thePreviousValue <- inSignal[i]
    outRightIndex <- i
    }
  }

  Find.NearestMinima <- list(outLeftIndex, outRightIndex)
  names(Find.NearestMinima) <- c("LeftIndex", "RightIndex")

  return(Find.NearestMinima)
}
