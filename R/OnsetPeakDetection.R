OnsetPeakDetection <- function (inSignal, inSampleFreq)
{
  outOnsetResults <- 0*inSignal
  outOnsetResultsFreq <- inSampleFreq

  for (i in 1:nrow(inSignal)) {
    theInput <- inSignal[i,]
    OPD1C <- OnsetPeakDetection1Channel(theInput, inSampleFreq)
    outOnsetResults[i, OPD1C[[1]]] <- OPD1C[[2]]
  }

  Onset.PeakDetection <- list(outOnsetResults, outOnsetResultsFreq)
  names(Onset.PeakDetection) <- c("OnsetResults", "OnsetResultsFreq")

  return(Onset.PeakDetection)
}
