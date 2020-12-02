OnsetPeakDetection1Channel <- function(inSignal, inSampleFreq)
{
  #Based on the function Peak4 in IPEMOnsetPeakDetection1Channel (see IPEM Toolbox)
  WindowSize1InSeconds <- 0.07
  WindowSize2InSeconds <- 0.5
  N <- length(inSignal)

  WindowSize1 <- round(WindowSize1InSeconds*inSampleFreq)
  HalfWidth1 <- WindowSize1/2
  WindowSize2 <- round(WindowSize2InSeconds*inSampleFreq)

  inSignal <- Clip(inSignal, inLowLimit = 0.06, inClipLowTo = 0.05)

  #signal::medfilt1 is deprecated.
  theFilteredSignal <- stats::runmed(inSignal, round(WindowSize2/3)*3, "constant")

  theDifference <- inSignal - theFilteredSignal

  thePeakIndices <- FindAllPeaks(inSignal, "center")

  #removed iteration if

    for (i in 1:length(thePeakIndices)) {
      theIndex <- thePeakIndices[i]
      theIndices <- FindNearestMinima(inSignal, theIndex)
      if (inSignal[theIndices[[1]]] > 0.9*inSignal[theIndex]) {
        thePeakIndices[i] <- 0
      }
    }

  outPeakIndices <- thePeakIndices[which(thePeakIndices > 0)]
  rm(thePeakIndices)

  thePeaks <- array(0, N)
  thePeaks[outPeakIndices] <- inSignal[outPeakIndices]

  theTime <- seq(0, (N-1)/inSampleFreq, 1/inSampleFreq)

  #FUNCTION CreateMask
  CreateMask <- function (inTime, inStartTimes, inAmplitudes,
                          inDecayPeriod, inFractionAtDecayPeriod = 0.5)
  {
    #FUNCTION OneMask
    OneMask <-function (inTime, inStartTime, inAmplitude,
                        inDecayPeriod, inFractionAtDecayPeriod)
    {
      Alpha <- -log(inFractionAtDecayPeriod)

      outY <- array(0, length(inTime))
      theMasked <- which(inTime >= inStartTime)
      if (!is.null(theMasked)) {
        outY[theMasked] <- inAmplitude*exp(-(inTime[theMasked]-inStartTime)/inDecayPeriod*Alpha)
      }
      return(outY)
    }
    # end FUNCTION OneMask

    outMask <- array(0, length(inTime))
    if (inDecayPeriod <= 0) {
      return(outMask)
    } else {

      for (i in 1:length(inStartTimes)) {
        theMask <- OneMask(inTime, inStartTimes[i], inAmplitudes[i],
                           inDecayPeriod, inFractionAtDecayPeriod)
        theIndices <- which(theMask > outMask)
        outMask[theIndices] <- theMask[theIndices]
      }
      return(outMask)
    }
  }

  # end FUNCTION CreateMask

  theMaskSignal <- CreateMask(theTime, outPeakIndices/inSampleFreq,
                                  thePeaks[outPeakIndices], 0.2)
  theMaskedIndices <- which(theMaskSignal > thePeaks)
  thePeaks[theMaskedIndices] <- 0
  outPeakIndices <- which(thePeaks > 0)

  for (i in 1:length(outPeakIndices)) {

    theIndex <- outPeakIndices[i]

    Left <- pmax(1,theIndex-HalfWidth1)
    Right <- pmin(N,theIndex+HalfWidth1)
    theMax <- max(thePeaks[Left:Right])

      if (thePeaks[theIndex] < theMax) {
        thePeaks[theIndex] <- 0
      } else {

        if(1) {
          # condition to make sure that the first block of code gets executed.
          theMedianValue <- theFilteredSignal[theIndex]

          if (thePeaks[theIndex] < 1.2*theMedianValue) {
            thePeaks[theIndex] <- 0
          } else {
            thePeaks[theIndex] <- theDifference[theIndex]/theMedianValue;
          }
        } else if (0) {

          theIndices <- FindNearestMinima(inSignal,theIndex)
          theLeftMinimumValue <- inSignal[theIndices[[1]]]

            if (thePeaks[theIndex] < 1.2*theLeftMinimumValue) {
              thePeaks[theIndex] <- 0
            } else {
                thePeaks[theIndex] <- (inSignal[theIndex]-theLeftMinimumValue)/theLeftMinimumValue
            }
        } else {

          theMedianValue <- theFilteredSignal[theIndex]
          theIndices <- FindNearestMinima(inSignal,theIndex)
          theLeftMinimumValue <- inSignal[theIndices[[1]]]

            if (thePeaks[theIndex] < 1.2*mean(c(theMedianValue, theLeftMinimumValue))) {
              thePeaks[theIndex] <- 0
            } else {
            thePeaks[theIndex] <- theDifference[theIndex]/theMedianValue
            }
        }
      } #end first if...
  } #end for...

  outPeakIndices <- which(thePeaks > 0) #first get the indices,
  outImportances <- thePeaks[outPeakIndices] #then get the importances...
  outImportances <- 2*outImportances #raise with a factor
  outImportances <- Clip(outImportances,0,1) #clip between 0 and 1

  OnsetPeakDetection.1Channel <- list(outPeakIndices, outImportances)
  names(OnsetPeakDetection.1Channel)  <- c("PeakIndices", "Importances")

  return(OnsetPeakDetection.1Channel)
}
