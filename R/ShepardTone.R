ShepardTone <- function(inMainFreq,
                        inDuration = 1,
                        inSampleFreq = 22050,
                        inPhaseFlag = 1,
                        indBLevel = NULL) {
  theMinFreq <- 15
  theMaxFreq <- inSampleFreq/2

  N <- round(inDuration*inSampleFreq)
  outSignal <- array(0L, N)
  theTime <- (0:(N-1))/inSampleFreq

  BellCenterFreq <- 1000
  BellMinFreq <- 100
  theFreq <- inMainFreq

  #FUNCTION BellShape
  BellShape <- function(inX, inCenter, inWidth, inPeak) {

    theFactor <- 1/sqrt(-log(0.1))
    outY <- inPeak*exp(-((inX-inCenter)/(inWidth*theFactor))^2)

    return(outY)
  }
  #END FUNCTION BellShape

    while (theFreq > theMinFreq) {
      theAmpl <- BellShape(log(theFreq/BellCenterFreq),0,
                           log(BellCenterFreq/BellMinFreq),1)
      outSignal <- outSignal + theAmpl*sin(2*pi*theFreq*theTime +
                                           inPhaseFlag*stats::runif(1)*pi)
      theFreq <- theFreq/2
    }

  theFreq <- inMainFreq*2
    while (theFreq < theMaxFreq) {
      theAmpl <- BellShape(log(theFreq/BellCenterFreq),0,
                               log(BellCenterFreq/BellMinFreq),1)
      outSignal <- outSignal + theAmpl*sin(2*pi*theFreq*theTime +
                                           inPhaseFlag*stats::runif(1)*pi)
      theFreq <- theFreq*2
    }

    if (!is.null(indBLevel)){
      outSignal <- AdaptLevel(outSignal, indBLevel)
    }

  return(as.numeric(outSignal))
}
