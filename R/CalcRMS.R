CalcRMS <- function (inSignal,
                     inSampleFreq,
                     inFrameWidth,
                     inFrameInterval) {

  M <- ncol(inSignal)
  N <- nrow(inSignal)

  theStep <- round(inFrameInterval/(1/inSampleFreq))
  theWidth <- round(inFrameWidth/(1/inSampleFreq))

  outRMSSignal <- matrix(0, nrow = N, ncol = ceiling((M-theWidth+1)/theStep))

  k <- 1
  for (i in seq(1, M-theWidth+1, theStep)) {
       outRMSSignal[,k] <- sqrt(rowSums(inSignal[,i:(i+theWidth-1)]^2)/theWidth)
       k <- k + 1
  }

  outRMSFreq <- inSampleFreq/theStep

  Calc.RMS <- list(outRMSSignal, outRMSFreq)
  class(Calc.RMS) <- "AI"
  names(Calc.RMS) <- c("RMSSignal", "RMSFreq")

  return(Calc.RMS)
}
