PeriodicityPitch <- function(inANIObj,
                             inLowFrequency = 80,
                             inFrameWidth = 0.064,
                             inFrameStepSize = 0.010) {
  inMatrix <- inANIObj[[1]]
  inSampleFreq <- inANIObj[[2]]

  HalfSampleFreq <- inSampleFreq/2
  FrameWidth <- round(inFrameWidth * inSampleFreq)
  FrameWidth2 <- FrameWidth*2
  FrameStepSize <- round(inFrameStepSize * inSampleFreq)
  outSampleFreq <- inSampleFreq/FrameStepSize
  outPeriods <- seq(0, (FrameWidth-1)/inSampleFreq, 1/inSampleFreq)

  f <- signal::butter(2, inLowFrequency/HalfSampleFreq)

  fANI <- inMatrix*0

  for (i in 1:nrow(inMatrix)){
    fANI[i,] <- signal::filter(f$b, f$a, inMatrix[i,])
  }

  IR <- signal::impz(f$b, f$a)

  theMax <- max(IR$x)
  theMaxIndex <- match(max(IR$x),IR$x)
  theDelay <- theMaxIndex-1

  FANI <- inMatrix[,1:(ncol(inMatrix)-theDelay)] - fANI[,(theDelay+1):ncol(fANI)]
  FANI[FANI < 0] <- 0

  ACOR <- matrix(0, nrow = FrameWidth,
                 ncol = length(seq(1, ncol(FANI)-FrameWidth2, FrameStepSize))+1)

  theZeroes <- array(0L, FrameWidth)
  counter <- 1
  for (i in seq(1, (ncol(FANI)-FrameWidth2+1), FrameStepSize)) {
    SumAutoCorr <- array(0L, FrameWidth)

    for (j in 1:nrow(FANI)){
      AutoCorr <- stats::convolve(c(theZeroes, FANI[j,i:(i+FrameWidth-1)]),
                                  FANI[j,i:(i+FrameWidth2-1)])
      SumAutoCorr <- SumAutoCorr + AutoCorr[(FrameWidth+1):(FrameWidth2)]
    }

    ACOR[,counter] <- rev(SumAutoCorr)
    counter <- counter + 1
  }

  Periodicity.Pitch <- list(ACOR, outSampleFreq,  outPeriods, FANI)
  class(Periodicity.Pitch ) <- "AI"
  names(Periodicity.Pitch) <- c("PeriodicityPitchImage", "SampleFreq",
                                "Periods","BPANI")
  return(Periodicity.Pitch)
}
