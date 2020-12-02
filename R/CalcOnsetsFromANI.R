CalcOnsetsFromANI <- function (inANIObj)
{
  inANI <- inANIObj[[1]]
  inANIFreq <- inANIObj[[2]]

  cat('Calculating RMS signals of ANI...\n')
  Calc.RMS <- CalcRMS(inANI, inANIFreq, 0.029, 0.0058)
  RMS <- Calc.RMS[[1]]
  RMSFreq <- Calc.RMS[[2]]
  rm(inANI)

  cat('Filtering RMS signals...\n')
  theCutOffFreq <- 15 #Hz
  f <- signal::butter(2, theCutOffFreq/(RMSFreq/2))

  theFilteredRMS <- matrix(nrow = nrow(RMS), ncol = ncol(RMS))
  for (i in 1:nrow(RMS)) {
  theFilteredRMS[i,] <- signal::filter(f$b, f$a, RMS[i,])
  }

  IR <- signal::impz(f$b, f$a)

  theDelayInSamples <- which(IR$x == max(IR$x))-1
  theDelay <- theDelayInSamples/RMSFreq

  #PLOT OMITED#

  RMS <- theFilteredRMS[,(theDelayInSamples+1):ncol(theFilteredRMS)]
  rm(theFilteredRMS)

  #Detect relevant peaks in RMS signal of each channel
  cat('Detecting relevant peaks in channels...\n')
  Onset.PeakDetection <- OnsetPeakDetection(RMS, RMSFreq)
  Onsets <- Onset.PeakDetection[[1]]
  OnsetsFreq <- Onset.PeakDetection[[2]]

  # Calculate onset pattern using integrate-and-fire NN
  cat('Calculating onset pattern using integrate-and-fire NN...\n')
  UpsOnsets <- t(signal::resample(t(Onsets), 16, 1))
  UpsOnsetsFreq <- OnsetsFreq*16
  rm(Onsets)
  Onset.Pattern <- OnsetPattern(UpsOnsets, UpsOnsetsFreq)
  rm(UpsOnsets)

  # Filter onset pattern
  cat('Filtering onset pattern...\n')
  OnsetPattern.Filter <- OnsetPatternFilter(Onset.Pattern[[1]], Onset.Pattern[[2]])
  rm(Onset.Pattern)

  # Output the results
  cat('Finished calculating onsets.')
  outOnsetSignal <- OnsetPattern.Filter[[1]]
  outOnsetFreq <- OnsetPattern.Filter[[2]]

  return(list(outOnsetSignal, outOnsetFreq))
}
