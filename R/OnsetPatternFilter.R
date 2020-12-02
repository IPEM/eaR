OnsetPatternFilter <- function(inOnsetPattern, inSampleFreq)
{
  # The parameters
  theFrameWidth <- 0.03          # frame within which a certain number of onsets must occur (s)
  theMinNumOnsetsFactor <- 9/40  # minimum fraction of channels that have an onset within the frame
  theMinDistance <- 0.04        # minimum distance between onsets

  # Conversion to samples
  W <- round(theFrameWidth*inSampleFreq)
  D <- round(theMinDistance*inSampleFreq)

  # Get number of channels and pattern length and init output
  outOnsetSignal <- array(0, ncol(inOnsetPattern))
  outOnsetSignalFreq <- inSampleFreq

  # Process pattern
  i <- 1
  while (i < ncol(inOnsetPattern)) {

  # Look for an onset in any channel
    if (max(inOnsetPattern[,i]) != 0) {

  # Get the total number of onsets within this frame
      theCount <- sum(inOnsetPattern[,i:pmin(ncol(inOnsetPattern), i+W-1)])

  # Decide whether we accept this to be an onset or not
      if ((theCount/nrow(inOnsetPattern)) >= theMinNumOnsetsFactor) {
        outOnsetSignal[i] <- theCount/nrow(inOnsetPattern)
        i = i + D
      } else {
        i = i + 1
      }
  # Otherwise, just proceed in time
    } else {
    i = i + 1
    }
  } # ...of while loop

  Onset.PatternFilter <- list(outOnsetSignal,  outOnsetSignalFreq)
  names(Onset.PatternFilter) <- c("OnsetSignal","OnsetSignalFreq")

  return(Onset.PatternFilter)
}
