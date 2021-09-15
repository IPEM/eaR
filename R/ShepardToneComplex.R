ShepardToneComplex <- function (inToneVector,
                                inDuration = 1,
                                inSampleFreq = 22050,
                                inPhaseFlag = 1,
                                indBLevel = NULL) {
  theNoteFreqs <- c(261.6, 277.2, 293.7, 311.1, 329.6, 349.2, 370.0, 392.0, 415.3,
                    440.0, 466.2, 493.9)

  outSignal <- array(0, round(inDuration*inSampleFreq))
  for (i in 1:12) {
    if (inToneVector[i] != 0) {
      outSignal <- outSignal + ShepardTone(theNoteFreqs[i], inDuration,
                                          inSampleFreq, inPhaseFlag)
    }
  }

  if (!is.null(indBLevel)) {
    outSignal = AdaptLevel(outSignal, indBLevel)
  }

return(as.numeric(outSignal))
}
