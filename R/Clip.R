Clip <- function (inSignal,
                  inLowLimit = NULL,
                  inHighLimit = NULL,
                  inClipLowTo = NULL,
                  inClipHighTo = NULL) {
  outSignal <- inSignal
  if (!is.null(inLowLimit)) {
    theIndices <- which(inSignal < inLowLimit)
    if (!is.null(inClipLowTo)) {
      outSignal[theIndices] <- inClipLowTo
      } else {
    outSignal[theIndices] <- inLowLimit
      }
  }

  if (!is.null(inHighLimit)) {
    theIndices <- which(inSignal > inHighLimit)
    if (!is.null(inClipHighTo)) {
      outSignal[theIndices] <- inClipHighTo
      } else {
    outSignal[theIndices] <- inHighLimit
      }
  }
  return(outSignal)
}
