AdaptLevel <- function(inSignal, indB)
{
  theRMS <- sqrt(sum(inSignal^2)/length(inSignal))
  theFactor <- 10^(indB/20 - log10(theRMS))
  outSignaldB <- inSignal*theFactor

  return(as.numeric(outSignaldB))
}
