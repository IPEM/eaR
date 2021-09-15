LeakyIntegration <- function(inSignal,
                             inSampleFreq,
                             inHalfDecayTime = 0.1,
                             inEnlargement = 0) {
  if (inEnlargement == -1) {
    inEnlargement <- 2*inHalfDecayTime
  }

  if (inHalfDecayTime != 0) {
    integrator <- 2^(-1/(inSampleFreq*inHalfDecayTime))
    } else {
    integrator <- 0
  }

  Matrix <- cbind(inSignal, matrix(0L, nrow = nrow(inSignal),
                                   ncol = round(inSampleFreq*inEnlargement)))
  outLeakyIntegration <- Matrix*0

  outLeakyIntegration[,1] <- Matrix[,1]
  for (i in 2:ncol(Matrix)) {
    outLeakyIntegration[,i] <- (outLeakyIntegration[,i-1]*integrator) + Matrix[,i]
  }

  return(outLeakyIntegration)
}
