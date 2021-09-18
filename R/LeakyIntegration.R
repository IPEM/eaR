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

  #Fits a new matrix for the enlargement
  Ae <- cbind(inSignal,
                  matrix(0L, nrow = nrow(inSignal),
                         ncol = round(inSampleFreq*inEnlargement)))

  outLeakyIntegration <- Ae*0 #deposit results

  outLeakyIntegration[,1] <- Ae[,1]
  for (i in 2:ncol(Ae)) {
    outLeakyIntegration[,i] <- (outLeakyIntegration[,i-1]*integrator) + Ae[,i]
    print( outLeakyIntegration[,i])
  }

  return(outLeakyIntegration)
}
