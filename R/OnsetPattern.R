OnsetPattern <- function (inSignal, inSampleFreq)
{
  # Parameters
  Dissipation <- 0.4           # dissipation for the neuron dynamics
  Nadj <- 6                    # number of extra adjecent channels that get same input
  Nfb <- 20 #10 #20            # number of channels receiving feedback
  ExtWeight <- 2               # weights for connections between inputs and neurons
  IntWeight <- 0.5             # weights for connections between neurons
  Threshold <- 7 #8 #7         % threshold that must be exceeded by the neuron in order to fire
  RefractoryPeriod <- 0.04     # period after firing during which a neuron is insensitive to input (in s)

  # Other variables
  dT <- 1/inSampleFreq                       # step period (in s)
  N <- nrow(inSignal); M <- ncol(inSignal)   # number of channels and number of samples
  A <- matrix(0, 1, N)                           # accumulator for the neurons
  Cext <- matrix(0, N, N)                    # connections between inputs and neurons
  Cint <- matrix(0, N, N)                    # connections between neurons (feedback, 1 time step delay)
  RefractTime <- matrix(0, 1, N)                 # represents the time still left to spend in refractory period

  # Setting up the external connections
  for (i in 1:N) {
    for (k in pmax(1,i-Nadj/2):pmin(N,i+Nadj/2)) {
      Cext[k,i] <- ExtWeight
    }
  }

  # Setting up the internal connections
  for (i in 1:N) {
    for (k in pmax(1,i-Nfb/2):pmin(N,i+Nfb/2)) {
      if (i != k) {
        Cint[k,i] <- IntWeight
      }
    }
  }

  # Initializing the output
  outOnsetPattern <- matrix(0, N, M)
  outOnsetPatternFreq <- inSampleFreq
  theOutput <- matrix(0, N, 1)

  # Processing the signal
  for (k in 1:M) {

  # Determine sensitivity of neurons to input
  Sensing <- matrix(1, N, 1)                               # normally, all neurons accept input...
  theIndices <- which(RefractTime > 0)                     # but neurons that are in their refractory period...
  Sensing[theIndices] <- 0                                 # don't accept input,
  RefractTime[theIndices] <- RefractTime[theIndices] - dT  # they only keep waiting for the end of the refractory period

  # Neuron dynamics
  A <- (1-Dissipation)*A + (crossprod(inSignal[,k],Cext)
        + crossprod(theOutput[,1],Cint))*Sensing[,1]

  # Detect firing
  theOutput <- matrix(0, N, 1)                # normally, the neurons don't fire...
  theIndices <- which(A > Threshold)          # but neurons that exceed the threshold value...
  theOutput[theIndices] <- 1                  # do fire,
  A[theIndices] <- 0                           # their value is reset to zero,
  RefractTime[theIndices] <- RefractoryPeriod  # and they enter a refractory period

  # Store result
  outOnsetPattern[,k] <- theOutput
  }

  Onset.Pattern <- list(outOnsetPattern, outOnsetPatternFreq)
  names(Onset.Pattern) <- c("OnsetPattern","OnsetPatternFreq")

  return(Onset.Pattern)
}
