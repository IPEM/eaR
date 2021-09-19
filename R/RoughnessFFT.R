RoughnessFFT <- function (inObjANI,
                          inFrameWidth = 0.2,
                          inFrameStepSize = 0.02,
                          alpha = 1.6) {
inANI <- inObjANI[[1]]
inANIFreq <- inObjANI[[2]]
inANIFilterFreqs <- inObjANI[[3]]

inLowFrequency <- 5 #Hz
inHighFrequency <- 300 #Hz

FrameWidthInSamples <- round(inFrameWidth*inANIFreq)
FrameStepSize <- round(inFrameStepSize*inANIFreq)
outSampleFreq <- inANIFreq/FrameStepSize

NFFT <- 2^ceiling(log2(abs(FrameWidthInSamples)))

#FUNCTION hamming
hamming <- function (n)
{
  if (n == 1)
    c <- 1
  else {
    n <- n - 1
    c <- 0.54 - 0.46 * cos(2 * pi * (0:n)/n)
  }
  return(c)
}#end FUNCTION hamming

Window <- kronecker(X = matrix(1, nrow(inANI)), Y = t(hamming(FrameWidthInSamples)),
              FUN = "*", make.dimnames = FALSE)

NumberOfUniquePoints = ceiling((NFFT+1)/2)

f <- 0:(NumberOfUniquePoints-1)/NFFT*inANIFreq
Begin <- which(f <= inLowFrequency, arr.ind = TRUE); Begin <- max(Begin)
End <- which(f <= inHighFrequency,  arr.ind = TRUE ); End <- max(End)

NumberOfBins <- End-Begin+1
if (NumberOfBins < 64) {
  warning("Too few fft bins! Increase frame width")
}

ANIRows <- nrow(inANI)

#FUNCTION FilterWeights
FilterWeights <- function (ANIRows, NumberOfBins, inANIFilterFreqs,
                           Begin, End, f) {
  AttenuateWindow <- kronecker(X = matrix(0, ANIRows), Y = t(matrix(1, NumberOfBins)),
                              FUN = "*", make.dimnames = FALSE)

  for (win in 1:ANIRows) {
          Sigmoid = ((win/40)^2 / (.04 + ((win/40)^2.8))-win*.007)

    MaximumHz <- 20 + (52*Sigmoid)
    MaximumInSamples <- round(End*(MaximumHz/300))
    #Sigmoid <- Sigmoid/max(Sigmoid)
    Sigmoid <- ((win/40)^2 / (.04 + ((win/40)^2.45))-win*.007)
    BandwidthHz <- 10 + (300 * Sigmoid )
    #10 + (MaximumBandWidth*(1 - exp(-6*(win-1)/40)))
    BandwidthInSamples <- round(End*(BandwidthHz/310))
    BW <- (1:BandwidthInSamples)

    FilterInSamples <-
      exp((-8*BW)/BandwidthInSamples)*(1-cos(2*pi*BW/(10*BandwidthInSamples)))

    #FilterInSamples = ( (1- cos(2*pi*(1:BandwidthInSamples)/BandwidthInSamples)).^.3 .* logspace(2.0,0,BandwidthInSamples));
    F <- array(0, length(FilterInSamples))
    index <- which(FilterInSamples > 0, arr.ind = TRUE)
    F[index] <- FilterInSamples[index]
    FilterInSamples <- F/max(F)

    MaxFilterInSamples <- which(FilterInSamples == max(FilterInSamples),
                                arr.ind = TRUE)

    NAddZeros <- End - (MaximumInSamples - MaxFilterInSamples
                       + length(FilterInSamples))
    Roughness <- c(array(0, MaximumInSamples - MaxFilterInSamples),
                  FilterInSamples, array(0, abs(NAddZeros)))
    Attenuate <- Roughness[Begin:End]
    AttenuateWindow[win,] <- (Attenuate * 1/(max(Attenuate) - min(Attenuate))) -
                              min(Attenuate)
  }

  return(AttenuateWindow)
}
#end FUNCTION FilterWeights

AttenuateWindow <- FilterWeights(ANIRows,NumberOfBins,
                                 inANIFilterFreqs,Begin,End,f)
#FUNCTION  ChannelWeighting
ChannelWeighting <- function (ANIRows, inANIFilterFreqs, AttenuateWindow)
{
  ChannelWeights <- seq(from = 1, to = .45, length.out = 40)

  return(ChannelWeights)
}
#end FUNCTION  ChannelWeighting

W <- ChannelWeighting(ANIRows,inANIFilterFreqs,AttenuateWindow)
WKron <- kronecker(X = W, Y = t(array(1,NumberOfBins)))

NumOfIterations <- length(seq(from = 1, to = (ncol(inANI)-FrameWidthInSamples+1),
                          by = FrameStepSize ))

outRoughness <- array(0, NumOfIterations)
outFFTMatrix1 <- matrix(0, nrow = nrow(inANI), ncol = NumOfIterations)
outFFTMatrix2 <- matrix(0, nrow = NumberOfBins, ncol = NumOfIterations)

Counter <- 1
for (i in  seq(from = 1, to = (ncol(inANI)-FrameWidthInSamples+1),
               by = FrameStepSize))
  {
  WindowedSignal <- Window * inANI[,i:(i+FrameWidthInSamples - 1)]
  padding <- matrix(0, ncol = (NFFT-ncol(WindowedSignal)),
                      nrow = nrow(WindowedSignal))
  WindowedSignal <- cbind(WindowedSignal, padding)

  FFT <-  WindowedSignal*0
  for (i in 1:nrow(WindowedSignal)) {
    FFT[i,] <- stats::fft(WindowedSignal[i,], inverse = TRUE)
  }

  A <- abs(FFT)
  DC <- kronecker(as.matrix(A[,1]) , t(array(1,NumberOfBins)))
  Weightings <- WKron * AttenuateWindow / DC

  EnergyOverChannels <-  sqrt(rowSums(Weightings * (A[,Begin:End]^alpha))/NFFT/FrameWidthInSamples)
  EnergyOverFrequencies <- sqrt(colSums(Weightings * (A[,Begin:End]^alpha))/NFFT/FrameWidthInSamples)

  outFFTMatrix1[,Counter] <- EnergyOverChannels
  outFFTMatrix2[,Counter] <- EnergyOverFrequencies
  outRoughness[Counter] <- sum(EnergyOverChannels)/ANIRows

  #TEXT PROGRESS

  Counter = Counter + 1
}

time <- NULL; roughness <- NULL
plotoutRoughness <- data.frame(1:nrow(outRoughness)/outSampleFreq, outRoughness)
colnames(plotoutRoughness) <- c("time","roughness")

g1 <- ggplot2::ggplot(data = plotoutRoughness, ggplot2::aes(x = time, y = roughness*1000, group = 1)) +
  ggplot2::labs(x = "Time (in s)", y = "Roughness") +
  ggplot2::geom_line(color = "steelblue", size = 0.65) +
  ggplot2::theme(text = ggplot2::element_text(size=10))

Roughness.FFT <- list(outFFTMatrix1, outFFTMatrix2, outRoughness, g1)
names(Roughness.FFT) <- c("FFT Matrix 1",
                          "FFT Matrix 2",
                          "Roughness",
                          "PlotRoughness")

return(Roughness.FFT)
}
