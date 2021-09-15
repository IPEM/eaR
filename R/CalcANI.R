CalcANI <- function(inSignal,
                    inSampleFreq,
                    inDownsamplingFactor = 4,
                    inNumOfChannels = 40,
                    inFirstCBU = 2.0,
                    inCBUStep = 0.5) {

NewSampleFreq <- 22050
NZeros <- ceiling(0.020/(1/NewSampleFreq))
theZeros <- array(0L, NZeros)

if (class(inSignal) == "character") inSignal <- tuneR::readWave(inSignal);

if (inSampleFreq != 22050) {
  inSignal <- seewave::resamp(inSignal, inSampleFreq, NewSampleFreq,
                        channel = 1, output = "Wave")
  inSignal@left <- c(theZeros, inSignal@left, theZeros)
  inSignal <- tuneR::normalize(inSignal, unit = "16", center = TRUE,
                               level = 1, rescale = TRUE, pcm = inSignal@pcm)
} else {
    if (class(inSignal)[1] == "Wave") {
      inSignal@left <- c(theZeros, inSignal@left, theZeros)
    } else {
  inSignal <- c(theZeros, inSignal, theZeros)
  inSignal <- tuneR::Wave(left = inSignal, samp.rate = inSampleFreq, bit = 16)
  inSignal <- tuneR::normalize(inSignal, unit = "16", center = TRUE,
                               level = 1, rescale = TRUE, pcm = inSignal@pcm)
    }
}

res = ear_process(inSignal@left, inNumOfChannels, inFirstCBU, inCBUStep, 22050)

outANI <- t(res$AuditoryNerveImage)
outANI <- outANI[,(1+ceiling(NZeros/2)):(dim(outANI)[2]-ceiling(NZeros/2))]
outANIFilterFreqs <- 1000*res$ANIFilterFreqs
outANIFreq <- NewSampleFreq/2

if (inDownsamplingFactor != 1) {
  ANIcols <- ceiling((dim(outANI)[2]/inDownsamplingFactor))
  ANI <- matrix(nrow = inNumOfChannels, ncol = ANIcols)

  for (i in 1:inNumOfChannels){
    ANI[i,] <- seewave::resamp(outANI[i,], outANIFreq,
                               outANIFreq/inDownsamplingFactor,
                               channel = 1, output = "matrix")
  }
  outANIFreq <- outANIFreq/inDownsamplingFactor
}

Calc.ANI <- list(ANI, outANIFreq,outANIFilterFreqs)
class(Calc.ANI) <- "AI"
names(Calc.ANI) <- c("AuditoryNerveImage", "ANIFreq","ANIFilterFreqs")
return(Calc.ANI)
}
