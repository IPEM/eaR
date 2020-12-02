CalcANI <- function(inSignal, inSampleFreq, inDownsamplingFactor = 4,
                    inNumOfChannels = 40, inFirstCBU = 2, inCBUStep = 0.5)
{
  cat('Start of IPEM::CalcANI...\n')

  NewSampleFreq <- 22050
  NZeros <- ceiling(0.020/(1/NewSampleFreq))
  theZeros <- array(0L, NZeros)

  if (class(inSignal) == "character") {
    inSignal <- tuneR::readWave(inSignal)
  }

     if (inSampleFreq != 22050) {
        inSignal <- seewave::resamp(inSignal, inSampleFreq, NewSampleFreq,
                              channel = 1, output = "Wave")
        inSignal@left <- c(theZeros,inSignal@left,theZeros)
        inSignal <- tuneR::normalize(inSignal, unit = "16", center = TRUE,
                                     level = 1, rescale = TRUE, pcm = inSignal@pcm)
    } else {
          if (class(inSignal)[1] == "Wave"){
            inSignal@left <- c(theZeros, inSignal@left, theZeros)
          } else {
        inSignal <- c(theZeros, inSignal, theZeros)
        inSignal <- tuneR::Wave(left = inSignal, samp.rate = inSampleFreq,
                                bit = 16)
        inSignal <- tuneR::normalize(inSignal, unit = "16", center = TRUE,
                                     level = 1, rescale = TRUE, pcm = inSignal@pcm)
          }
        }

  OldPath <- getwd()
  inAuditoryModelPath <- paste(.libPaths(),"/IPEM/Auditory_Model/", sep="")
  setwd(inAuditoryModelPath)

  tuneR::writeWave(inSignal, "input.wav", extensible = FALSE)

  os <- Sys.info()["sysname"]
  if (os == "Windows"){
    #inAuditoryModelPath <- shortPathName(gsub("/", "\\\\", inAuditoryModelPath))
    system("./ASTMwin", show.output.on.console = FALSE)
  } else {
    system(paste(inAuditoryModelPath, "ASTMunix", sep = ""))
  }

  nerve_image.ani <- "e8n00bin"
  outANI <- scan(nerve_image.ani, quote = "", sep = "", dec =".", quiet = TRUE)
  outANI <- matrix(outANI,inNumOfChannels,length(outANI)/inNumOfChannels)
  outANIFilterFreqs <- utils::read.delim("FilterFrequencies.txt",
                                  header = FALSE, sep = "" )
  outANIFilterFreqs <- 1000*outANIFilterFreqs
  outANIFreq <- NewSampleFreq/2

  outANI<-outANI[,(1+ceiling(NZeros/2)):(dim(outANI)[2]-ceiling(NZeros/2))]

  file.remove("decim.dat")
  file.remove("eef.dat")
  file.remove("FilterFrequencies.txt")
  file.remove("filters.dat")
  file.remove("input.wav")
  file.remove("lpf.dat")
  file.remove("e8n00bin")
  file.remove("omef.dat")
  file.remove("outfile.dat")

  cat('Ended dll, ready for downsampling if needed...\n')

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

  setwd(OldPath)

  cat('...end of IPEM::CalcANI.')
  return(Calc.ANI)

  }
