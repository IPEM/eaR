ContextualityIndex <- function(inANIObj,
                               inSnapShot = NULL,
                               inHalfDecayChords = 0.1,
                               inHalfDecayToneCenters = 1.5,
                               inEnlargement = 0) {

  inPeriodicityPitch <- inANIObj[[1]]
  inSampleFreq <- inANIObj[[2]]
  inPeriods <- inANIObj[[3]]

  if (inEnlargement == -1) {
    inEnlargement = 2*inHalfDecayToneCenters;
  }

  if (is.null(inSnapShot)){
    inSnapShot <- (ncol(inPeriodicityPitch)-1)/inSampleFreq
  }

  if (inSnapShot >= 0){
    inSnapShotSamples <- round(inSnapShot*inSampleFreq)+1
  } else {
    inSnapShotSamples <- ncol(inPeriodicityPitch) - abs(round(inSnapShot*inSampleFreq))
  }

  outChords <- LeakyIntegration(inPeriodicityPitch, inSampleFreq,
                                inHalfDecayChords, inEnlargement)

  outToneCenters <- LeakyIntegration(inPeriodicityPitch, inSampleFreq,
                                     inHalfDecayToneCenters, inEnlargement)

  outContextuality1 = array()
  outContextuality2 = array()
  outContextuality3 = array()

  for (i in 1:ncol(outToneCenters)){
    value1 <- stats::cor(outChords[,i], outChords[,inSnapShotSamples], method = "pearson")
    value2 <- stats::cor(outToneCenters[,i], outChords[,inSnapShotSamples], method = "pearson")
    value3 <- stats::cor(outToneCenters[,i], outChords[,i], method = "pearson")
    outContextuality1[i] =  value1
    outContextuality2[i] =  value2
    outContextuality3[i] =  value3
  }

  time <- NULL; coef1 <- NULL; coef2 <- NULL; coef3 <- NULL
  plotcoefs <-data.frame(1:length(outContextuality1)/100, outContextuality1,
                         outContextuality2, outContextuality3)
  colnames(plotcoefs) <- c("time","coef1","coef2","coef3")

  g1 <- ggplot2::ggplot(data = plotcoefs, ggplot2::aes(x=time, y=coef1, group=1)) +
    ggplot2::labs(x = "Time (in s)", y = "Corr. Coef.", title = "Local Inspection")+
    ggplot2::geom_line(color = "steelblue", size = 0.65) +
    ggplot2::ylim(0,1) +
    ggplot2::theme(text =  ggplot2::element_text(size=10))

  g2 <- ggplot2::ggplot(data = plotcoefs, ggplot2::aes(x=time, y=coef2, group=1)) +
    ggplot2::labs(x="Time (in s)", y = "Corr. Coef.", title = "Global Inspection") +
    ggplot2::geom_line(color = "steelblue", size = 0.65) +
    ggplot2::ylim(0,1) +
    ggplot2::theme(text =  ggplot2::element_text(size=10))

  g3 <- ggplot2::ggplot(data = plotcoefs, ggplot2::aes(x=time, y=coef3, group=1)) +
    ggplot2::labs(x="Time (in s)", y = "Corr. Coef.", title = "Comparison") +
    ggplot2::geom_line(color = "steelblue", size = 0.65) +
    ggplot2::ylim(0,1) +
    ggplot2::theme(text =  ggplot2::element_text(size=10))

  coefplot <- cowplot::plot_grid(g1, g2, g3, label_x = 0.2, nrow = 3)

    Contextuality.Index <- list(outToneCenters, outChords,  outContextuality1,
                              outContextuality2, outContextuality3)
    class(Contextuality.Index) <- "AI"
    names(Contextuality.Index) <- c("ChordsImage",
                                    "ToneCentersImage",
                                    "LocalInspection",
                                    "GlobalInspection",
                                    "Comparison")
    return(list(Contextuality.Index, coefplot))
  }
