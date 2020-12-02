PlotImage <- function (inAIObj)
{

  if (class(inAIObj) != "AI") {
    stop("Object must be of class AI.")
  }

  message("Large Auditory Images can delay. Please, wait...")

  Var1 <- NULL; Var2 <- NULL; value <- NULL
  if (names(inAIObj)[1] == "AuditoryNerveImage") {
    AI <- inAIObj$AuditoryNerveImage
    mAI <- reshape2::melt(AI)
    mAI$Var1 <- 1:dim(inAIObj$AuditoryNerveImage)[1]
    mAI$Var2 <- mAI$Var2/100
  } else {
    AI <- inAIObj$PeriodicityPitchImage
    mAI <- reshape2::melt(AI)
    mAI$Var1 <- inAIObj$Periods
    mAI$Var2 <- mAI$Var2/100
  }

    Plot.AI <- ggplot2::ggplot(mAI, ggplot2::aes(x = Var2, y = Var1)) +
    ggplot2::geom_raster(ggplot2::aes(fill=value)) +
    ggplot2::scale_fill_gradient(low="blue", high="gold1") +
    ggplot2::labs(x="Time (in s)", y="Periods (in s)", title="Periodicity Pitch") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x=ggplot2::element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=ggplot2::element_text(size=9),
                       plot.title=ggplot2::element_text(size=11))

  return(Plot.AI)
}
