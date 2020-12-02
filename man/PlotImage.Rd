\name{PlotImage}
\alias{PlotImage}
\title{Plot an Auditory Image}
\description{Auditory images reflect features of the sound as internal representations, in other words as brain activations so to speak. This function display Auditory Images from \code{\link{CalcANI}} (Auditory Nerve Image) and \code{\link{PeriodicityPitch}} (Pitch Image) or an "\code{AI}" data object (future versions).}
\usage{PlotImage(inAIObj)}
\arguments{
  \item{inAIObj}{A object of class "\code{AI}" which contains the matrix to be represented}}
\details{This function uses package \pkg{ggplot2} for representations.}
\value{'done'}
\author{Marc Vidal.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s, 22050)
PPs <- PeriodicityPitch(ANIs)
PlotImage(PPs)}
