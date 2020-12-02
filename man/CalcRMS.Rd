\name{CalcRMS}
\alias{CalcRMS}
\title{Calculate RMS}
\description{This function calculates the running \eqn{RMS} value of the given signal: a \eqn{RMS} value is generated every \code{inFrameInterval} seconds over a period of \code{inFrameWidth} seconds. Thus,  for each frame \eqn{F} in each channel \eqn{\mathbf{c}_{k}(t)} of an auditory nerve image \eqn{\mathbf{A}}, an \eqn{RMS} value is calculated according to:
\deqn{RMS\{F_{i}(\mathbf{c}_{k}(t))\}=\sqrt{\frac{1}{N}\sum_{i=1}^{N}F_{i}^{2}}}
where \eqn{N} is frame width in samples, and \eqn{F_{i}} is the \emph{i}-th sample of frame \eqn{F}.
}
\usage{CalcRMS(inSignal, inSampleFreq, inFrameWidth, inFrameInterval)}
\arguments{
  \item{inSignal}{the input signal (if this is a matrix, \eqn{RMS} values are calculated for each channel (ie. row) in the signal).}
  \item{inSampleFreq}{the sample frequency of the input signal (in Hz).}
  \item{inFrameWidth}{the period over which the \eqn{RMS} is calculated (in s).}
  \item{inFrameInterval}{the period between two successive frames (in s).}}
\value{An object of class "\code{AI}" that contains the running \eqn{RMS} value (\code{RMSSignal}) of the given signal and its sample frequency (\code{RMSFreq}).}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s, 22050)
RMS <- CalcRMS(ANIs$AuditoryNerveImage, ANIs$ANIFreq, 0.029, 0.0058)}
