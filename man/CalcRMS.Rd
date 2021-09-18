\name{CalcRMS}
\alias{CalcRMS}
\title{Calculate \code{RMS}}
\description{This function calculates the running \code{RMS} value of multi-channel signal as
\deqn{\code{RMS}\{F(A)\}=w^{-1}\left(\sum_{i=\tau_{1}}^{\tau_1 +w}\cdots\sum_{i=\tau_{l}}^{\tau_l+w}A_{ij}^{2}\right)^{1/2}\quad (j=1,\ldots,m),}
where \eqn{F} is a function that divides the signal in \eqn{l}-equispaced time frames of length \eqn{w} and \eqn{A=(A_{i1},\ldots,A_{im})^\top} is a \eqn{m} dimensional vector representing the multi-channel signal. As a result, we obtain a \eqn{l \times m} vector  with the \code{RMS} values for each time frame and channel. The \code{RMS} value is generated every \code{inFrameInterval} seconds over a period \eqn{\tau} of \code{inFrameWidth} seconds.
}
\usage{CalcRMS(inSignal, inSampleFreq, inFrameWidth, inFrameInterval)}
\arguments{
  \item{inSignal}{the input signal. If this is a matrix, \code{RMS} values are calculated for each channel.}
  \item{inSampleFreq}{the sample frequency of the input signal (in Hz).}
  \item{inFrameWidth}{the period over which the \code{RMS} is calculated (in s).}
  \item{inFrameInterval}{the period between two successive frames (in s).}}
\value{An object of class "\code{AI}" that contains the running \code{RMS} value (\code{RMSSignal}) of the given signal and its sample frequency (\code{RMSFreq}).}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s, 22050)
RMS <- CalcRMS(ANIs$AuditoryNerveImage, ANIs$ANIFreq, 0.029, 0.0058)}
