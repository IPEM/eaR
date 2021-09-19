\name{OnsetPeakDetection}
\alias{OnsetPeakDetection}
\title{Onset Peak Detection}
\description{Returns a pattern having a non-zero value on moments of possible onset peaks.}
\usage{OnsetPeakDetection(inSignal, inSampleFreq)}
\arguments{
  \item{inSignal}{a matrix of size  \emph{n} x \emph{m} where \emph{n} is the length of time observations and \emph{m} is the number of channels.}
  \item{inSampleFreq}{the sample frequency of the incoming signal (in Hz).}}
\value{A list of two arrays:
  \item{OnsetResults}{a matrix of size \emph{n} x \emph{m} with a value between 0 and 1 (non-zero where an onset occurs, and proportional to the importance of the onset).}
  \item{OnsetResultsFreq}{sample frequency of the result (same as \code{inSampleFreq}).}}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s[1:22050], 22050)
OPD <- OnsetPeakDetection(ANIs$AuditoryNerveImage, ANIs$ANIFreq)}
