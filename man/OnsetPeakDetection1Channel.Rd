\name{OnsetPeakDetection1Channel}
\alias{OnsetPeakDetection1Channel}
\title{Onset Peak Detection in One Channel}
\description{Returns the indices of the "important" peaks in the input signal, together with an indication of the "importance" of the peak.}
\usage{OnsetPeakDetection1Channel(inSignal, inSampleFreq)}
\arguments{
  \item{inSignal}{the signal to be scanned for peaks.}
  \item{inSampleFreq}{the sample frequency of the signal (in Hz).}}
\value{A list of two arrays:
  \item{PeakIndices}{the indices within the input signal where the peaks are found.}
  \item{Importances}{a value between 0 and 1 showing the "importance" of a peak.}}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{data(SchumannKurioseGeschichte)
OPD1C <- OnsetPeakDetection1Channel(SchumannKurioseGeschichte, 22050)}
