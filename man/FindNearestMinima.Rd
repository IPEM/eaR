\name{FindNearestMinima}
\alias{FindNearestMinima}
\title{Find Nearest Minima}
\description{Finds the nearest minima to the left and the right of the specified peak. Peaks can be found with \code{FindAllPeaks}.}
\usage{FindNearestMinima(inSignal, inPeakIndex)}
\arguments{
  \item{inSignal}{the index of the nearest miminum to the left of the peak if no real minimum was found, 1 is returned.}
  \item{inPeakIndex}{the index of the nearest miminum to the right of the peak. If no real minimum was found, \code{length(inSignal)} is returned.}}
\value{A list of two elements:
\item{LeftIndex}{the index of the nearest miminum to the left of the peak. If no real minimum was found, 1 is returned.}
\item{RightIndex}{the index of the nearest miminum to the right of the peak. If no real minimum was found, \code{length(inSignal)} is returned.}}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
