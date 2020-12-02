\name{FindAllPeaks}
\alias{FindAllPeaks}
\title{Find All Peaks in a Signal}
\description{This function finds the indices of all peaks in the given signal vector. A peak is taken to occur whenever a rising part in the signal is followed by a falling part.}
\usage{FindAllPeaks(inSignal, inFlatPreference = "center")}
\arguments{
  \item{inSignal}{the signal that is scanned for peaks (a \code{numeric} vector).}
  \item{inFlatPreference}{preference for choosing exact position in case of flat peaks:
    \itemize{
      \item{if "left", the leftmost point of a flat peak is chosen}
      \item{if "center", the center of a flat peak is chosen}
      \item{if "right", the rightmost point of a flat peak is chosen.}}
If empty or not specified, "center" is used by default.}}
\value{A numeric vector  with the indices of the peaks (could be empty!).}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{data(SchumannKurioseGeschichte)
Ps <- FindAllPeaks(SchumannKurioseGeschichte)}
