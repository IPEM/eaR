\name{Clip}
\alias{Clip}
\title{Clip a Multi-channel Signal}
\description{This function clips the incoming (multi-channel) signal at the given limits. Specify empty values for either one of the limits if you don’t want clipping at that side of the signal range. Signal channels are represented by rows.}
\usage{Clip(inSignal, inLowLimit = NULL, inHighLimit = NULL,
     inClipLowTo = NULL, inClipHighTo = NULL)}
\arguments{
  \item{inSignal}{a matrix representing the (multi-channel) signal to be clipped.}
  \item{inLowLimit}{specifies the low level clipping value: values lower than this are replaced by either the limit itself or \code{inClipLowTo}. If empty or not specified, no clipping occurs.}
  \item{inHighLimit}{specifies the high level clipping value: values higher than this are replaced by either the limit itself or \code{inClipHighTo}. If empty or not specified, no clipping occurs.}
  \item{inClipLowTo}{if non-empty, this is a replacement value for too low values. If empty or not specified, \code{inLowLimit} is used.}
  \item{inClipHighTo}{if non-empty, this is a replacement value for too high values. If empty or not specified, \code{inHighLimit} is used.}}
\value{A matrix representing the clipped signal.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{s <- matrix(runif(64), ncol=8)
sc <- Clip(s, 0.5 ,0.7, 0, 1)}
