\name{AdaptLevel}
\alias{AdaptLevel}
\title{Adapt \code{RMS} Power Level}
\description{This function adapts the root mean square (\code{RMS}) power level of the signal to the specified dB level (\code{indB}). For a sampled signal \eqn{s_i\ (i=1,\ldots,n)}, the \code{RMS} is defined as
\deqn{\code{RMS}\{s_i\}=\sqrt{\frac{1}{n}\sum_{i=1}^n s_i^{2}.}}
The factor to adapt the signal is calculated as
\eqn{\code{factor}=10\times\log_{10}(\code{indB}/\code{RMS})}.}
\usage{AdaptLevel(inSignal, indB)}
\arguments{
  \item{inSignal}{a numeric vector representing the input signal.}
  \item{indB}{a scalar indicating the wanted level (0 is maximum level without clipping,  -20 is reasonable).}}
\details{The reference value of 0 dB is the level of a square wave with amplitude 1. Thus, a sine wave with amplitude 1 yields -3.01 dB.}
\value{A numeric vector representing the adapted input signal.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\note{The multi-channel version of \code{AdaptLevel} is deprecated and only is available in \pkg{IPEM} Toolbox.}
\examples{AdaptLevel(SchumannKurioseGeschichte, -20)}
