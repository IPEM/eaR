\name{AdaptLevel}
\alias{AdaptLevel}
\title{Adapt RMS Power Level}
\description{This function adapts the Root Mean Square (RMS) power level of the signal \eqn{s(t)} to the specified dB level. The RMS is calculated in the time domain for the sampled signal as:
\deqn{RMS\{s(t)\}=\sqrt{\frac{1}{N}\sum_{t}s^{2}(t)}}
where \eqn{N} is the sample size. To calculate the factor to adapt the signal,
\deqn{A=10*\log_{10}(\mathrm{P}/RMS)}
where \eqn{RMS} is the reference to which \eqn{P} is being compared.}
\usage{AdaptLevel(inSignal, indB)}
\arguments{
  \item{inSignal}{a numeric vector representing the input signal.}
  \item{indB}{a scalar indicating the wanted level (0 is maximum level without clipping,  -20 is reasonable).}}
\details{The reference value of 0 dB is the level of a square wave with amplitude 1. Thus, a sine wave with amplitude 1 yields -3.01 dB.}
\value{A numeric vector representing the adapted input signal.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\note{The multi-channel version of \code{AdaptLevel} is deprecated and only is available in \pkg{IPEM} Toolbox.}
\examples{AdaptLevel(SchumannKurioseGeschichte, -20)}
