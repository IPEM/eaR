\name{LeakyIntegration}
\alias{LeakyIntegration}
\title{Leaky Integration}
\description{An echo can be defined as a leaky integration with a given half decay time. At each moment in time, a leaky integrator adds the incoming signal value to an attenuated version of the previously calculated value to form the newly calculated value. The half decay time specifies the time it takes for an impulse signal to reach half its original value.

This function takes an image as input and gives the leaky integrated image as output. Let us suppose that we have a Periodicity Pitch Image \eqn{\mathbf{P}} with a factor of enlargement \eqn{k=0}, then the new echoed image is calculated as
\deqn{\widehat{\mathbf{P}}_{*,j}=\mathbf{P}_{*,j}+\widehat{\mathbf{P}}_{*,j-1}\cdot2^{\frac{-1}{T}} \quad \forall j>1}
where \eqn{T} denotes the echo in seconds and the columns of both matrices are indexed with the subscript \eqn{j}. Leaky integration had been used in Leman (2000) to construct
the pitch images of an echoic memory.}
\usage{LeakyIntegration(inSignal, inSampleFreq, inHalfDecayTime = 0.1,
                 inEnlargement = 0)}
\arguments{
  \item{inSignal}{input signal (multi-dimensional), each row representing a channel leaky integration is performed for each channel.}
  \item{inSampleFreq}{sample frequency of inSignal (in Hz).}
  \item{inHalfDecayTime}{time (in s) at which an impulse would be reduced to half its value. If empty or not specified, 0.1 is used by default.}
  \item{inEnlargement}{time (in s) to enlarge the input signal. If -1, \code{2*inHalfDecayTime} is used. If empty or not specified, 0 is used by default.}}
\details{Sample frequency of \code{outLeakyIntegration} is the same as \code{inSampleFreq}.}
\value{An object of \code{matrix} class representing the leaky integration of the input signal.}
\references{Leman, M. (2000). \href{https://mp.ucpress.edu/content/17/4/481}{An auditory model of the role of short-term memory in probe-tone ratings}. Music Perception, 17(4), 481-509.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{s <- c(SchumannKurioseGeschichte)
ANIs <- CalcANI(s, 22050)
LIs <- LeakyIntegration (ANIs$AuditoryNerveImage, ANIs$ANIFreq, 0.1, -1)}
