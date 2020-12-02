\name{ShepardTone}
\alias{ShepardTone}
\title{Shepard Tone}
\description{This function generates a Shepard tone.}
\usage{ShepardTone(inMainFreq, inDuration = 1, inSampleFreq = 22050,
            inPhaseFlag = 1, indBLevel = NULL)}
\arguments{
  \item{inMainFreq}{the main frequency (in Hz).}
  \item{inDuration}{the duration (in s). If empty or not specified, 1 is used by default.}
  \item{inSampleFreq}{the desired sample frequency for the output signal (in Hz). If empty or not specified, 22050 is used by default.}
  \item{inPhaseFlag}{for choosing whether random phase is to be used or not (1 to use random phase, 0 otherwise). If empty or not specified, 1 is used by default.}
  \item{indBLevel}{dB level of generated tone (in dB) if empty or not specified, no level adjustment is performed.}}
\value{\code{outSignal}, the signal for the tone in \code{numeric} class.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\note{To convert the output to "\code{Wave}" class use \code{tuneR::Wave} and \code{tuneR::normalize}  functions.}
\examples{# Note D
D <- ShepardTone(293.66, indBLevel = -20)}
