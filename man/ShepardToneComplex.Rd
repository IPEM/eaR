\name{ShepardToneComplex}
\alias{ShepardToneComplex}
\title{Shepard Tone Complex}
\description{This function generates a tone complex built up of Shepard tones.}
\usage{ShepardToneComplex(inToneVector, inDuration = 1, inSampleFreq = 22050,
                   inPhaseFlag = 1, indBLevel = NULL)}
\arguments{
  \item{inToneVector}{a 12 elements vector representing the amplitude for each tone \code{"C", "C#", "D",} ... in the tone complex (0 = no tone, 1 = full amplitude).}
  \item{inDuration}{the duration (in s). If empty or not specified, 1 is used by default.}
  \item{inSampleFreq}{the desired sample frequency for the output signal (in Hz) if empty or not specified, 22050 is used by default.}
  \item{inPhaseFlag}{for choosing whether random phase has to be used or not (1 to use random phase, 0 otherwise). If empty or not specified, 1 is used by default.}
  \item{indBLevel}{ dB level of generated tone complex (in dB) if empty or not specified, no level adjustment is performed.}}
\value{\code{outSignal}, the signal for the tone complex in \code{numeric} class.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{# Chord C
C  <- ShepardToneComplex(c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0), indBLevel = -20)}
