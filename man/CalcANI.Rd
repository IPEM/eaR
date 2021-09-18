\name{CalcANI}
\alias{CalcANI}
\title{Auditory Nerve Image}
\usage{CalcANI(inSignal, inSampleFreq,
        inDownsamplingFactor = 4, inNumOfChannels = 40,
        inFirstCBU = 2.0, inCBUStep = 0.5)}
\description{This function calculates the auditory nerve image (ANI) from a sampled signal \eqn{s_i \ (i=1,\ldots,n)} using an adapted version of Van Immerseel and Martens (1992) model of the auditory periphery. The signal \eqn{s_i} is transformed into a multi-channel signal that emulates the spreading of excitation across equally spaced frequency bands.
Different filtering techniques and an envelope component extractor are used to obtain the neural firings; see Van Immerseel and Martens (1992). The output of the model is an auditory nerve image or \emph{primary image}, a \emph{n} x \emph{m} matrix, with \emph{m} the number of channels representing the instantaneous amplitude across all frequency subbands. The frequency range covered by the auditory filters depends on the centre frequencies of the filters, the number of channels and the distance between the channels. In our simulations, we select 40 channels, half a critical band apart from each other. This setting covers a range from 140 Hz to 8877 Hz.

To let the auditory model process the acoustical signal, note that the function \code{CalcANI} resamples all signal inputs automatically to a sampling rate of 22050 Hz. The outcome of the auditory model has a sampling frequency of 11025 Hz, but for most calculations, this can be downsampled to a lower value (\eqn{f_{A}=11025/4} is the default).}
\arguments{
  \item{inSignal}{the sound signal to be processed. It can either be a "\code{WAVE}" object (\pkg{tuneR}), a numeric vector or a "\code{*.wav}" file stored in the working wirectory. }
  \item{inSampleFreq}{the sample frequency of the input signal (in Hz).}
  \item{inDownsamplingFactor}{the integer factor by which the outcome of the
auditory model is downsampled (use 1 for no downsampling). If empty or not specified, 4 is used by default.}
  \item{inNumOfChannels}{number of channels to use. If empty or not specified, 40 is used by default.}
  \item{inFirstCBU}{frequency of first channel (in critical band units). If empty or not specified, 2 is used by default.}
  \item{inCBUStep}{frequency difference between channels (in cbu). If empty or not specified, 0.5 is used by default.}}
\value{An object of class "\code{AI}", which is a list with the following elements:
  \item{AuditoryNerveImage}{a matrix of size \emph{m} x \emph{n} representing the auditory nerve image, where \emph{m} is the number of channels and \emph{n} is the number time observations.}
  \item{ANIFreq}{sample frequency of ANI (in Hz).}
  \item{ANIFilterFreqs}{center frequencies used by the auditory model (in Hz).}}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\references{Van Immerseel, L. Van and Martens, J. (1992). \href{https://asa.scitation.org/doi/abs/10.1121/1.402840?journalCode=jas}{Pitch and voiced/unvoiced determination with an auditory model}. \emph{The Journal of the Acoustical Society of America}, vol. 91, pp.3511-3526.

Ligges, U., Krey, S., Mersmann, O., Schnackenberg, S. (2018). \href{https://CRAN.R-project.org/package=tuneR}{tuneR: Analysis of Music and Speech}. R package version 1.3.3.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s, 22050)}
