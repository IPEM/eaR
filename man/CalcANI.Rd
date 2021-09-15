\name{CalcANI}
\alias{CalcANI}
\title{Auditory Nerve Image}
\usage{CalcANI(inSignal, inSampleFreq,
        inDownsamplingFactor = 4, inNumOfChannels = 40,
        inFirstCBU = 2.0, inCBUStep = 0.5)}
\description{This function calculates the Auditory Nerve Image (ANI) for the given signal \eqn{s(t)} using an adapted version of Van Immerseel and Martens (1992) model of the auditory periphery. The model is implemented in c-code and stored in the dependencies of the package operating in parallel using \code{Rcpp}.

We consider the input signal \eqn{s(t),  t \in \mathcal{T}=\left\{1,2,3\ldots n\right\} } to be transformed to a multidimensional signal (an image) which is a matrix. The output of the model is known as Auditory Nerve Image or \emph{Primary Image}. This can be noted as \eqn{\mathbf{A}=A(k,t)}, where \eqn{\left\{ \mathbf{c}_{k}(t)\right\} _{k=1,2,\ldots,m}} are frequency bands (or channels) at time \eqn{t}. Thus, for a fixed \eqn{\mathbf{c}_{k}} we have a discrete time series representing the rate-code of neural discharge in a channel  \eqn{k}.

Before-mentioned transformation processes are described in more detail in Immerseel and Martens (1992) but prim/rily consist of a simulation of the cochlear mechanical filtering using an array of overlapping band-pass filters. Then, the auditory signal is decomposed in different subbands and represented as neural patterns. The frequency range covered by the auditory filters depends on the center frequencies of the filters, the number of chosen channels and the chosen distance between the channels. In most of our simulations, we chose forty channels (\eqn{m = 40}), half a critical band apart from each other. This covers a range from 140 Hz to 8877 Hz.

To let the auditory model process the acoustical signal, note that the function \code{CalcANI} resamples all signal inputs automatically to a sampling rate of 22050 Hz. The outcome of the auditory model has a sample frequency of 11025 Hz, but for most calculations, this can be downsampled to a lower value (\eqn{f_{A}=11025/4} is the default).}
\arguments{
  \item{inSignal}{the sound signal to be processed. It can either be a "\code{WAVE}" object (\pkg{tuneR}), a numeric vector or a "\code{*.wav}" file stored in the Working Directory. }
  \item{inSampleFreq}{the sample frequency of the input signal (in Hz).}
  \item{inDownsamplingFactor}{the integer factor by which the outcome of the
auditory model is downsampled (use 1 for no downsampling). If empty or not specified, 4 is used by default.}
  \item{inNumOfChannels}{number of channels to use. If empty or not specified, 40 is used by default.}
  \item{inFirstCBU}{frequency of first channel (in critical band units). If empty or not specified, 2 is used by default.}
  \item{inCBUStep}{frequency difference between channels (in cbu). If empty or not specified, 0.5 is used by default.}}
\value{An object of class "\code{AI}", which is a list with the following elements:
  \item{AuditoryNerveImage}{a matrix of size \emph{n} x \emph{m} representing the auditory nerve image, where \emph{n} is the number of channels and \emph{m} is the number of samples.}
  \item{ANIFreq}{sample frequency of ANI (in Hz).}
  \item{ANIFilterFreqs}{center frequencies used by the auditory model (in Hz).}}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\references{Van Immerseel, L. Van and Martens, J. (1992). \href{https://asa.scitation.org/doi/abs/10.1121/1.402840?journalCode=jas}{Pitch and voiced/unvoiced determination with an auditory model}. \emph{The Journal of the Acoustical Society of America}, vol. 91, pp.3511-3526.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s, 22050)}
