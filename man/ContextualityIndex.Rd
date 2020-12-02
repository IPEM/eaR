\name{ContextualityIndex}
\alias{ContextualityIndex}
\title{Tone Contextuality Index}
\description{
Contextuality measures the pitch commonality between two running pitch images of the same sound, each having a possible different echo \eqn{T}. In Leman (2000), contextuality is used to measure the pitch commonality of local (short echo) images versus global (long echo) images.

This function uses a pitch image \eqn{\mathbf{P}} to determine two echoed images with a different \eqn{T} using \code{\link{LeakyIntegration}}. Then, we calculated the Pearson correlation coefficient  concerning  the following methods:
\itemize{\item{\emph{Inspection}: compares a fixed local image (or \emph{probe}) with running local images and running global images.  In this case, \eqn{\mathbf{P}^{T}} refers to both running local and global images while the fixed local image is the last sample of it; thus, the correlation coefficient can be expressed as}}
\deqn{\rho_{i}=\frac{1}{m-1}\sum_{k=1}^{m}\left(\frac{\overline{\mathbf{P}_{ik}^{T}-\mu_{i}}}{\sigma_{i}}\right)\left(\frac{\mathbf{P}_{nk}^{T}-\mu_{n}}{\sigma_{n}}\right).}
\itemize{\item{\emph{Comparison}: compares a running local image \eqn{\mathbf{P}^{T_{1}}} with running global image \eqn{\mathbf{P}^{T_{2}}}. Then the correlation coefficient between corresponding columns is}}
\deqn{\rho_{i}=\frac{1}{m-1}\sum_{k=1}^{m}\left(\frac{\overline{\mathbf{P}_{ik}^{T_{1}}-\mu_{i}}}{\sigma_{i}}\right)\left(\frac{\mathbf{P}_{ik}^{T_{2}}-\mu_{i}}{\sigma_{i}}\right).}
}
\usage{ContextualityIndex(inANIObj, inSnapShot = NULL, inHalfDecayChords = 0.1,
                  inHalfDecayToneCenters = 1.5, inEnlargement = 0)}
\arguments{
  \item{inANIObj}{an object of class "\code{AI}". It must contain the output of the \code{\link{PeriodicityPitch}} function: a periodicity pitch image, its sample frequency (in Hz) and the periods of periodicity analysis (in s).}
  \item{inSnapShot}{time where the snapshot should be taken (in s). If negative, the time is taken at \code{abs(inSnapShot)} from the end of the sample. If empty or not specified, the time of the last sample is used by default.}
  \item{inHalfDecayChords}{half decay time for leaky integration into chord image (local image). If empty or not specified, 0.1 is used by default.}
  \item{inHalfDecayToneCenters}{ half decay time for leaky integration into tone center image (global image). If empty or not specified, 1.5 is used by default.}
  \item{inEnlargement}{time by which the input signal is enlarged (in s). If -1, \code{2*inHalfDecayToneCenters} is used. If empty or not specified, 0 is used by default.}}
\value{An object of class "\code{AI}", which is a list with the following elements:
\item{ChordsImage}{local integration of \code{inPeriodicityPitch} into chord image.}
\item{ToneCentersImage}{global integration of \code{inPeriodicityPitch} into tone center image.}
\item{LocalInspection}{correspondence between chord taken at snapshot position and running chord image.}
\item{GlobalInspection}{correspondence between chord taken at snapshot position and running tone center image}
\item{Comparison}{correspondence between running chord image and running tone center image.}}
\details{Sample frequency of output signals is the same as \code{inSampleFreq}.}
\references{Leman, M. (2000). An auditory model of the role of short-term memory in probe-tone ratings. \emph{Music Perception}, 17(4), pp.481-509.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\note{Sample frequency of output signals is the same as \code{inSampleFreq}.}
\examples{C  <- ShepardToneComplex(c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0),0.75,22050,1,-20)
F  <- ShepardToneComplex(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),0.75,22050,1,-20)
G  <- ShepardToneComplex(c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1),0.75,22050,1,-20)
s<-c(C, F, G, C)
ANIs <- CalcANI(s, 22050)
PPs <- PeriodicityPitch(ANIs)
CIs <- ContextualityIndex(PPs)}
