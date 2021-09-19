\name{ContextualityIndex}
\alias{ContextualityIndex}
\title{Tone Contextuality Index}
\description{
The contextuality index measures the pitch commonality between two running periodicity pitch images, each one having a possible different echo. For both images, pitch commonality is also evaluated with respect to a probe. In Leman (2000), the contextuality index is used to measure the pitch commonality of local (short echo) images versus global (long echo) images.

The function uses a periodicity pitch image %\eqn{(P_{kl})_{w \times z}}
to determine two echoed images with a different rate of decay using \code{\link{LeakyIntegration}}. Then, Pearson's correlation coefficient  is calculated across all periodicities concerning  the following methods:
\itemize{\item{\emph{Inspection}: compares a fixed local image (the \emph{probe}) with running local images and running global images.}}  %Let \eqn{P_{lk}^{\tau}} refer to both running local and global images while the fixed local image is the last sample of it; thus, the correlation coefficient can be expressed as}} \deqn{\rho_{l}=\frac{1}{m_2-1}\sum_{k=1}^{m_2}\left(\frac{\overline{P_{lk}^{\tau}-\mu_{l}}}{\sigma_{l}}\right)\left(\frac{P_{n_2k}^{\tau}-\mu_{n_2}}{\sigma_{n_2}}\right).}
\itemize{\item{\emph{Comparison}: compares a running local image with a running global image. %Then the correlation coefficient between corresponding columns is}} \deqn{\rho_{l}=\frac{1}{m_2-1}\sum_{k=1}^{m_2}\left(\frac{\overline{P_{lk}^{\tau_{1}}-\mu_{l}}}{\sigma_{l}}\right)\left(\frac{P_{lk}^{\tau_{2}}-\mu_{l}}{\sigma_{l}}\right).
}}
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
F <- ShepardToneComplex(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),0.75,22050,1,-20)
G <- ShepardToneComplex(c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1),0.75,22050,1,-20)
s <- c(C, F, G, C)
ANIs <- CalcANI(s, 22050)
PPs <- PeriodicityPitch(ANIs)
CIs <- ContextualityIndex(PPs)}
