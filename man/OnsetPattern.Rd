\name{OnsetPattern}
\alias{OnsetPattern}
\title{Onset Pattern}
\description{Integrate-and-fire neural net for onset-detection. Based on an article by Leslie S. Smith (1996). Neuron dynamics are given by: \code{dA/dt = Input(t) - diss*A}, being \code{A} the neurons' accumulated value and \code{diss} the dissipation.}
\usage{OnsetPattern(inSignal, inSampleFreq)}
\arguments{
  \item{inSignal}{a matrix of size  \emph{n} x \emph{m} where \emph{n} is the length of time observations and \emph{m} is the number of channels.}
  \item{inSampleFreq}{the sample frequency of the incoming signal (in Hz).}}
\value{
 \item{OnsetPattern}{a matrix of size  \emph{n} x \emph{m} where an element is 1 if an onset was detected.}
 \item{OnsetPatternFreq}{sample frequency of result (same as \code{inSampleFreq}).}}
\references{Smith, L. (1996). \href{http://papers.nips.cc/paper/1024-onset-based-sound-segmentation.pdf}{Onset-based Sound Segmentation}. \emph{Advances in neural information processing systems}, vol.9, pp.729-735.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s, 22050)
OPs <- OnsetPattern(ANIs$AuditoryNerveImage, ANIs$ANIFreq)}
