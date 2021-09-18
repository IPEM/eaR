\name{RoughnessFFT}
\alias{RoughnessFFT}
\title{Calculation of the Roughness of Acoustical Musical Signals}
\description{This function estimates the roughness (or the sensory dissonance) of a sound, from its Auditory Nerve Image calculated in \code{\link{CalcANI}}.

Roughness is considered to be a sensory process highly related to texture perception. The visualization and calculation method is based on Leman (2000), where roughness is defined as the energy of the relevant beating frequencies in the auditory channels. The model is based on phase-locking to frequencies that are present in the neural patterns. A synchronization index allows the visualization of the energy components underlying roughness, in particular concerning the auditory channels and the phase-locking synchronization, that is, the synchronization index for the relevant beating frequencies on a frequency scale.

Beating frequencies are those at which the sound oscillates in amplitude. Thus, for an amplitude modulated sine wave with carrier frequency \eqn{f_{c}} and modulated frequency \eqn{f_m}, its spectrum has only three frequencies: \eqn{f_{c}}, \eqn{f_{c}-f_{m}} and \eqn{f_{c}+f_{m}}. The beating frequency is \eqn{f_m}. Now, in the auditory system, these frequencies are introduced as elective beating frequencies into the spectrum of the neural rate-code patterns. This is because to wave rectification in the cochlea, where the lower part of the modulated signal is cut off. As a result, the new frequencies are introduced of which the most important ones correspond with the beating frequency \eqn{f_m} and its multiples. Neurons may synchronize with these frequencies provided that they fall in the frequency range where synchronization is physiologically possible. This mechanism forms a physiological basis for the detection of beats and hence, the sensation of roughness.

The synchronization index model calculates roughness in terms of the energy of neural synchronization to the beating frequencies. The energy refers to a quantity which we derive from the magnitude spectrum. Since the beating frequencies are contained in the lower spectral area of a continuous neural auditory pattern \eqn{a(t)}, the spectral part we are interested in is
\deqn{b_j(t,\omega)=F_j(t,\omega)|d_{j}(t,\omega)|,\quad(j=1,\ldots,m),}
where \eqn{F(\omega)} is a filter and \eqn{d_{j}(t,\omega)} is the short-term spectrum
\deqn{d_{j}(t,\omega)=\int_{-\infty}^{+\infty}a_{j}(t)w\left(t^{\prime}-t\right)e^{-2\pi i \omega t^{\prime}}\mathrm{d}t^{\prime},}
being \eqn{w\left(t^{\prime}-t\right)} a (hamming) window. The \emph{magnitude} spectrum is then defined as \eqn{|d_{j}(t, \omega)|} and the \emph{phase} spectrum as \eqn{\angle d_{j}(t, \omega)}. In order to be able to reproduce the psychoacoustical data on roughness, the filters \eqn{F_j(t,\omega)} should be more narrow at auditory channels where the center frequency is below 800 Hz, and the filters should be attenuated for high center frequencies as well.

The pattern \eqn{b_{j}(t,\omega)} represents the \emph{spectrum} of the neural synchronization to the beating frequencies in each channel. The \emph{synchronization index} (Javel et al., 1988) of the beating frequencies is then defined as the normalized magnitude
\deqn{\xi_j(t,\omega)=\left|\frac{b_{j}(t,\omega)}{d_{j}(t, 0)}\right|,}
where \eqn{\xi_{j}(t, \omega)} is the normalized magn  itude and \eqn{d_{j}(t,0)} is the DC-component of the whole signal at each channel. The short-term energy spectrum of the neural synchronization to beating frequencies is defined by
\deqn{\hat{b}_{j}(t,\omega)=\xi_{j}(t,\omega)^{\alpha},}
where \eqn{(1<\alpha<2)} is a  parameter  which  can  be  related  to  the  power  law (Leman, 2000). For our function setup  \eqn{\alpha=1.6}. We then define the roughness as
\deqn{r_{\hat{b}}(t)=\int\sum_{j=1}^{m}\hat{b}_{j}(t,\omega)\mathrm{d}\omega}
%\deqn{r_{\hat{b}_{j}}(t)=\sum_{j=1}^{m}\hat{b}_{j}(j)=\sum_{j=1}^{m}\int\hat{b}_{j}(k,\omega)\mathrm{d}f}
which is obtained by an integration of the energy over all frequencies, as well as over all channels. This definition implies a proper visualization, one along the axis of auditory channels and one along the axis of the (beating) frequencies.}
\usage{RoughnessFFT(inObjANI, inFrameWidth = 0.2, inFrameStepSize = 0.02)}
\arguments{
  \item{inObjANI}{an object of class "\code{AI}" which must contain an auditory nerve image, its sample frequency and the filterbank frequencies used by the auditory model.}
  \item{inFrameWidth}{the width of the window for analysing the signal (in s). If empty or not specified, 0.2 s is used by default.}
  \item{inFrameStepSize}{the stepsize or time interval between two \code{inFrameWidthInSampless} (in s).}}
\details{For now, the roughness values are dependend on the used frame width. So, to make usefull comparisons, only results obtained using the same frame width should be compared.}
\value{
 \item{outFFTMatrix1}{visualisation of energy over channels.}
  \item{outFFTMatrix2}{visualisation of energy spectrum for synchronization (synchronisation index SI).}
  \item{outRoughness}{roughness over signal.}
  \item{outSampleFreq}{sampling rate of outRoughness (in Hz).}
  \item{PlotRoughness}{the plot of the roughness over signal.}}
\references{
Javel, E., McGee, J., Horst, J., & Farley, G. (1988). Temporal mechanisms in auditory stimulus coding. In G. Edelman, W. Gall, & W. Cowan (Eds.), \emph{Auditory function: neurobiological bases of hearing}. New York: John Wiley and Sons.

Leman, M. (2000). \href{https://pdfs.semanticscholar.org/b1df/e03230e1e0e0d38275bd4200255a8532a45d.pdf}{Visualization and calculation of the roughness of acoustical musical signals using the synchronization index model (SIM)}. In: \emph{Proceedings of the COST G-6 Conference on Digital Audio Effects (DAFX-00)}, Verona, Italy.}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s, 22050)
Rs <- RoughnessFFT(ANIs)}
