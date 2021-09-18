\name{PeriodicityPitch}
\alias{PeriodicityPitch}
\title{Periodicity Pitch Image}
\description{This function calculates the periodicity pitch image (PPI) from the ANI matrix obtained with the function \code{\link{CalcANI}}. The periodicity pitch is calculated as the summed autocorrelation over bandpass filtered fluctuations across the auditory channels. Since the output of the auditory model gives the envelopes of the neural firing probabilities (< 1250Hz), it suffices to apply a low-pass filter to obtain the pitch in a reliable frequency range (80-1250 Hz).

Let \eqn{F_{j}} denote a second-order Butterworth filter with a cutoff frequency of 80 Hz. Then, the filtered channels are %\eqn{\tilde{A}_{j}=A_j-F_{80}[A_j],}
\eqn{\tilde{A}_{j}=F_{j}[A_j],} where \eqn{A_j\ (j=1,\ldots,m)} are the channels of the ANI matrix. %\eqn{\mathbf{A}=(A_{ij})_{n\times m}}  with associated sample frequency \eqn{f_{A}}.
The lower limit of 80 Hz accounts for the fact that for smaller frequencies, the pitch becomes more a sensation of textural properties. The higher limit of 1250 Hz is related to the limits of neural synchronization. Beyond about 1250 Hz, the neurons are no longer able to follow the exact period of the signal very accurately, and periodicity pitch becomes unreliable. Only the lowest frequency can be changed by \code{inLowFrequency}.

Secondly, a frame-based convolution analysis and a coincidence mechanism is conducted by the summation of the convolution results over all channels. Then, for frame width \eqn{w}, ecah column of the periocicity pitch image is obtained by
%\deqn{r_{j}^{k}(t)=\int_{t}^{t+F W} \tilde{A}^k_{j}(\tau)\tilde{A}^k_{j}(\tau+\delta) \mathrm{d} \tau}
\deqn{P_{i}=\sum_{j=1}^{m}\tilde{A}_{i_{k}j}\ast\tilde{A}_{(i+\delta)_{k}j}\ (\forall i=\tau_{1},\ldots,\tau_{z};k=1,\ldots,w),}
where \eqn{\delta} is the frame lag, \eqn{\tau_1,\ldots,\tau_l} is a sequence of equidistant time steps defining the time instants of the PPI (a matrix of size \eqn{w \times z}).}

%\eqn{\delta \in[0,FW]}, \eqn{\tau_1,\ldots,} denotes the periods of the periodicity analysis and \eqn{k\in\lfloor1,\mathsf{int}(t/FS)\rfloor}.}

%A coincidence mechanism is fulfilled by the summation of the auto-correlation results over all channels, so that \deqn{P_{\cdot,k}=\sum_{j=1}^{m}r_{j}^{k}(t)^\top} where \eqn{\tilde{A}} are pitch image.}
\usage{PeriodicityPitch(inANIObj, inLowFrequency = 80,
                 inFrameWidth = 0.064, inFrameStepSize = 0.010)}
\arguments{
  \item{inANIObj}{an object of class "\code{AI}". It must contain the output of \code{\link{CalcANI}} function:
  \itemize{
    \item{a matrix of size  \emph{n} x \emph{m} where \emph{n} is the number of auditory channels (<= 40) and \emph{m} is the number of samples}
    \item{the sample frequency of the input signal (in Hz).}}}
  \item{inLowFrequency}{cutoff frequency (in Hz) of a first order lowpass filter applied before calculating the autocorrelation. If empty or not specified, 80 is used by default.}
  \item{inFrameWidth}{width of the frame used for the accumulation of the autocorrelation (in s). If empty or not specified, 0.064 is used by default.}
  \item{inFrameStepSize}{stepsize or time interval between two \code{inFrameWidth} (in s). If empty or not specified, 0.010 is used by default.}}
\details{As for any frame-based function, the first value of the output signal is the value calculated for the first complete frame in the input signal. Thus, if you have an input signal of length 1 s at a sampling frequency of 1000 Hz, frame width of 0.050 s, and a frame step size of 0.010 s, then there will be \code{ceiling(((1 - 0.050)*1000 + 1)/(0.010*1000)) = 96} values in the output signal, where the first value corresponds to the first complete frame (the interval 0 to 0.050 s).}
\value{An object of class "\code{AI}", which is a list with the following elements:
  \item{PeriodicityPitchImage}{periodicity pitch: a matrix of size \code{inFrameWidth * length(inMatrix) / outSampleFreq}}
  \item{SampleFreq}{sampling rate, equal to inSampleFreq/inFrameStepSize (in Hz).}
  \item{Periods}{analyzed periods (in s).}
  \item{BPANI}{bandpass filtered auditory nerve images (at the original sample frequency).}}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{probe <- ShepardTone(293.66, 1, indBLevel = -20)
s <- c(SchumannKurioseGeschichte, numeric(2205), probe)
ANIs <- CalcANI(s, 22050)
PPs <- PeriodicityPitch(ANIs)}
