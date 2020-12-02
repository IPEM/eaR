\name{PeriodicityPitch}
\alias{PeriodicityPitch}
\title{Periodicity Pitch Image}
\description{This function calculates the periodicity pitch of an auditory nerve image containing neural firing patterns in different auditory channels. It is based on the idea that periodicity pitch is calculated as the summed autocorrelation over bandpass filtered fluctuations in auditory channels.

We consider the auditory nerve image \eqn{\mathbf{A}} and its sample frequency \eqn{f_{A}},  from the output of the function \code{\link{CalcANI}}. The channels (rows) of \eqn{\mathbf{A}} are filtered between 80 and 1250 Hz. Due to the fact that the output of the auditory model gives the envelopes of the neural firing probabilities (< 1250Hz), it suffices to first apply a low-pass filter and then subtract that from the original signal in order to obtain the pitch. Thus, let \eqn{F} be a second order Butterworth filter with a cutoff frequency of 80 Hz, then,
\deqn{\widehat{\mathbf{A}}=\mathbf{A}-F[\mathbf{A}].}
The lower limit of 80 Hz accounts for the fact that for smaller frequencies, the sensation of pitch becomes more a sensation of textural properties. The higher limit of 1250 Hz is related to the limits of neural synchronization. Beyond about 1250 Hz, the neurons are no longer able to follow the exact period of the signal very accurately, and periodicity pitch becomes unreliable. Only the lowest frequency can be changed by \code{inLowFrequency}.

A frame-based auto-correlation analysis is performed on the filtered channels. A frame width \eqn{FW} and step size \eqn{FS} are chosen and then for each frame and each channel \eqn{\mathbf{c}_{k}} of \eqn{\widehat{\mathbf{A}}}, we perform an auto-correlation for each time-lag \eqn{\delta},
\deqn{r_{k}^{j}(t)=\int_{t}^{t+F W} \mathbf{c}^j_{k}(\tau)\mathbf{c}^j_{k}(\tau+\delta) d \tau}
where \eqn{\delta \in[0,FW]}, \eqn{\tau} denotes the periods of the periodicity analysis and \eqn{j\in\lfloor1,\mathsf{int}(t/FS)\rfloor}. A coincidence mechanism is fulfilled by the summation of the auto-correlation results over all channels, so that
\deqn{\mathbf{P}_{*,j}=\sum_{k=1}^{m}r_{k}^{j}(t)^{T}}
where \eqn{\mathbf{P}} is the pitch image, that is related to the notion of virtual pitch pattern. It gives an account of the common periodicity along the auditory neurons in the frequency region of 80 Hz to 1250 Hz.}
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
\details{As for any frame-based function, the first value of the output signal is the value calculated for the first complete frame in the input signal. This means the following: if you have an input signal of length 1 s at a sample frequency of 1000 Hz, a frame width of 0.050 s, and a frame step size of 0.010 s, then there will be \code{ceiling(((1 - 0.050)*1000 + 1)/(0.010*1000)) = 96} values in the output signal, where the first value corresponds to the first complete frame (the interval 0 to 0.050 s).}
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
