\name{CalcOnsetsFromANI}
\alias{CalcOnsetsFromANI}
\title{Calculates Onsets from an ANI}
\description{This function calculates the onsets of a signal represented by its auditory nerve image using an integrate-and-fire neural net layer.}
\usage{CalcOnsetsFromANI(inANIObj)}
\arguments{
  \item{inANIObj}{an object of class "\code{AI}" which must contain an auditory nerve image, its sample frequency and the filterbank frequencies used by the auditory model.}}
\value{
  \item{OnsetSignal}{A signal having a non-zero value for an onset, and zero otherwise. The higher the non-zero value, the more our system is convinced that the onset is really an onset.}
  \item{OnsetFreq}{sample frequency of the result (same as \code{inSampleFreq}).}}
\author{Marc Vidal (\code{R} version). Based on the original code from \pkg{IPEM} Toolbox.}
\examples{data(SchumannKurioseGeschichte)
s <- SchumannKurioseGeschichte
ANIs <- CalcANI(s, 22050)
CalcOnsetsFromANI(ANIs)}
