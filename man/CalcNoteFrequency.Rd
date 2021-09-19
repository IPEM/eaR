\name{CalcNoteFrequency}
\alias{CalcNoteFrequency}
\title{Note Frequency}
\description{This function calculates any note frequency for an equal-tempered scale within a range of eight octaves (0-7). The used formula is

\deqn{\code{frequency}=f_{0} \times (\lambda)^{\code{N}},}

where \eqn{f_{0}} is a fixed frequency defined by default at 440 Hz (\eqn{\code{A}_{4}}). The coefficient  \eqn{\lambda=\sqrt[{12}]{2}} allows to calculate the note frequency through \code{N} steps; e.g., \eqn{\code{N}=0} equals to \code{A}.}
\usage{CalcNoteFrequency(inNote, inOctave)}
\arguments{
  \item{inNote}{a note string \code{A}-\code{G} with or without accidental (e.g. \code{Bb}). Both sharps (\code{#}) and flats (\code{b}) can be used.}
  \item{inOctave}{the octave number of the note if empty or not specified, \code{inNoteNr} is assumed to contain the octave specification.}}
\details{\code{CalcNoteFrequency}  is a shorter version of \code{IPEMCalcNoteFrequency} (\pkg{IPEM} Toolbox).}
\value{A numeric giving the frequency (Hz) of the indicated note.}
\author{Marc Vidal.}
\examples{freqA4 <- CalcNoteFrequency("A")}
