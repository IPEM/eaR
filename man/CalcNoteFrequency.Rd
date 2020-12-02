\name{CalcNoteFrequency}
\alias{CalcNoteFrequency}
\title{Note Frequency}
\description{This function calculates any note frequency for an equal-tempered scale within a range of eight octaves (0-7). The used formula is,

\deqn{f_{n}=f_{0} *(a)^{n}}{f_n=F_0*a^n}

where \eqn{f_{0}} is a fixed frequency defined at 440 Hz (\eqn{A_{4}}), and the coefficient  \eqn{a=\sqrt[{12}]{2}} allows to calculate the note frequency through the steps \eqn{n\in \mathbf{Z}} in a bounded interval, being \eqn{n_{A_{4}}=0}.}
\usage{CalcNoteFrequency(inNote, inOctave)}
\arguments{
  \item{inNote}{a note string A-G with or without accidental (e.g. "Bb"). Both sharps (#) and flats (b) can be used.}
  \item{inOctave}{the octave number of the note if empty or not specified, \code{inNoteNr} is assumed to contain the octave specification.}}
\details{\code{CalcNoteFrequency}  is a shorter version of \code{IPEMCalcNoteFrequency} (\pkg{IPEM} Toolbox).}
\value{A numeric giving the frequency (Hz) of the indicated note.}
\author{Marc Vidal.}
\examples{CalcNoteFrequency("A")}
