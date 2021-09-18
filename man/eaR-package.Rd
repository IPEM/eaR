\name{eaR-package}
\alias{eaR-package}
\alias{eaR}
\docType{package}
\title{Perception-based Music Analysis}

\description{Analysis of auditory content and automatic extraction and description of musical features from auditory images, calculated from the acoustical signal.

Some functions were ported from earlier versions of \href{https://github.com/IPEM/IPEMToolbox}{IPEM Package} in Matlab.}

\details{
\tabular{ll}{

Package: \tab eaR\cr
Type: \tab Package\cr
Version: \tab 0.2.1\cr
Date: \tab 2021-mm-dd\cr
License: \tab GPL-2\cr
URL:
     \tab \url{https://github.com/m-vidal/eaR} \cr
}
Transformation of acoustic signals to auditory nerve images (ANI) can be realized with function \code{\link{CalcANI}}.  Results of the model (a time/period image) can be accessed through plot function \code{\link{PlotImage}}. Some functions are intended for the creation of auditory stimuli for its subsequent transformation and analysis (\code{\link{AdaptLevel}}, \code{\link{CalcNoteFrequency}}, \code{\link{ShepardTone}}, \code{\link{ShepardToneComplex}}).

Further transformations from auditory nerve images can be taken using \code{\link{PeriodicityPitch}} and \code{\link{LeakyIntegration}} functions. Inferences on pitch images can be performed on \code{\link{ContextualityIndex}}. This function calculates the goodness of fit of tone patterns (\emph{probe}) for a given tonal context.

Additionally, \code{\link{CalcOnsetsFromANI}} finds onsets of sound events and \code{\link{RoughnessFFT}} analyses the roughness (or equivalently: the sensory dissonance) of a sound. Generally, plots of the analysis can be accessed on the generated object.

The rest of the functions have an accessory character, and can eventually be used in certain circumstances in combination to the main ones.\cr

}
\author{Marc Vidal, Marc Leman and Joren Six; credits are also due to Koen Tanghe, Micheline Lesaffre.}
\references{
%Langner, G. (1992). Periodicity coding in the auditory system.  \emph{Hearing Research}, 60, pp.115–142.
%Langner, G., Benson, C. (2015). \emph{The Neural Code of Pitch and Harmony}. Cambridge	University	Press.

Leman, M. (2000). An auditory model of the role of short-term memory in probe-tone ratings. \emph{Music Perception}, 17(4), pp. 481-509.

Leman, M. (2000). \href{https://pdfs.semanticscholar.org/b1df/e03230e1e0e0d38275bd4200255a8532a45d.pdf}{Visualization and calculation of the roughness of acoustical musical signals using the synchronization index model (SIM)}. \emph{Proceedings of the COST G-6 Conference on Digital Audio Effects (DAFX-00)}, Verona, Italy.

Leman, M., Lesaffre, M., Tanghe, K. (2014).
Toolbox for perception-based music analysis. Matlab toolbox version 1.02 (beta). Ghent University.

%Ligges, U., Krey, S., Mersmann, O., Schnackenberg, S. (2018). \href{https://CRAN.R-project.org/package=tuneR}{tuneR: Analysis of Music and Speech}. R package version 1.3.3.

%Smith, L. (1996). \href{http://papers.nips.cc/paper/1024-onset-based-sound-segmentation.pdf}{Onset-based Sound Segmentation}. \emph{Advances in neural information processing systems}, vol.9, pp.729-735.

%Sueur, J. (2018).  \emph{Sound analysis and synthesis with R}. Heidelberg, Germany: Springer.

Van Immerseel, L. Van and Martens, J. (1992). \href{https://asa.scitation.org/doi/abs/10.1121/1.402840?journalCode=jas}{Pitch andvoiced/unvoiced determination with an auditory model}. \emph{The Journal of the Acoustical Society of America}, vol. 91, pp.3511-3526.
}

\keyword{ package }
% \seealso{ }

