\name{ear_process}
\alias{ear_process}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ear_process
}
\description{see \code{\link{CalcANI}}}
\usage{
ear_process(wav, inNumOfChannels, inFirstFreq, inFreqDist, inSampleFrequency)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wav}{
%%     ~~Describe \code{wav} here~~
}
  \item{inNumOfChannels}{the number of channels for decimation.}
  \item{inFirstFreq}{frequency of first channel (in critical band units).}
  \item{inFreqDist}{the frequency bandwidth of the "auditory filter".}
  \item{inSampleFrequency}{the sample frequency of the input signal.}
}
\value{auditory image and additional files.}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% @keywords internal
 \keyword{internal}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
