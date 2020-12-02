FindAllPeaks <- function (inSignal, inFlatPreference ="center")
{
  N <- length(inSignal)
  Diff <- diff(inSignal)

  Peaks <- array(0, N)
  Start <- 0
  i <- 1
  switch (inFlatPreference,

    "left" = {
      while (i < N) {
        if (Diff[i] > 0) {
          Start <- i
        } else if (Diff[i] < 0 && Start != 0) {
          Peaks[Start+1] <- 1
          Start <- 0
        }
        i <- i + 1
      }

    }, "center" = {
      while (i < N) {
        if (Diff[i] > 0) {
          Start <- i
        } else if (Diff[i] < 0 && Start != 0) {
          Peaks[floor((i+Start+1)/2)] <- 1
          Start <- 0
        }
        i <- i + 1
      }

      }, "right" = {
      while (i < N) {
        if (Diff[i] > 0) {
          Start <- i
        } else if (Diff[i] < 0 && Start != 0) {
          Peaks[i] <- 1
          Start <- 0
        }
        i <- i + 1
      }
    }
    )

outPeakIndices <- which(Peaks > 0)

return(outPeakIndices)
}
