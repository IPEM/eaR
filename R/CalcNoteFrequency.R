CalcNoteFrequency <- function (inNote, inOctave = 4)
{
  if (substr(inNote, 2:2, 2:2) == "#") {
    NoteName <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')
    } else {
    NoteName <- c('C', 'Db', 'D', 'Eb', 'E', 'F', 'Gb', 'G', 'Ab', 'A', 'Bb', 'B')
  }

  Notenum <- c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2)
  As <- c(27.5, 55, 110, 220, 440, 880, 1760, 3520)
  for (i in 1:length(NoteName)) {
    if (NoteName[i] == inNote) {
    n <- i
    }
  }

  Freqnote <- As[inOctave+1] * (1.059463)^Notenum[n]
  if (Freqnote < 20) {
    cat("Human hearing ranges from 20 Hz to 20000 Hz.\n")
  }

  return(Freqnote)
}


