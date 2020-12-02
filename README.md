# **eaR**: An R package for perception-based music analysis

[![Travis-CI Build Status](https://travis-ci.org/FredHasselman/casnet.svg?branch=master)](https://travis-ci.org/FredHasselman/casnet)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/FredHasselman/casnet?branch=master&svg=true)](https://ci.appveyor.com/project/FredHasselman/casnet)
[![CRAN status](https://www.r-pkg.org/badges/version/casnet)](https://cran.r-project.org/package=casnet)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

This is a version in development of **eaR** package, an open-source auditory **R** tool for perception-based music analysis. We provide a user-friendly and flexible tool oriented to facilitate the workflow of researchers interested in content analysis and automatic extraction and description of musical features. Routines are mainly designed according to the  Auditory Short Term Memory (ASTM) model developed by Marc Leman and previously implemented in [IPEM Toolbox](https://www.ugent.be/lw/kunstwetenschappen/ipem/en/research/projects/finishedprojects/ipem-toolbox.htm).  


## Install the Package
Before downloading **eaR**, you should have installed **devtools** and load it:

```R
install.packages("devtools")
library("devtools")
```

After that, to download **eaR**, you can type in the **R** prompt:

```R
install_github("m-vidal/eaR")
```
You will get a message confirming that your **eaR** Package has been installed correctly.
___

:exclamation: **Note for Mac users:**
Running the Auditory Model makes it necessary to give permissions to execute the file `.../Auditory_Model/ASTMunix`. To install correctly all dependecies, once the package is download type in the **R** prompt:

```R
library(eaR)
InstallAuditoryModel("mac")
```

Otherwise, for a manual setting see [Readme.txt](https://github.com/m-vidal/pv01/blob/master/inst/Auditory_Model/Readme.txt).

## A short glipmse to Auditory Images, the atom of **eaR** Package
Artificial Neural Networks are information processing systems whose structure and operating are inspired by the biological ones. Thus, Auditory Images are, in essence, the basis elements that represents features related to an acoustic signal, particurally, a set of harmonic oscillators distributed in pitch-related frequency bands.

The kernel of **eaR** Package  is an adapted version of  [Van Immerseel and Martens (1992)](https://asa.scitation.org/doi/10.1121/1.402840) model of the auditory periphery. The auditory model -written in c-code- simulates the cochlear mechanical filtering using an array of overlapping band-pass filters. The output of the model is the *primary image* or *auditory nerve image* (ANI), which represents the rate-code of neural discharge in a set of channels *`C`*.

The package was designed through the object class `ANI`, which at least must contain an image, the sample frequency of the image and a set of frequencies or periods used in their transformations. Unlike in IPEM Toolbox, the formulation of the class `ANI`  facilitates interaction in the use of the functions and prevent disorderly data. 

To calculate the ANI of the *SchumannKurioseGeschichte* dataset, type:

```R
R> s <- SchumannKurioseGeschichte
R> ANIs <- CalcANI(s, 22050)
R> PPs <- PeriodicityPitch(ANIs)
R> PlotImage(PPs)
```

You will get an image as follows,

<div align="center">
 <img src="https://github.com/m-vidal/pv01/blob/master/PP.jpeg"></a><br>
</div>

As described in the documentation, **eaR** Package is a tool for auditory information processing that inferences on sound patterns to simulate the perception of humans listening to music.

## Version 0.1.2 beta

This version includes the functions detailed in the following table. In addition, you can check whether the functions and documentation are available as it is a beta version. Note that the documentation is under revision yet.

Function | Status | Documentation | Migrated |
| :---  |  :---:  |  :---:  |  :---:  
`AdaptLevel` |:heavy_check_mark:|:heavy_check_mark:|Yes|
`as.Wave` |--|--|No|
`BellShape` |:heavy_check_mark:|--|Yes|
`CalcANI` |:heavy_check_mark:|:heavy_check_mark:|Yes|
`CalcNoteFrequency` |:heavy_check_mark:|:heavy_check_mark:|No|
`CalcOnsetsFromANI` |:heavy_check_mark:|--|Yes|
`CalcProfile` |--|--|No|
`CalcRMS` |:heavy_check_mark:|--|Yes|
`Clip` |:heavy_check_mark: |--|Yes |
`ContextualityIndex` |:heavy_check_mark: |:heavy_check_mark:|Yes|
`CreateMask` |:heavy_check_mark: |--|Yes|
`FindAllPeaks` |:heavy_check_mark:|--|Yes|
`FindNearestMinima` |:heavy_check_mark:|--|Yes|
`InstallAuditoryModel` |:heavy_check_mark:|:heavy_check_mark:|No|
`LeakyIntegration` |:heavy_check_mark: |:heavy_check_mark:|Yes|
`OnsetPattern` |:heavy_check_mark:|--|Yes|
`OnsetPatternFilter` |:heavy_check_mark:|--|Yes|
`OnsetPeakDetection` |:heavy_check_mark:|--|Yes|
`OnsetPeakDetection1Channel` |:heavy_check_mark:|:heavy_check_mark:|Yes|
`PeriodicityPitch` |:heavy_check_mark: |:heavy_check_mark:|Yes|
`PlotImage` |:heavy_check_mark:|:heavy_check_mark:|No|
`RoughnessFFT` |:heavy_check_mark:|:heavy_check_mark:|Yes|
`ShepardTone` |:heavy_check_mark:|:heavy_check_mark:|Yes|
`ShepardToneComplex` |:heavy_check_mark:|:heavy_check_mark:|Yes|

## More details
**eaR** uses a core application written in c-code which is fully executable in R environments. 
Both the **eaR** Package code and the auditory model c-code are released under the GPL.

`eaR` Package was build on **R** version 3.6.1.

[PPImage]: https://github.com/m-vidal/pv01/blob/master/PP.jpeg "Periodicity Pitch Image"
