# **eaR**: An R package for perception-based auditory analysis
<!--- 
[![Travis-CI Build Status](https://travis-ci.org/FredHasselman/casnet.svg?branch=master)](https://travis-ci.org/FredHasselman/casnet)
-->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/FredHasselman/casnet?branch=master&svg=true)](https://ci.appveyor.com/project/FredHasselman/casnet)
[![CRAN status](https://www.r-pkg.org/badges/version/casnet)](https://cran.r-project.org/package=casnet)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

This is a version in development of the **eaR** package, an open-source **R** tool for perception-based auditory analysis. **eaR** provides user-friendly and flexible tool oriented to facilitate the workflow of researchers interested in content analysis and automatic extraction of auditory and musical features. Routines are mainly designed according to the  auditory short term memory (ASTM) model developed by Marc Leman and previously implemented in [IPEM Toolbox](https://www.ugent.be/lw/kunstwetenschappen/ipem/en/research/projects/finishedprojects/ipem-toolbox.htm).  


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
<!--- 
You will get a message confirming that your **eaR** Package has been installed correctly.
___
:exclamation: **Note for Mac users:**
Running the Auditory Model makes it necessary to give permissions to execute the file `.../Auditory_Model/ASTMunix`. To install correctly all dependecies, once the package is download type in the **R** prompt:

```R
library(eaR)
InstallAuditoryModel("mac")
```
(See: Auditory_Model/Readme.txt)
-->

## A short glipmse to Auditory Images, the atom of **eaR**
The **eaR**  package contains an adapted version of the auditory periphery model proposed by [Van Immerseel and Martens (1992)](https://asa.scitation.org/doi/10.1121/1.402840). The model simulates the cochlear mechanical filtering using an array of overlapping band-pass filters yielding to a set of harmonic oscillators distributed in pitch-related frequency bands. An audio signal can be represented as a *primary image* or *auditory nerve image* (ANI), which is k-dimensional vector of rate-code of neural discharges in k-frequency bands.

To calculate the ANI of the *SchumannKurioseGeschichte* dataset, type:

```R
library(eaR)
data(SchumannKurioseGeschichte)
ANIs <- CalcANI(SchumannKurioseGeschichte, 22050) #auditory nerve image
PPs <- PeriodicityPitch(ANIs)
PlotImage(PPs)
```

After running the code, you will get an image as follows,

<div align="center">
 <img src="https://github.com/m-vidal/eaR/blob/main/PP.jpeg"></a><br>
</div>
which is a transformation of the auditory nerve image into a more coherent pitch representation.

## Version 0.2.1

The package was designed through the object class `ANI`, which at least must contain an image, the sample frequency of the image and a set of frequencies or periods used in their transformations. Unlike in IPEM Toolbox, the formulation of the class `ANI`  facilitates interaction in the use of the functions and prevent disorderly data. This version includes the functions detailed in the following table. In addition, you can check whether the functions and documentation are available. 

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

`eaR` Package was build on **R** 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out".

[PPImage]: https://github.com/m-vidal/pv01/blob/master/PP.jpeg "Periodicity Pitch Image"

