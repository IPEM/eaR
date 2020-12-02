\name{InstallAuditoryModel}
\alias{InstallAuditoryModel}
\title{Install the Auditory Model}
\description{Running the auditory model requires giving permissions to execute a compiled version of the auditory modelling library in c-code. Before start working with \pkg{eaR} use this function.}
\usage{InstallAuditoryModel(inOs = NULL)}
\arguments{
  \item{inOs}{On which system do you want to check the installation of the auditory model?
  It can be a character vector (selecting from "mac", "linux", "win") indicating your operating system.}}
\details{This function is for MAC and Linux users, while Windows users can verify if the model is installed correctly.}
\value{Depending on the system you will get a specific message about the installation process. The window where the files are located will open (only Windows). }
\author{Marc Vidal.}
\examples{InstallAuditoryModel("win")}
