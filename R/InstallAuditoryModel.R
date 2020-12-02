InstallAuditoryModel <- function (inOs =  NULL)
{
   if (is.null(inOs)) {
    stop("\n Invalid argument. Type 'mac', 'win', or 'linux' according to your system.")
    } else if (inOs != "win" && inOs != "linux" && inOs != "mac") {
      stop("\n Invalid argument. Type 'mac', 'win', or 'linux' according to your system.")
      } else {
      os <- Sys.info()["sysname"]
      OldPath <- getwd()
      inAuditoryModelPath <- paste(.libPaths(),"/IPEM/Auditory_Model/", sep="")
      setwd(inAuditoryModelPath)

      switch (inOs,
        "mac" = {
          if (os == "Windows") {
            stop ("Your system is ", os,".")
            }
          system("chmod +x ASTMunix")
          cat('The Auditory Model has successfully been installed.')
        }, "linux" = {
          cat('The Auditory Model is not yet avaible for this system.')
        }, "win" = {
          if (os != "Windows") {
            stop ("Your system is ", os,".")
          }
          system("cmd.exe /c start .", intern = T)
          cat('The Auditory Model has already been installed.')
        }
      )
      setwd(OldPath)
  }
}
