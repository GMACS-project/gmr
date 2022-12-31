#' @title Read all the GMACS input files
#'
#' @description Function that read all the GMACS input files.
#
#' @param FileName - path to the folder containing the GMACS input files
#' @param verbose - (TRUE/FALSE); flag to print processing information
#'
#' @return the gmacs.dat; .dat, .ctl, and .prj files, as a named list.
#'
#' @details the following names are used in the list returned by this function
#' - GmacsFile: list holding the gmacs.dat file
#' - DatFile: list holding the Spc.dat file
#' - CtlFile: list holding the Spc.ctl file
#' - PrjFile: list holding the Spc.prj file
#' where Spc designs the stock of interest.
#'
#' @seealso \code{\link{readGMACS.dat}},\code{\link{readGMACSdat}},
#' \code{\link{readGMACSctl}},\code{\link{readGMACSprj}}
#'
#' @export
#' @md
#
readGMACSfiles <- function(Dir = NULL, verbose = NULL) {
  oldWD <- getwd()

  on.exit(setwd(oldWD))

  cat("--Setting working directory to '", Dir, "' \n", sep = "")
  cat("\n")
  setwd(Dir)

  # Read gmacs.dat
  GmacsFile <- readGMACS.dat(path = "gmacs.dat", verbose = verbose)
  DatFileName <- GmacsFile$DatFileName
  CtlFileName <- GmacsFile$CtlFileName
  PrjFileName <- GmacsFile$PrjFileName
  N_Year_Retro <- GmacsFile$N_Year_Retro

  # Read gmacs .dat, .ctl, .prj files
  DatFile <- readGMACSdat(FileName = DatFileName, verbose = verbose)
  CtlFile <- readGMACSctl(
    FileName = CtlFileName,
    DatFile = DatFile,
    verbose = verbose,
    nyrRetro = N_Year_Retro
  )
  PrjFile <- readGMACSprj(FileName = PrjFileName, verbose = verbose)

  Out <- list()
  Out$GmacsFile <- GmacsFile
  Out$DatFile <- DatFile
  Out$CtlFile <- CtlFile
  Out$PrjFile <- PrjFile
  return(Out)
}
