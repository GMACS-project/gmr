#' @title Read all input and output files
#'
#' @description This function reads all the Gmacs input files (\code{"gmacs.dat"},
#' \code{"model.dat"}, \code{"model.ctl"}, \code{"model.prj"} and if they exist, all the
#' output files (\code{"Gmacsall.out"}, \code{"gmacs.par"}, \code{"gmacs.rep"},
#' \code{"simdata.out"})).
#'
#' @param Dir (character string)- path where the input/out files are saved for the
#' stock of interest.
#'
#' @return a named list of lists:
#' \describe{
#'  \item{\code{GMACSdat}}{the `gmacs.dat` file;}
#'  \item{\code{datFile}}{the `model.dat` file;}
#'  \item{\code{ctlFile}}{the `model.ctl` file;}
#'  \item{\code{prjfile}}{the `model.prj` file;}
#'  \item{\code{GmacsAll_out}}{the `Gmacsall.out` file if available;}
#'  \item{\code{gmacs_par}}{the `gmacs.par` file if available;}
#'  \item{\code{gmacs_rep}}{the `gmacs.rep` file if available;}
#'  \item{\code{gmacs_sim}}{the `simdata.out` file if available}
#' }
#'
#' @seealso \code{\link{readGMACS.dat}},\code{\link{readGMACSdat}},
#' \code{\link{readGMACSctl}}, \code{\link{readGMACSprj}}.
#'
#' @examples
#' \dontrun{
#' # Load package ----
#' library(gmr)
#'
#' # Set directories ----
#' Dir_Dvpt_Vers <- file.path(here::here(), "Dvpt_Version", fsep = fsep)
#' Dir_Last_Vers <- file.path(here::here(), "Latest_Version", fsep = fsep)
#'
#' # Define the stock of interest ----
#' stock <- "SMBKC"
#'
#' # Read Gmacs files ----
#' Stock_files <-
#' getInpOutFiles(Dir = file.path(Dir_Dvpt_Vers, 'build', stock, fsep = fsep),
#' verbose = TRUE)
#' }
#'
#' @export
#' @md
getInpOutFiles <- function(Dir = NULL, verbose = NULL) {
  fsep <- .Platform$file.sep
  Out <- NULL

  gmacs_fileName <- "gmacs.dat"
  Alloutfile <- "Gmacsall.out"
  parfile <- "gmacs.par"
  repfile <- "gmacs.rep"
  simfile <- "simdata.out"

  # Read Gmacs input files ----
  # gmacs.dat file ----
  gmacs_fileName <- file.path(Dir, gmacs_fileName, fsep = fsep)
  GMACSdat <- readGMACS.dat(path = gmacs_fileName, verbose = verbose)
  Out[["GMACSdat"]] <- GMACSdat
  datfileName <- GMACSdat[['DatFileName']]
  ctlfileName <- GMACSdat[['CtlFileName']]
  prjfileName <- GMACSdat[['PrjFileName']]

  # Data file ----
  datFile <- file.path(Dir, datfileName, fsep = fsep)
  datFile <- readGMACSdat(FileName = datFile, verbose = verbose)
  Out[["datFile"]] <- datFile

  # Control file ----
  ctlFile <- file.path(Dir, ctlfileName, fsep = fsep)
  ctlFile <- readGMACSctl(
    FileName = ctlFile,
    verbose = verbose,
    DatFile = datFile,
    nyrRetro = GMACSdat$N_Year_Retro
  )
  Out[["ctlFile"]] <- ctlFile

  # Projection file ----
  prjfile <- file.path(Dir, prjfileName, fsep = fsep)
  prjfile <-
    readGMACSprj(FileName = prjfile, verbose = verbose)
  Out[["prjfile"]] <- prjfile

  # Read Gmacs output files ----
  # GmacsAll.out ----
  Alloutfile <- file.path(Dir, Alloutfile, fsep = fsep)
  if (file.exists(Alloutfile)) {
    GmacsAll_out <-
      readGMACSallOUT(
        FileName = Alloutfile,
        verbose = verbose,
        DatFile = datFile,
        CtlFile = ctlFile,
        GmacsFile = GMACSdat,
        nyrRetro = GMACSdat$N_Year_Retro
      )
    Out[["GmacsAll_out"]] <- GmacsAll_out
  }

  # gmacs.par ----
  if (file.exists(file.path(Dir, parfile, fsep = fsep))) {
    gmacs_par <- readGMACSpar(
      Dir = Dir,
      FileName = parfile,
      verbose = verbose,
      DatFile = datFile,
      CtlFile = ctlFile,
      GMACSdat = GMACSdat
    )
    Out[["gmacs_par"]] <- gmacs_par
  }

  # Rep file ----
  repfile <- file.path(Dir, repfile, fsep = fsep)
  if (file.exists(repfile)) {
    gmacs_rep <- readGMACSrep(
      FileName = repfile,
      verbose = verbose,
      DatFile = datFile,
      CtlFile = ctlFile,
      nyrRetro = GMACSdat$N_Year_Retro
    )
    Out[["gmacs_rep"]] <- gmacs_rep
  }

  # Simulated data ----
  if (file.exists(file.path(Dir, simfile, fsep = fsep))) {
    gmacs_sim <- readGMACSsimdat(
      Dir = Dir,
      FileName = simfile,
      verbose = verbose,
      DatFile = datFile,
      CtlFile = ctlFile
    )
    Out[["gmacs_sim"]] <- gmacs_sim
  }
  return(Out)
}
