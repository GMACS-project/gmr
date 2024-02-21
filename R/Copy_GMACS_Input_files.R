#' @title Copy specific GMACS input files
#'
#' @description
#'  Reads the gmacs.dat file to figure out the names of the control, data,
#' and projection files, then copies those files along with gmacs.dat and
#' gmacs.pin (if applicable).
#'
#' @param fromDir (path)- path to the directory where the input files to copy
#' are stored.
#' @param toDir (path)- path to the directory where to copy the files.
#' @param GMACS_files (character string)- name of the GMACS input file(s) you want to
#' copy. Options for \code{GMACS_files} are: 'gmacs.dat','gmacs.pin',
#' the names of either the control, data, or projection files
#' (with extention files, i.e.: .ctl, .dat, .prj).
#' If \code{GMACS_files="all"}, then the function will copy
#' and paste all the input files cited above.
#' from the base model directory to your current model in development.
#' @param overwrite (logical) - Overwrite the input files if they already exist
#' in the \code{toDir} repertory.
#'
#' @author Matthieu Veron
#  Contact: mveron@uw.edu
#'
#' @export
#'
copy_GMACSinputs <- function(fromDir = NULL,
                             toDir = NULL,
                             GMACS_files = NULL,
                             overwrite = NULL) {
  # local declarations
  fsep <- .Platform$file.sep

  # check for presence of SS files
  if (is.null(GMACS_files))
    stop("Please provide name(s) for the input file(s) to copy from the following directory\n\t:",
         fromDir)
  # Check consistency of SS file
  filesFrom <- list.files(fromDir)


  if (!GMACS_files %in% c(filesFrom, "all"))
    stop(
      "The specified GMACS input file(s) do(es) not exist.\n"
    )

  # Test if the gmacs.dat file can be read
  if (GMACS_files %in% c("gmacs.dat", "all")) {
    Datfile <- file.path(fromDir,
                         "gmacs.dat")
    if (file.exists(Datfile)) {
      gmacsDatfile <- readGMACS.dat(path = Datfile, verbose = FALSE)
    } else {
      warning(
        "The gmacs.dat file is not found in the following directory:\n=>",
        file.path(fromDir)
      )
      return(FALSE)
    }
  }

  cat("- Copying GMACS input file(s) from:\n")
  cat("\t=>", fromDir, "\n to: \n\t=>", toDir, "\n")

  cop_Done <- NULL

  if (GMACS_files == "all") {
    cop <- file.copy(
      from = file.path(fromDir, gmacsDatfile[["CtlFileName"]]),
      to = file.path(toDir, gmacsDatfile[["CtlFileName"]]),
      overwrite = overwrite
    )
    cop_Done <- c(cop_Done, cop)
    cop <- file.copy(
      from = file.path(fromDir, gmacsDatfile[["DatFileName"]]),
      to = file.path(toDir, gmacsDatfile[["DatFileName"]]),
      overwrite = overwrite
    )
    cop_Done <- c(cop_Done, cop)
    cop <- file.copy(
      from = file.path(fromDir, gmacsDatfile[["PrjFileName"]]),
      to = file.path(toDir, gmacsDatfile[["PrjFileName"]]),
      overwrite = overwrite
    )
    cop_Done <- c(cop_Done, cop)
    cop <- file.copy(
      from = file.path(fromDir, "gmacs.dat"),
      to = file.path(toDir, "gmacs.dat"),
      overwrite = overwrite
    )
    if (file.exists(file.path(fromDir, "gmacs.pin", fsep = fsep))) {
      cop <- file.copy(
        from = file.path(fromDir, "gmacs.pin"),
        to = file.path(toDir, "gmacs.pin"),
        overwrite = overwrite
      )
      cop_Done <- c(cop_Done, cop)
    }
  } else {
    for (f in 1:length(GMACS_files)) {
      copFile <- GMACS_files[f]

      if (copFile == gmacsDatfile[["CtlFileName"]]) {
        cop <- file.copy(
          from = file.path(fromDir, starter[["CtlFileName"]]),
          to = file.path(toDir, starter[["CtlFileName"]]),
          overwrite = overwrite
        )
        cop_Done <- c(cop_Done, cop)
      } else if (copFile == gmacsDatfile[["DatFileName"]]) {
        cop <- file.copy(
          from = file.path(fromDir, gmacsDatfile[["DatFileName"]]),
          to = file.path(toDir, gmacsDatfile[["DatFileName"]]),
          overwrite = overwrite
        )
        cop_Done <- c(cop_Done, cop)
      } else if (copFile == gmacsDatfile[["PrjFileName"]]) {
        cop <- file.copy(
          from = file.path(fromDir, gmacsDatfile[["PrjFileName"]]),
          to = file.path(toDir, gmacsDatfile[["PrjFileName"]]),
          overwrite = overwrite
        )
        cop_Done <- c(cop_Done, cop)
      } else if (copFile == "gmacs") {
        cop <- file.copy(
          from = file.path(fromDir, "gmacs.dat"),
          to = file.path(toDir, "gmacs.dat"),
          overwrite = overwrite
        )
        cop_Done <- c(cop_Done, cop)
      } else if (copFile == "gmacs.pin") {
        cop <- file.copy(
          from = file.path(fromDir, 'gmacs.pin'),
          to = file.path(toDir, "gmacs.pin"),
          overwrite = overwrite
        )
        cop_Done <- c(cop_Done, cop)
      }
    }
  }
  # check for successful copying
  if (unique(cop_Done) == TRUE) {
    cat("\n => Files have been copied.")
    # return(TRUE)
  } else {
    warning("At least 1 file failed to copy.")
    # return(FALSE)
  }
}
# ----------------------------------------------------------
