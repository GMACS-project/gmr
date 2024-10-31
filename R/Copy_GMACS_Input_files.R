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
#' @param verbose (logical)- flag to print processing information.
#' @param do_pinFile (logical)- flag to indicate if the "gmacs.pin" has to be
#' copied over if available in the \code{path} folder.
#' Default, \code{do_pinFile = TRUE}.
#'
#' @author Matthieu Veron
#  Contact: mveron@uw.edu
#'
#' @export
#'
copy_GMACSinputs <- function(fromDir = NULL,
                             toDir = NULL,
                             GMACS_files = NULL,
                             overwrite = NULL,
                             verbose = TRUE,
                             do_pinFile = TRUE) {
  # local declarations
  fsep <- .Platform$file.sep

  # check for presence of SS files
  if (is.null(GMACS_files)){
    txt1 <- paste0("Please provide name(s) for the input file(s) to copy from the following directory\n\t:",
                   fromDir)
    stop(txt1)
  }

  # Check consistency of SS file
  filesFrom <- list.files(fromDir)


  if (!GMACS_files %in% c(filesFrom, "all")){
    txt2 <- "The specified GMACS input file(s) do(es) not exist.\n"
    stop(txt2)
  }

  # Test if the gmacs.dat file can be read
  if (GMACS_files %in% c("gmacs.dat", "all")) {
    Datfile <- file.path(fromDir,
                         "gmacs.dat")
    if (file.exists(Datfile)) {
      gmacsDatfile <- readGMACS.dat(path = Datfile, verbose = verbose)
    } else {
      txt3 <- paste0("The gmacs.dat file is not found in the following directory:\n=>",
                     file.path(fromDir))
      warning(txt3)
      return(FALSE)
    }
  } else {
    gmacsDatfile <- NULL
  }
  if(verbose){
    cat("- Copying GMACS input file(s) from:\n")
    cat("\t=>", fromDir, "\n to: \n\t=>", toDir, "\n")
  }

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

    file_cop <- c("gmacs.dat", gmacsDatfile[["DatFileName"]],
                  gmacsDatfile[["CtlFileName"]], gmacsDatfile[["PrjFileName"]])

    if (do_pinFile && file.exists(file.path(fromDir, "gmacs.pin", fsep = fsep))) {
      cop <- file.copy(
        from = file.path(fromDir, "gmacs.pin"),
        to = file.path(toDir, "gmacs.pin"),
        overwrite = overwrite
      )
      cop_Done <- c(cop_Done, cop)
      file_cop <- c(file_cop, "gmacs.pin")
    }
  } else {
    file_cop <- NULL
    for (f in 1:length(GMACS_files)) {
      copFile <- GMACS_files[f]
      file_cop <- c(file_cop, copFile)

      if (!is.null(gmacsDatfile) && copFile == gmacsDatfile[["CtlFileName"]]) {
        cop <- file.copy(
          from = file.path(fromDir, starter[["CtlFileName"]]),
          to = file.path(toDir, starter[["CtlFileName"]]),
          overwrite = overwrite
        )
        cop_Done <- c(cop_Done, cop)
      } else if (!is.null(gmacsDatfile) && copFile == gmacsDatfile[["DatFileName"]]) {
        cop <- file.copy(
          from = file.path(fromDir, gmacsDatfile[["DatFileName"]]),
          to = file.path(toDir, gmacsDatfile[["DatFileName"]]),
          overwrite = overwrite
        )
        cop_Done <- c(cop_Done, cop)
      } else if (!is.null(gmacsDatfile) && copFile == gmacsDatfile[["PrjFileName"]]) {
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
      } else if (do_pinFile && copFile == "gmacs.pin") {
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
  if(any(!cop_Done)){
    txt4 <- paste0(
      "At least 1 file failed to copy. The following file were not copied to:\n",
      toDir, "\n",
      paste("\t-", file_cop[!cop_Done], collapse = "\n"))
  } else {
    if(verbose){
      cat("\n The following files have been copied:\n")
      cat(paste("\t-", file_cop, collapse ="\n"))
    }
  }
  return(file_cop)
}
# ----------------------------------------------------------
