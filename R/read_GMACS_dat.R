#' @title Read the gmacs.dat file
#'
#' @description Read the gmacs.dat file.
#
#' @param path - path to the gmacs.dat file
#' @param verbose - (TRUE/FALSE); flag to print processing information
#'
#' @return the gmacs.dat file as a named list.
#'
#' @seealso \code{\link{readGMACSdat}},\code{\link{readGMACSctl}},
#' \code{\link{readGMACSprj}}
#'
#' @export
#' @md
#
readGMACS.dat <- function(path = NULL, verbose = TRUE) {
  DatOut <- list()
  # 1- Internal functions
  # -------------------------------------------------------------------------
  # @title get.num
  #
  # @description Extract a numeric value at a specific location.
  #
  # @param dat the object in which the value is searched
  # @param Loc the position of the value
  #
  # @return the value and increment Loc in the parent environment.
  #
  get.num <- function(dat, Loc, num = TRUE) {
    assign("Loc", Loc + 1, parent.frame())
    if (!num) {
      dat[Loc]
    } else {
      .an(dat[Loc])
    }
  }

  # @title get.vec
  #
  # @description Extract a vector at a specific location.
  #
  # @param dat the object in which the value is searched
  # @param Loc the position of the value
  #
  # @return the vector and increment Loc in the parent environment.
  #
  get.vec <- function(dat, Loc, num = TRUE) {
    assign("Loc", Loc + 1, parent.frame())
    # Split by whitespace and collapse (+)
    vec <- strsplit(dat[Loc], "[[:blank:]]+")
    if (!num) {
      vec
    } else {
      .an(vec[[1]])
    }
  }

  # 2- Prepare the data to work on
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("-- Reading gmacs.dat file \n")
    cat("====================================================\n")
  }

  dat <- readLines(path)
  # Remove any preceeding whitespace on all lines.
  dat <- gsub("^[[:blank:]]+", "", dat)
  # Remove all comments.
  dat <- gsub("#.*", "", dat)
  # Remove trailing whitespace on all lines
  dat <- gsub("[[:blank:]]+$", "", dat)
  # Remove blank lines.
  dat <- dat[dat != ""]
  # Initialize the location index
  Loc <- 1

  # Key GMACS files
  # -------------------------------------------------------------------------
  DatOut[["DatFileName"]] <-
    get.num(dat, Loc, num = FALSE) # .dat file
  DatOut[["CtlFileName"]] <-
    get.num(dat, Loc, num = FALSE) # .ctl file
  DatOut[["PrjFileName"]] <-
    get.num(dat, Loc, num = FALSE) # .prj file
  if (verbose)
    cat("-> Read names of GMACS files\n")

  DatOut[["Jitter"]] <-
    get.vec(dat, Loc) # Jitter specifications (1:yes / SD)
  if (verbose)
    cat("-> Read jitter specifications\n")

  # Output variance
  DatOut[["OutVar_BRPs"]] <-
    get.num(dat, Loc) # Biological reference points
  DatOut[["OutVar_Recr"]] <- get.num(dat, Loc) # Recruits
  DatOut[["OutVar_SSB"]] <-
    get.num(dat, Loc) # Spawning Stock Biomass
  DatOut[["OutVar_Fbar"]] <-
    get.num(dat, Loc) # Mean fishing mortality rate
  DatOut[["OutVar_DynB0"]] <- get.num(dat, Loc) # Dynamics B0
  if (verbose)
    cat("-> Read Output variance specifications\n")

  # RETROSPECTIVE ANALYSIS
  DatOut[["N_Year_Retro"]] <-
    get.num(dat, Loc) # Number of year to do it
  if (verbose)
    cat("-> Read Number of year for the retrospective analysis\n")

  # End of data file
  # -------------------------------------------------------------------------
  eof <- get.num(dat, Loc)
  if (verbose) {
    cat("====================================================\n")
    cat("Read of gmacs.dat file complete. Final value = ", eof, "\n")
    cat("\n")
  }

  DatOut[["eof"]] <- FALSE
  if (eof == 9999)
    DatOut[["eof"]] <- TRUE
  return(DatOut)
}
