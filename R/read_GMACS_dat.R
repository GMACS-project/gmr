#' @title Read the gmacs.dat file
#'
#' @description Read the gmacs.dat file.
#
#' @param path - path to the gmacs.dat file
#' @param verbose - (TRUE/FALSE); flag to print processing information
#'
#' @return A list with the content of the gmacs.dat file.
#' \itemize{
#'   \item \code{DatFileName} - The name of the data input file (e.g., model.dat).
#'   \item \code{CtlFileName} - The name of the control input file (e.g., model.ctl).
#'   \item \code{PrjFileName} - The name of the projection input file (e.g., model.prj).
#'   \item \code{Weight_Unit} - The unit use for the weight data.
#'   \item \code{Number_Unit} - The unit use for the numbers data.
#'   \item \code{Stock_name} - The name of the stock considered.
#'   \item \code{Jitter} - The jitter configuration (Yes/No).
#'   \item \code{sd_Jitter} - The sd used in the jitter (if applicable).
#'   \item \code{OutVar_BRPs} - Output variance for the Biological Reference Points.
#'   \item \code{OutVar_Recr} - Output variance for the recruitment.
#'   \item \code{OutVar_SSB} - Output variance for the Spawning Stock Biomass.
#'   \item \code{OutVar_Fbar} -   Output variance for the mean fishing mortality.
#'   \item \code{OutVar_DynB0} - Output variance for the dynamic B0 approach.
#'   \item \code{N_Year_Retro} - The number of year for the retrospective analysis.
#'   \item \code{TurnOffPhase} - The maximum number of phase(s) (i.e., stop the estimation after this phase)
#'   \item \code{StopAfterFnCall} - The number of function call.
#'   \item \code{CalcRefPoints} - Specify if the reference points are computed during the run.
#'   \item \code{UsePinFile} - Use the .pin file to specify (initial) values for parameters.
#'   \item \code{Verbose} - Verbose flag.
#'   \item \code{eof} - Logical indicating the end of the file (used for checking the reading).
#' }
#'
#' @seealso \code{\link{readGMACSdat}},\code{\link{readGMACSctl}},
#' \code{\link{readGMACSprj}}
#'
#' @examples
#' \dontrun{
#' # Stock ----
#' stock <- "SNOW_crab"
#' # read gmacs.dat ----
#' fileName <- "gmacs.dat"
#' fileName <- file.path(dir_Base, stock, fileName, fsep = fsep)
#' GMACSdat <- readGMACS.dat(path = fileName, verbose = TRUE)
#' }
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
  DatOut[["DatFileName"]] <-
    get.num(dat, Loc, num = FALSE) # .dat file
  DatOut[["CtlFileName"]] <-
    get.num(dat, Loc, num = FALSE) # .ctl file
  DatOut[["PrjFileName"]] <-
    get.num(dat, Loc, num = FALSE) # .prj file
  if (verbose)
    cat("-> Read names of GMACS files\n")

  # Stock specifications
  DatOut[["Weight_Unit"]] <-
    get.num(dat, Loc, num = FALSE) # Weight unit
  DatOut[["Number_Unit"]] <-
    get.num(dat, Loc, num = FALSE) # Number unit
  DatOut[["Stock_name"]] <-
    get.num(dat, Loc, num = FALSE) # Stock name

  # Jitter specifications (1:yes / SD)
  # DatOut[["Jitter"]] <-
  #   get.vec(dat, Loc) # Jitter specifications (1:yes / SD)
  DatOut[["Jitter"]] <-
    get.num(dat, Loc) # Is Jittered?
  DatOut[["sd_Jitter"]] <-
    get.num(dat, Loc) # sd Jittered
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

  # Other controls
  DatOut[["TurnOffPhase"]] <-
    get.num(dat, Loc) # Maximum phase (stop the estimation after this phase)
  DatOut[["StopAfterFnCall"]] <-
    get.num(dat, Loc) # Maximum number of function calls, if 1, stop at fn1 call;
  # if -1 run as long as it takes
  DatOut[["CalcRefPoints"]] <-
    get.num(dat, Loc) # Calculate reference points (0:no, 1: yes)
  DatOut[["UsePinFile"]] <-
    get.num(dat, Loc) # Use pin file (0:normal run; 1:yes-set parameter values)
  DatOut[["Verbose"]] <-
    get.num(dat, Loc) # VERBOSE flag (0: off, 1: on, 2: objective func; 3: diagnostics)

  # End of data file
  # -------------------------------------------------------------------------
  eof <- get.num(dat, Loc)

  if (eof != 9999) {
    cat("\n\nSomething went wrong while reading the gmacs.dat file !!\n")
    stop()
  }

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
