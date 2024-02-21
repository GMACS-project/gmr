#' @title Write the gmacs.dat file
#'
#' @description Write a new gmacs.dat file. This function is used to modify within
#' R a pre-existent gmacs.dat file.
#'
#' @param Dir (character string)- directory where to save the new gmacs.dat file.
#' @param FileName (character string)- name of the new gmacs.dat file.
#' @param gmacsDat (character string)- Object (list) containing the ex gmacs.dat
#' file - The list is created using the [readGMACS_dat()] function.
#' @param stock (character string)- name of the stock of interest.
#' @param model_name (character string)- name of the model currently considered (e.g., "model 22.A").
#' @param Ass_Year (character string)- Year of this assessment.
#' @param DirTPL (character string)- the directory where the gmacsbase.TPL file
#' you are using for the stock assessment is hold.
#'
#' @return create a new gmacs.dat file.
#'
#' @seealso \code{\link{readGMACS.dat}}
#'
#' @export
#'
#
writeGmacs.dat <- function(Dir = NULL,
                           FileName = NULL,
                           gmacsDat = NULL,
                           stock = "",
                           model_name = "",
                           Ass_Year = "",
                           DirTPL = NULL) {
  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # Get GMACS version number and compilation date

  tmp <- GMACSversion(Dir = DirTPL)
  # DirTPL <- unlist(strsplit(DirTrue, "build/"))[1]
  # tmp <- GMACSversion(Dir = DirTPL)
  Ver <- tmp$ver
  Comp <- tmp$Comp

  obj <- gmacsDat

  base::sink(FileName)
  cat("# ============================================================ #\n")
  cat("#                    GMACS main data file \n")
  cat("# \n")
  cat("# *** \n")
  cat("#", Ver, "\n")
  cat("# Last GMACS mofification made by: ", Comp, "\n")
  cat("# Date of writing the gmacs.dat file:", .ac(Sys.time()))
  cat("# *** \n")
  cat("# \n")
  cat("# Stock of interest: ", stock, "\n")
  cat("# Model name: ", model_name, "\n")
  cat("# Year of assessment: ", Ass_Year, "\n")
  cat("# ============================================================ #\n")
  cat("\n")

  cat("## Key GMACS files\n")
  cat("# -------------------------------------- #\n")
  cat("# Data file name\n", sep = "")
  cat(obj$DatFileName, "\n", sep = "")
  cat("# Control file name\n", sep = "")
  cat(obj$CtlFileName, "\n", sep = "")
  cat("# Projection file name\n", sep = "")
  cat(obj$PrjFileName, "\n", sep = "")
  cat("\n")

  cat("## Stock specifications\n")
  cat("# -------------------------------------- #\n")
  cat("# Weight unit\n", sep = "")
  cat(obj$Weight_Unit, "\n", sep = "")
  cat("# Stock name\n", sep = "")
  cat(obj$Stock_name, "\n", sep = "")
  cat("\n")

  cat("## jitter specifications\n")
  cat("# -------------------------------------- #\n")
  cat("# Is Jittered?\n")
  cat(obj$Jitter, "\n")
  cat("# sd Jitter?\n")
  cat(obj$sd_Jitter, "\n")
  cat("\n")

  cat("## Output variance specifications\n")
  cat("# -------------------------------------- #\n")
  cat("# Output variance (0 = No; 1 = Yes\n")
  cat(obj$OutVar_BRPs, "# Reference points\n")
  cat(obj$OutVar_Recr, "# Recruits\n")
  cat(obj$OutVar_SSB, "# Spawning Stock Biomass\n")
  cat(obj$OutVar_Fbar, "# Mean Fishing mortality (Fbar)\n")
  cat(obj$OutVar_DynB0, "# Dynamic B0 approach\n")
  cat("\n")

  cat("## Retrospective analysis specifications\n")
  cat("# -------------------------------------- #\n")
  cat(obj$N_Year_Retro,
      "# Number of year for the retrospective analysis\n")
  cat("\n")

  cat("## Other controls\n")
  cat("# -------------------------------------- #\n")
  cat("# Maximum phase (stop the estimation after this phase)\n")
  cat(obj$TurnOffPhase, "\n")
  cat(
    "# Maximum number of function calls, if 1, stop at fn1 call;\n# if -1 run as long as it takes\n"
  )
  cat(obj$StopAfterFnCall, "\n")
  cat("# Calculate reference points (0 = No, 1 = Yes)\n")
  cat(obj$CalcRefPoints, "\n")
  cat("# Use pin file (0 = normal run; 1 = yes-set parameter values)\n")
  cat(obj$UsePinFile, "\n")
  cat("# VERBOSE flag (0 = off, 1 = on, 2 = objective function; 3 = diagnostics)\n")
  cat(obj$Verbose, "\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## End of gmacs.dat file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")

  base::sink()
}
