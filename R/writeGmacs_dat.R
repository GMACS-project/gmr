#' @title Write the gmacs.dat file
#'
#' @description Write a new gmacs.dat file. This function is used to modify within
#' R a pre-existent gmacs.dat file.
#'
#' @param Dir (character string)- path where to save the new gmacs.dat file
#' @param FileName (character string)- name of the new gmacs.dat file
#' @param gmacsDat (character string)- Object (list) containing the ex gmacs.dat file - The list is
#' created using the [readGMACS_dat()] function.
#' @param stock (character string)- name of the stock of interest
#' @param model_name (character string)- name of the model currently considered (e.g., "model 22.A")
#' @param Ass_Year (character string)- Year of this assessment
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
                          Ass_Year = "") {

  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # Get GMACS version number and compilation date

  tmp <- GMACSversion(Dir = Dir)
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
  cat("# Data file name\n")
  cat(obj$DatFileName, "\n")
  cat("# Control file name\n")
  cat(obj$CtlFileName, "\n")
  cat("# Projection file name\n")
  cat(obj$PrjFileName, "\n")
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
  cat(obj$N_Year_Retro, "# Number of year for the retrospective analysis\n")
  cat("\n")


  cat("# -------------------------------------- #\n")
  cat("## End of gmacs.dat file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")

  base::sink()
}
