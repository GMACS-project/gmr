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
#' @param Ver_number (character)- The version of Gmacs. This is used only when
#' developing new code versions of Gmacs.
#'
#' @return create a new gmacs.dat file.
#'
#' @seealso \code{\link{readGMACS.dat}}
#'
#' @export
#' @md
#'
writeGmacs.dat <- function(Dir = NULL,
                           FileName = NULL,
                           gmacsDat = NULL,
                           stock = "",
                           model_name = "",
                           Ass_Year = "",
                           DirTPL = NULL,
                           Ver_number = NULL) {
  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # Get GMACS version number and compilation date
  if(!is.null(Ver_number)){
    Ver <- paste0("GMACS Version: ",Ver_number)
    Comp <- "_Gmacs Development version"
  } else {
    tmp <- GMACSversion(Dir = DirTPL)
    Ver <- stringr::str_squish(tmp$ver)
    Comp <- tmp$Comp
  }

  obj <- gmacsDat

  base::sink(FileName)
  cat("# ============================================================ #\n")
  cat("#                    GMACS main data file \n")
  cat("# \n")
  cat("# *** \n")
  cat(stringr::str_squish(string = paste0("#_", Ver)), "\n")
  if(is.null(Ver_number)){
    cat(stringr::str_squish(string = paste0("#_Last GMACS mofification made by: ", Comp)), "\n")
  } else {
    cat(stringr::str_squish(string = paste0("#", Comp)), "\n")
  }
  cat(stringr::str_squish(string = paste0("#_Date of writing the gmacs.dat file:", .ac(Sys.time()))), "\n")
  cat("# *** \n")
  cat("# \n")
  cat(stringr::str_squish(string = paste0("#_Stock of interest: ", stock)), "\n")
  cat(stringr::str_squish(string = paste0("#_Model name: ", model_name)), "\n")
  cat(stringr::str_squish(string = paste0("#_Year of assessment: ", Ass_Year)), "\n")
  cat("# ============================================================ #\n")
  cat("\n")

  cat("##_Key GMACS files\n")
  cat("# -------------------------------------- #\n")
  cat("#_Data file name\n", sep = "")
  cat(obj$DatFileName, "\n", sep = "")
  cat("#_Control file name\n", sep = "")
  cat(obj$CtlFileName, "\n", sep = "")
  cat("#_Projection file name\n", sep = "")
  cat(obj$PrjFileName, "\n", sep = "")
  cat("\n")

  cat("##_Stock specifications\n")
  cat("# -------------------------------------- #\n")
  cat("#_Weight unit (used for plotting)\n", sep = "")
  cat(obj$Weight_Unit, "\n", sep = "")
  cat("#_Number unit (used for plotting)\n", sep = "")
  cat(obj$Number_Unit, "\n", sep = "")
  cat("#_Stock name (used for plotting)\n", sep = "")
  cat(obj$Stock_name, "\n", sep = "")
  cat("\n")

  cat("##_Jitter specifications\n")
  cat("# -------------------------------------- #\n")
  cat("#_Is Jittered?\n")
  cat(obj$Jitter, "\n")
  cat("#_sd Jitter\n")
  cat(obj$sd_Jitter, "\n")
  cat("\n")

  cat("##_Output variance specifications\n")
  cat("# -------------------------------------- #\n")
  cat("#_Output variance (0 = No; 1 = Yes)\n")
  cat(obj$OutVar_BRPs, "#_Reference points\n")
  cat(obj$OutVar_Recr, "#_Recruits\n")
  cat(obj$OutVar_SSB, "#_Spawning Stock Biomass\n")
  cat(obj$OutVar_Fbar, "#_Mean Fishing mortality (Fbar)\n")
  cat(obj$OutVar_DynB0, "#_Dynamic B0 approach\n")
  cat("\n")

  cat("##_Retrospective analysis specifications\n")
  cat("# -------------------------------------- #\n")
  cat(obj$N_Year_Retro,
      "#_Number of year for the retrospective analysis\n")
  cat("\n")

  cat("##_Other controls\n")
  cat("# -------------------------------------- #\n")
  cat("#_Maximum phase (stop the estimation after this phase)\n")
  cat(obj$TurnOffPhase, "\n")
  cat(
    "#_Maximum number of function calls: if 1, stop at fn1 call;\n# if -1, run as long as it takes\n"
  )
  cat(obj$StopAfterFnCall, "\n")
  cat("#_Calculate reference points (0 = No, 1 = Yes)\n")
  cat(obj$CalcRefPoints, "\n")
  cat("#_Use pin file (0 = normal run; 1 = yes-set parameter values)\n")
  cat(obj$UsePinFile, "\n")
  cat("#_VERBOSE flag (0 = off, 1 = on, 2 = objective function; 3 = diagnostics)\n")
  cat(obj$Verbose, "\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_End of gmacs.dat file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")

  base::sink()
}
