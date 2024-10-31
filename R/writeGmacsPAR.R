#' @title Write the gmacs.par file
#'
#' @description Write a new gmacs.par file. This function is used to modify within
#' R a pre-existent gmacs.par file.
#'
#' @param Dir (character string)- path where to save the new gmacs.par file
#' @param FileName (character string)- name of the new gmacs.par file
#' @param gmacsPar (character string)- Object (list) containing the ex gmacs.par file - The list is
#' created using the [readGMACSpar()] function.
#'
#' @return create a new gmacs.dat file.
#'
#' @seealso \code{\link{readGMACSpar}}
#'
#' @examples
#' \dontrun{
#' # Stock ----
#' stock <- "SNOW_crab"
#' # GMACS input files ----
#' datfileName <- "snow_21_M09.dat"
#' ctlfileName <- "snow_21_M09.ctl"
#'
#' # read gmacs.dat ----
#' fileName <- "gmacs.dat"
#' fileName <- file.path(dir_Base, stock, fileName, fsep = fsep)
#' GMACSdat <- readGMACS.dat(path = fileName, verbose = TRUE)
#' # Read the data file ----
#' datFile <- file.path(dir_Base, stock, datfileName, fsep = fsep)
#' datFile <- readGMACSdat(FileName = datFile, verbose = T)
#' # Read the control file ----
#' ctlFile <- file.path(dir_Base, stock, ctlfileName, fsep = fsep)
#' ctlFile <- readGMACSctl(
#'   FileName = ctlFile,
#'   verbose = T,
#'   DatFile = datFile,
#'   nyrRetro = GMACSdat$N_Year_Retro
#' )
#'
#' # Read the gmacs.par file ----
#' GMACSparfile <- readGMACSpar(
#' Dir =file.path(Dir_Dvpt_Vers, "build", stock, fsep = fsep),
#' FileName = "gmacs.par",
#' verbose = TRUE,
#' DatFile = datFile,
#' CtlFile = ctlFile,
#' GMACSdat = GMACSdat
#' )
#'
#' # Write the gmacs.par file as a pin file
#' writeGmacsPAR(
#' Dir = file.path(Dir_Dvpt_Vers, "build", stock, fsep = fsep),
#' FileName = "gmacs.pin",
#' gmacsPar = gmacs_par
#' )
#'
#' }
#' @export
#' @md
#
writeGmacsPAR <- function(Dir = NULL,
                          FileName = NULL,
                          gmacsPar = NULL) {
  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # 1. Internal function ----
  writeDF <- function(dat) {
    for (t in 1:dim(dat)[1]) {
      cat(paste0("# ", unlist(paste0(
        dat[t, "Param_ID"], " -- ", dat[t, "Param_name"]
      )), ":\n"))
      cat(base::sprintf("%.12f",unlist(dat[t, "value"])), "\n")
    }
  }
  writeLIST <- function(ls) {
    for (l in 1:length(ls)) {
      cat(paste0("# ----", unlist(names(ls[l])), ":----\n"))
      tmp <- ls[[l]]
      for (t in 1:dim(tmp)[1]) {
        cat(paste0("# ", unlist(tmp[t, "Param_name"])), ":\n")
        cat(base::sprintf("%.12f",.an(unlist(tmp[t, "value"]))), "\n")
      }
    }
  }
  # --------------------------------------

  obj <- gmacsPar

  base::sink(FileName)

  cat("# ============================================================ #\n")
  cat("#_Number of parameters: ", obj$Nparams, "\n")
  cat("#_Objective function value: ", base::sprintf("%.12f",.an(obj$ObjFvalue)), "\n")
  cat("#_Maximum gradient component: ", base::sprintf("%.12f",.an(obj$MaxGradComp)), "\n")
  cat("# ============================================================ #\n")
  cat("\n")

  cat("#_theta parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$theta)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Growth parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Grwth)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Vulnerability parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Vul)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Asymptotic retention parameters: \n")
  cat("# -------------------------------------- #\n")
  if(!is.null(obj$Asympt)){
    writeDF(dat = obj$Asympt)
  } else {
    cat("\n")
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Environmental parameters: \n")
  cat("# -------------------------------------- #\n")
  cat("# vulnerability: \n")
  writeDF(dat = obj$Envpar_Slx)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Vulnerability deviations: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Slx_Devs)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Mean fishing mortality rate parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Fbar)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Mean fishing mortality rate deviations: \n")
  cat("# -------------------------------------- #\n")
  writeLIST(ls = obj$Fdev)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Female F offset to male fishing mortality parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Foff)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Female F deviation offset parameters: \n")
  cat("# -------------------------------------- #\n")
  writeLIST(ls = obj$Fdov)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Initial values of recruitment: \n")
  cat("# -------------------------------------- #\n")
  cat("# rec_ini:\n")
  cat(base::sprintf("%.12f",.an(obj$rec_ini$value)), "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Recruitment deviation estimates: \n")
  cat("# -------------------------------------- #\n")
  cat("# rec_dev_est:\n")
  cat(base::sprintf("%.12f",.an(obj$rec_dev_est$value)), "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Sex-ratio recruitment deviation estimates: \n")
  cat("# -------------------------------------- #\n")
  cat("# logit_rec_prop_est:\n")
  cat(base::sprintf("%.12f",.an(obj$logit_rec_prop_est$value)), "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Natural mortality deviation parameters: \n")
  cat("# -------------------------------------- #\n")
  if(!is.null(obj$Mdev)){
    writeDF(dat = obj$Mdev)
  } else {
    cat("\n")
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Maturity specific natural mortality parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$M_mat)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Effective sample size parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$EffSamp_size)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Catchability coefficient parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$survey_Q)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Addtional CV for surveys/indices parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$add_cv)
  cat("# -------------------------------------- #\n")
  cat("\n")


  cat("# -------------------------------------- #\n")
  cat("##_End of parameter file file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")
  base::sink()
}
