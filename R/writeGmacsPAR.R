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
#' @export
#' @md
#
writeGmacsPAR <- function(Dir = NULL,
                          FileName = NULL,
                          gmacsPar = NULL) {

  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  obj <- gmacsPar

  # 1. Internal function ----
  writeDF <- function(dat){
    for(t in 1:dim(dat)[1]){
      cat(paste0("# ",unlist(dat[t,1]),":\n"))
      cat(unlist(dat[t,2]), "\n")
    }
  }

  writeLIST <- function(ls){
    for(l in 1:length(ls)){
      cat(paste0("# ",unlist(names(ls[l])),":\n"))
      cat(unlist(ls[[l]]), "\n")
    }
  }
  # --------------------------------------

  base::sink(FileName)


  cat("# ============================================================ #\n")
  cat("# Number of parameters: ", obj$Nparams, "\n")
  cat("# Objective function value: ", obj$ObjFvalue, "\n")
  cat("# Maximum gradient component: ", obj$MaxGradComp, "\n")
  cat("# ============================================================ #\n")
  cat("\n")

  cat("# theta parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$theta)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Growth parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Grwth)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Vulnerability parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Vul)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Asymptotic retention parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Asympt)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Environmental parameters: \n")
  cat("# -------------------------------------- #\n")
  cat("# vulnerability: \n")
  writeDF(dat = obj$Envpar_Slx)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Vulnerability deviations: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Slx_Devs)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Mean fishing mortality rate parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Fbar)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Mean fishing mortality rate deviations: \n")
  cat("# -------------------------------------- #\n")
  writeLIST(ls = obj$Fdev)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Female F offset to male fishing mortality parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Foff)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Female F deviation offset parameters: \n")
  cat("# -------------------------------------- #\n")
  writeLIST(ls = obj$Fdov)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Initial values of recruitment: \n")
  cat("# -------------------------------------- #\n")
  cat("# rec_ini:\n")
  cat(obj$rec_ini, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Recruitment deviation estimates: \n")
  cat("# -------------------------------------- #\n")
  cat("# rec_dev_est:\n")
  cat(obj$rec_dev_est, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Sex-ratio recruitment deviation estimates: \n")
  cat("# -------------------------------------- #\n")
  cat("# logit_rec_prop_est:\n")
  cat(obj$logit_rec_prop_est, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Natural mortality deviation parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$Mdev)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Maturity specific natural mortality parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$M_mat)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Effective sample size parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$EffSamp_size)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Catchability coefficient parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$survey_Q)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# Addtional CV for surveys/indices parameters: \n")
  cat("# -------------------------------------- #\n")
  writeDF(dat = obj$add_cv)
  cat("# -------------------------------------- #\n")
  cat("\n")


  cat("# -------------------------------------- #\n")
  cat("## End of parameter file file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")
  sink()
}
