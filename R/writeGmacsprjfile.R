#' @title Write the projection file
#'
#' @description Write a new Spc.prj file. This function is used to modify within
#' R a pre-existent Spc.prj file.
#'
#' @param Dir (character string)- path where to save the new Spc.prj file
#' @param FileName (character string)- name of the new Spc.prj file
#' @param PrjFile (character string)- Object (list) containing the ex Spc.prj file - The list is
#' created using the [readGMACSprj()] function.
#' @param stock (character string)- name of the stock of interest
#' @param model_name (character string)- name of the model currently considered (e.g., "model 22.A")
#' @param Ass_Year (character string)- Year of this assessment.
#' @param DirTPL (character string)- the directory where the gmacsbase.TPL file
#' you are using for the stock assessment is hold. This is used to identify the
#' version of Gmacs.
#' @param Ver_number (character)- The version of Gmacs. This is used only when
#' developing new code versions of Gmacs.
#'
#' @return create a new .prj file.
#'
#' @seealso \code{\link{readGMACSprj}}
#'
#' @export
#' @md
#
writeGmacsprjfile <- function(Dir = NULL,
                              FileName = NULL,
                              PrjFile = NULL,
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
    tmp <- GMACSversion(DirTPL = DirTPL)
    Ver <- stringr::str_squish(tmp$ver)
    Comp <- tmp$Comp
  }

  obj <- PrjFile

  base::sink(FileName)
  cat("# ============================================================ #\n")
  cat("#                    GMACS main projection file \n")
  cat("# \n")
  cat("#_*** \n")
  cat(stringr::str_squish(string = paste0("#_", Ver)), "\n")
  if(is.null(Ver_number)){
    cat(stringr::str_squish(string = paste0("#_Last GMACS mofification made by: ", Comp)), "\n")
  } else {
    cat(stringr::str_squish(string = paste0("#", Comp)), "\n")
  }
  cat(stringr::str_squish(string = paste0("#_Date of writing the projection file: ", .ac(Sys.time()))), "\n")
  cat("#_*** \n")
  cat("# \n")
  cat(stringr::str_squish(string = paste0("#_Stock of interest: ", stock)), "\n")
  cat(stringr::str_squish(string = paste0("#_Model name: ", model_name)), "\n")
  cat(stringr::str_squish(string = paste0("#_Year of assessment: ", Ass_Year)), "\n")
  cat("# ============================================================ #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_References controls (Spawning per recruit specifications)\n")
  cat("#_-------------------------------------- #\n")
  cat(obj$Calc_MSY, "#_Should the MSY be calculated (0 = No; 1 = Yes)\n")
  cat(obj$Ffixed, "#_Mortality rate applied to each fishery (0= F35%; 1= F is fixed)\n")
  cat(c(obj$spr_syr,obj$spr_nyr),"#_First and last years for average recruitment/MMB for Bspr calculation\n")
  cat(c(obj$spr_SexR_syr,obj$spr_SexR_nyr), "#_First and last years for computing the sex ratio used in the calculation of the BRPs\n")
  cat(c(obj$spr_aveF_syr,obj$spr_aveF_nyr), "#_First and last years for computing the average fishing mortality for discards\n")
  cat(c(obj$spr_M_syr,obj$spr_M_nyr), "#_First and last years for computing the natural mortality\n")
  cat(c(obj$spr_Prop_syr,obj$spr_Prop_nyr), "#_First and last years for computing season lengths\n")
  cat(obj$spr_grow_yr, "#_First year for computing growth\n")
  cat(c(obj$spr_sel_syr,obj$spr_sel_nyr), "#_First and last year for computing the average vulnerability\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_OFL specifications\n")
  cat("# -------------------------------------- #\n")
  cat(obj$spr_target, "#_Target SPR ratio for Bmsy proxy\n")
  cat(obj$OFLTier, "#_Tier system\n")
  cat(obj$OFLalpha, "#_Alpha (i.e., cut-off)\n")
  cat(obj$OFLbeta, "#_Beta (i.e., limit)\n")
  cat(obj$OFLgamma, "#_Gamma\n")
  cat(obj$ABCBuffer, "#_ABC-OFL buffer\n")
  cat(obj$Compute_yield_prj, "#_(0 = No; 1 = year) for whether the yield function should be reported\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_Projection specifications\n")
  cat("# -------------------------------------- #\n")
  cat(obj$pyr, "#_Last year of the projection period\n")
  cat(obj$prj_type, "#_Projection type (1 = Constant F; 2 = proportion of current F)\n")
  cat(obj$prj_Nstrat, "#_Number of strategies considered in the projections\n")
  if(obj$prj_Nstrat == 0){
    cat("\n")
  } else {
    cat(obj$prj_Frange,"#_Range of F values for the strategies (empty if 0 strategies is considered (see previous line))\n")
  }
  cat(obj$prj_bycatch_on, "#_Allow for bycatch fleets to have non-zero mortality\n")
  cat(obj$prj_replicates, "#_How many times each MCMC draw is run\n")
  cat(obj$Fixed_prj_Bmsy, "#_Should Bmsy be fixed?\n")

  cat(c(obj$proj_syr,obj$proj_nyr),"#_First and last years for computing the average recruitment\n")
  cat(c(obj$proj_SexR_syr,obj$proj_SexR_nyr),"#_First and last years for computing the average sex ratio\n")
  cat(c(obj$proj_aveF_syr,obj$proj_aveF_nyr),"#_First and last years for computing the average fishing mortality for discards\n")
  cat(c(obj$proj_M_syr,obj$proj_M_nyr),"#_First and last years for computing the natural mortality\n")
  cat(c(obj$proj_Prop_syr,obj$proj_Prop_nyr),"#_First and last years for computing season lengths\n")
  cat(obj$proj_grow_yr, "#_Year for specifying growth in the projections\n")
  cat(c(obj$proj_sel_syr,obj$proj_sel_nyr),"#_First and last year for computing the average vulnerability\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_Recruitment specifications\n")
  cat("# -------------------------------------- #\n")
  cat(obj$Stock_rec_prj, "#_Stock-recruitment option (1=Mean Rec;2=Ricker;3=Beverton-Holt;4=Mean recruitment)\n")
  cat(obj$Age_at_rec_prj, "#_Time (age) to recruitment\n")
  cat(c(obj$prj_futRec_syr,obj$prj_futRec_nyr),"#_First and last year for generating recruitment\n")
  cat(obj$mean_rec_prj, "#_Mean recruitment for projections\n")
  cat(obj$SigmaR_prj, "#_Sigma used to compute the recruitment\n")
  cat(obj$Prow_prj, "#_Prow(R)\n")
  cat(obj$Initial_eps, "#_First recruitment deviation\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_Specifying State strategies\n")
  cat("# -------------------------------------- #\n")
  cat(obj$Apply_HCR_prj, "#_Apply strategies [OFL, ABC] (1 = apply HCR; 0 = constant F)\n")
  cat(obj$Apply_StateStrat_prj, "#_Apply the state strategy (1 = yes;0 = no)\n")
  cat(obj$Nb_state_param, "#_Number of state parameters\n")
  cat(obj$MeanWStateMature, "#_Mean weight to use - mature individuals\n")
  cat(obj$MeanWStateLegal, "#_Mean weight to use (legal)\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_Run specificities\n")
  cat("# -------------------------------------- #\n")
  cat(obj$max_prj, "#_Stop after XX mcdraws\n")
  cat(obj$full_prj_diag, "#_Full diagnostics (0 = No; 1 = Yes)\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_End of projection file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")

  base::sink()
}
