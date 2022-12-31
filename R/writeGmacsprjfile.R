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
#' @param Ass_Year (character string)- Year of this assessment
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
                              Ass_Year = "") {

  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # Get GMACS version number and compilation date
  tmp <- GMACSversion(Dir = Dir)
  Ver <- tmp$ver
  Comp <- tmp$Comp

  obj <- PrjFile

  base::sink(FileName)
  cat("# ============================================================ #\n")
  cat("#                    GMACS main projection file \n")
  cat("# *** \n")
  cat("#", Ver, "\n")
  cat("#", Comp, "\n")
  cat("# *** \n")
  cat("# Stock of interest: ", stock, "\n")
  cat("# Model name: ", model_name, "\n")
  cat("# Year of assessment: ", Ass_Year, "\n")
  cat("# ============================================================ #\n")
  cat("\n")



  cat("# -------------------------------------- #\n")
  cat("## References controls (Spawning per recruit specifications)\n")
  cat("# -------------------------------------- #\n")
  cat(obj$Calc_MSY, "# Should the MSY be calculated (0 = No; 1 = Yes)\n")
  cat(obj$Ffixed, "# Mortality rate applied to each fishery (0= F35%; 1= F is fixed)\n")
  cat(c(obj$spr_syr,obj$spr_nyr),"# First and last years for average recruitment/MMB for Bspr calculation\n")
  cat(c(obj$spr_SexR_syr,obj$spr_SexR_nyr), "# First and last years for computing the sex ratio used in the calculation of the BRPs\n")
  cat(c(obj$spr_aveF_syr,obj$spr_aveF_nyr), "# First and last years for computing the average fishing mortality for discards\n")
  cat(c(obj$spr_M_syr,obj$spr_M_nyr), "# First and last years for computing the natural mortality\n")
  cat(c(obj$spr_Prop_syr,obj$spr_Prop_nyr), "# First and last years for computing season lengths\n")
  cat(obj$spr_grow_yr, "# First year for computing growth\n")
  cat(c(obj$spr_sel_syr,obj$spr_sel_nyr), "# First and last year for computing the average vulnerability\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## OFL specifications\n")
  cat("# -------------------------------------- #\n")
  cat(obj$spr_target, "# Target SPR ratio for Bmsy proxy\n")
  cat(obj$OFLTier, "# Tier system\n")
  cat(obj$OFLalpha, "# Alpha (i.e., cut-off)\n")
  cat(obj$OFLbeta, "# Beta (i.e., limit)\n")
  cat(obj$OFLgamma, "# Gamma\n")
  cat(obj$ABCBuffer, "# ABC-OFL buffer\n")
  cat(obj$Compute_yield_prj, "# (0 = No; 1 = year) for whether the yield function should be reported\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Projection specifications\n")
  cat("# -------------------------------------- #\n")
  cat(obj$pyr, "# Last year of the projection period\n")
  cat(obj$prj_Nstrat, "# Number of strategies considered in the projections\n")
  cat(c(obj$prj_lowF,obj$prj_hiF),"# Range of F values\n")
  cat(obj$prj_bycatch_on, "# Allow for bycatch fleets to have non-zero mortality\n")
  cat(obj$prj_replicates, "# How many times each MCMC draw is run\n")
  cat(obj$Fixed_prj_Bmsy, "# Should Bmsy be fixed?\n")

  cat(c(obj$proj_syr,obj$proj_nyr),"# First and last years for computing the average recruitment\n")
  cat(c(obj$proj_SexR_syr,obj$proj_SexR_nyr),"# First and last years for computing the average sex ratio\n")
  cat(c(obj$proj_aveF_syr,obj$proj_aveF_nyr),"# First and last years for computing the average fishing mortality for discards\n")
  cat(c(obj$proj_M_syr,obj$proj_M_nyr),"# First and last years for computing the natural mortality\n")
  cat(c(obj$proj_Prop_syr,obj$proj_Prop_nyr),"# First and last years for computing season lengths\n")
  cat(obj$proj_grow_yr, "# Year for specifying growth in the projections\n")
  cat(c(obj$proj_sel_syr,obj$proj_sel_nyr),"# First and last year for computing the average vulnerability\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Recruitment specifications\n")
  cat("# -------------------------------------- #\n")
  cat(obj$Stock_rec_prj, "# Stock-recruitment option (1=Mean Rec;2=Ricker;3=Beverton-Holt;4=Mean recruitment)\n")
  cat(obj$Age_at_rec_prj, "# Time (age) to recruitment\n")
  cat(c(obj$prj_futRec_syr,obj$prj_futRec_nyr),"# First and last year for generating recruitment\n")
  cat(obj$mean_rec_prj, "# Mean recruitment for projections\n")
  cat(obj$SigmaR_prj, "# Sigma used to compute the recruitment\n")
  cat(obj$Prow_prj, "# Prow(R)\n")
  cat(obj$Initial_eps, "# First recruitment deviation\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Specifying State strategies\n")
  cat("# -------------------------------------- #\n")
  cat(obj$Apply_HCR_prj, "# Apply strategies [OFL, ABC] (1=yes;0=no)\n")
  cat(obj$MeanWStateMature, "# Mean weight to use - mature individuals\n")
  cat(obj$MeanWStateLegal, "# Mean weight to use (legal)\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Run specificities\n")
  cat("# -------------------------------------- #\n")
  cat(obj$max_prj, "# Stop after XX mcdraws\n")
  cat(obj$full_prj_diag, "# Full diagnostics (0 = No; 1 = Yes)\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## End of projection file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")

  base::sink()
}
