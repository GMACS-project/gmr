#' @title readGMACSprj
#'
#' @description Read the GMACS projection file.
#
#' @param FileName - path (and name) of the projection file (e.g. snow.prj)
#' @param verbose - (TRUE/FALSE); flag to print processing information
#'
#' @return the .prj file as a named list.
#'
#' @seealso \code{\link{readGMACS.dat}},\code{\link{readGMACSdat}},
#' \code{\link{readGMACSctl}}
#'
#' @export
#' @md
#
readGMACSprj <- function(FileName = NULL, verbose =TRUE) {

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
  # -------------------------------------------------------------------------


  # 2- Read the projection file and find the first line containing numeric data
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("-- Reading projection file \n")
    cat("====================================================\n")
  }

  dat <- readLines(FileName, warn = FALSE)

  Startdat <- which(stringr::str_detect(dat, "^\\#.*") == F)[1]
  Com <-
    grep(x = dat[seq_len(Startdat - 1)],
         pattern = "^#",
         value = TRUE)
  # -------------------------------------------------------------------------

  # 3- Prepare the data to work on
  # -------------------------------------------------------------------------
  # Remove any preceeding whitespace on all lines.
  dat <- gsub("^[[:blank:]]+", "", dat)
  # Remove all comments.
  dat <- gsub("#.*", "", dat)
  # Remove trailing whitespace on all lines
  dat <- gsub("[[:blank:]]+$", "", dat)
  # Remove blank lines.
  dat <- dat[dat != ""]

  DatOut[["sourcefile"]] <- FileName
  if(length(Com)== 0  || Com =="# references")
    Com <- ""
  DatOut[["Comments"]] <- Com

  # Initialize the location index
  Loc <- 1
  # -------------------------------------------------------------------------

  # References controls (Spawning per recruit specifications)
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading References controls (Spawning per recruit specifications) \n")

  DatOut[["Calc_MSY"]] <- get.num(dat, Loc) # Compute MSY (0:no; 1:yes)
  DatOut[["Ffixed"]] <- get.vec(dat, Loc) # mortality rate applied to each fishery (0: F35%; 1: Fis fixed)
  # First and last years for average recruitment/MMB for Bspr calculation
  tmp <- get.vec(dat, Loc)
  DatOut[["spr_syr"]] <- tmp[1]
  DatOut[["spr_nyr"]] <- tmp[2]
  # First and last years for computing the sex ratio used in the calculation of the BRPs
  tmp <- get.vec(dat, Loc)
  DatOut[["spr_SexR_syr"]] <- tmp[1]
  DatOut[["spr_SexR_nyr"]] <- tmp[2]
  # First and last years for computing the average fishing mortality for discards
  tmp <- get.vec(dat, Loc)
  DatOut[["spr_aveF_syr"]] <- tmp[1]
  DatOut[["spr_aveF_nyr"]] <- tmp[2]
  # First and last years for computing the natural mortality
  tmp <- get.vec(dat, Loc)
  DatOut[["spr_M_syr"]] <- tmp[1]
  DatOut[["spr_M_nyr"]] <- tmp[2]
  # First and last years for computing season lengths
  tmp <- get.vec(dat, Loc)
  DatOut[["spr_Prop_syr"]] <- tmp[1]
  DatOut[["spr_Prop_nyr"]] <- tmp[2]
  # First year for computing growth
  tmp <- get.vec(dat, Loc)
  DatOut[["spr_grow_yr"]] <- tmp[1]
  # First and last year for computing the average vulnerability
  tmp <- get.vec(dat, Loc)
  DatOut[["spr_sel_syr"]] <- tmp[1]
  DatOut[["spr_sel_nyr"]] <- tmp[2]

  if (verbose)
    cat("-> Read References controls  \n")
  # -------------------------------------------------------------------------

  # OFL specifications
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading OFL specifications controls \n")

  DatOut[["spr_target"]] <- get.num(dat, Loc) # Target SPR ratio for Bmsy proxy
  DatOut[["OFLTier"]] <- get.num(dat, Loc) # Tier system
  DatOut[["OFLalpha"]] <- get.num(dat, Loc) # Alpha (i.e., cut-off)

  DatOut[["OFLbeta"]] <- get.num(dat, Loc) # Beta (i.e., limit)
  DatOut[["OFLgamma"]] <- get.num(dat, Loc) # Gamma
  DatOut[["ABCBuffer"]] <- get.num(dat, Loc) # ABC-OFL buffer
  DatOut[["Compute_yield_prj"]] <- get.num(dat, Loc) # (0: no; 1: year) for whether the yield function should be reported
                                                     # if MSY is not computed, Compute_yield_prj = 0
  if (verbose)
    cat("-> Read OFL specifications controls  \n")
  # -------------------------------------------------------------------------

  # Projection specifications
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading Projection specifications controls \n")

  DatOut[["pyr"]] <- get.num(dat, Loc) # Last year of the projection period
  DatOut[["prj_Nstrat"]] <- get.num(dat, Loc) # Number of strategies considered in the projections
  # Range of F values
  tmp <- get.vec(dat, Loc)
  DatOut[["prj_lowF"]] <- tmp[1]
  DatOut[["prj_hiF"]] <- tmp[2]
  DatOut[["prj_bycatch_on"]] <- get.num(dat, Loc) # Allow for bycatch fleets to have non-zero mortality
  DatOut[["prj_replicates"]] <- get.num(dat, Loc) # How many times each MCMC draw is run
  DatOut[["Fixed_prj_Bmsy"]] <- get.num(dat, Loc) # Should Bmsy be fixed?
  # First and last years for computing the average recruitment
  tmp <- get.vec(dat, Loc)
  DatOut[["proj_syr"]] <- tmp[1]
  DatOut[["proj_nyr"]] <- tmp[2]
  # First and last years for computing the average sex ratio
  tmp <- get.vec(dat, Loc)
  DatOut[["proj_SexR_syr"]] <- tmp[1]
  DatOut[["proj_SexR_nyr"]] <- tmp[2]
  # First and last years for computing the average fishing mortality for discards
  tmp <- get.vec(dat, Loc)
  DatOut[["proj_aveF_syr"]] <- tmp[1]
  DatOut[["proj_aveF_nyr"]] <- tmp[2]
  # First and last years for computing the natural mortality
  tmp <- get.vec(dat, Loc)
  DatOut[["proj_M_syr"]] <- tmp[1]
  DatOut[["proj_M_nyr"]] <- tmp[2]
  # First and last years for computing season lengths
  tmp <- get.vec(dat, Loc)
  DatOut[["proj_Prop_syr"]] <- tmp[1]
  DatOut[["proj_Prop_nyr"]] <- tmp[2]
  # Year for specifying growth in the projections
  tmp <- get.vec(dat, Loc)
  DatOut[["proj_grow_yr"]] <- tmp[1]
  # First and last year for computing the average vulnerability
  tmp <- get.vec(dat, Loc)
  DatOut[["proj_sel_syr"]] <- tmp[1]
  DatOut[["proj_sel_nyr"]] <- tmp[2]

  if (verbose)
    cat("-> Read Projection specifications controls  \n")
  # -------------------------------------------------------------------------

  # Recruitment specifications
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading (stock)-recruitment specifications controls \n")

  DatOut[["Stock_rec_prj"]] <- get.num(dat, Loc) # Stock-recruitment option (1=Mean Rec;2=Ricker;3=Beverton-Holt;4=Mean recruitment)
  DatOut[["Age_at_rec_prj"]] <- get.num(dat, Loc) # Time (age) to recruitment
  # First and last year for generating recruitment
  tmp <- get.vec(dat, Loc)
  DatOut[["prj_futRec_syr"]] <- tmp[1]
  DatOut[["prj_futRec_nyr"]] <- tmp[2]
  DatOut[["mean_rec_prj"]] <- get.num(dat, Loc) # Mean recruitment for projections
  DatOut[["SigmaR_prj"]] <- get.num(dat, Loc) # Sigma used to compute the recruitment
  DatOut[["Prow_prj"]] <- get.num(dat, Loc) # Prow(R) ??????????????
  DatOut[["Initial_eps"]] <- get.num(dat, Loc) # First recruitment deviation

  if (verbose)
    cat("-> Read (stock)-recruitment specifications controls  \n")
  # -------------------------------------------------------------------------


  # Specifying State strategies
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading Harvest control rules (state strategy) specifications controls \n")

  DatOut[["Apply_HCR_prj"]] <- get.num(dat, Loc) # Apply strategies [OFL, ABC] (1=yes;0=no)
  DatOut[["MeanWStateMature"]] <- get.num(dat, Loc) # Mean weight to use - mature individuals
  DatOut[["MeanWStateLegal"]] <- get.num(dat, Loc) # Mean weight to use (legal)

  if (verbose)
    cat("-> Read State strategy specifications controls  \n")
  # -------------------------------------------------------------------------

  # Run specificities
  # -------------------------------------------------------------------------
  DatOut[["max_prj"]] <- get.num(dat, Loc) # Stop after XX mcdraws
  DatOut[["full_prj_diag"]] <- get.num(dat, Loc) # Full diagnostics (0: no; 1:yes)
  # -------------------------------------------------------------------------

  # End of data file
  # -------------------------------------------------------------------------
  eof <- get.num(dat, Loc)
  if(eof != 9999){
    cat("\n\nSomething went wrong while reading the projection file !!\n")
    stop()
  }
  if (verbose) {
    cat("====================================================\n")
    cat("Read of projection file complete. Final value = ", eof, "\n")
    cat("\n")
  }
  DatOut[["eof"]] <- FALSE
  if (eof == 9999)
    DatOut[["eof"]] <- TRUE

  return(DatOut)
}
