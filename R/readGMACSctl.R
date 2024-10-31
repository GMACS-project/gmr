#' @title readGMACSctl
#'
#' @description Read the GMACS control file.
#
#' @param FileName (character string)- path (and name (e.g. snow.ctl)) of the control file
#' @param verbose (logical)- (TRUE/FALSE); flag to print processing information
#' @param DatFile (list)- Object containing the .dat file - This is the output
#' of the [readGMACSdat()] function.
#' @param nyrRetro (integer)- Number of year for the retrospective analysis
#'
#' @return A list with the content of the model.ctl file.
#' \itemize{
#'   \item \code{sourcefile} - The file source.
#'   \item \code{Comments} - Specifications about the GMACS version used and the stock
#'   assessed.
#'   \item \code{ntheta} - The number of key parameters control.
#'   \item \code{theta_control} - The matrix pf teh key parameters control.
#'   \item \code{lw_type} - The type of length-weight (LW) relationship.
#'   \item \code{lw_alfa} - The alpha parameter of the LW relationship (if \code{lw_type = 1}).
#'   \item \code{lw_beta} - The beta parameter of the LW relationship (if \code{lw_type = 1}).
#'   \item \code{mean_wt_in} - The vector/matrix of mean weight-at-size (if \code{lw_type = 2,3}).
#'   \item \code{maturity} - The proportion of mature at size by sex.
#'   \item \code{legal_maturity} - The legal definition of the proportion of mature at size by sex.
#'   \item \code{Func_maturity_TermMolting} - Use functional maturity for terminally molting animals?
#'   \item \code{bUseCustomGrowthMatrix} - The selected option for the growth matrix.
#'   \item \code{bUseGrowthIncrementModel} - The selected option for the growth increment model.
#'   \item \code{bUseCustomMoltProbability} - The selected molt probability function.
#'   \item \code{bUseCustomMatureProbability} - The selected option for the maturity probability
#'   (only applicable if nmature = 2)
#'   \item \code{nSizeClassRec} - The maximum size-class for recruitment
#'   \item \code{nSizeIncVaries} - The number of size increment periods
#'   \item \code{iYrsSizeIncChanges} - The year(s) for which the size increment change.
#'   \item \code{nMoltVaries} - The number of molt periods.
#'   \item \code{iYrsMoltChanges} - The year(s) for which molt change.
#'   \item \code{BetaParRelative} - Are the beta parameters relative?
#'   \item \code{nMatureVaries} - The number of maturity probability periods
#'   \item \code{iYrsMatureChanges} - The year(s) for which maturity probability change.
#'   \item \code{nGrwth} - The total number of growth parameters
#'   (depends upon nSizeIncVaries).
#'   \item \code{nSizeIncPar} - The number of size increment parameters.
#'   \item \code{Grwth_control} - The matrix of growth controls.
#'   \item \code{MoltProb_control} - The matrix of molting probability controls.
#'   \item \code{MatureProb_control} - The matrix of mature probability controls.
#'   \item \code{CustomGrowthMatrix} - The custom growth-increment matrix or size-transition
#'   matrix.
#'   \item \code{CustomMoltProbabilityMatrix} - The custom molt probability matrix.
#'   \item \code{slx_nsel_period_in} - The number of selectivity time period per fleet.
#'   \item \code{slx_bsex_in} - The number of sex specific selectivity.
#'   \item \code{slx_type_in} - The sex-specific selectivity type.
#'   \item \code{slx_include_in} - Is there any insertion of a fleet selectivity in another?
#'   \item \code{slx_extra_in} - Extra parameter for each selectivity pattern.
#'   \item \code{ret_nret_period_in} - The number of retention time period per fleet.
#'   \item \code{ret_bsex_in} -  The number of sex specific retention.
#'   \item \code{ret_type_in} - The sex-specific retention type.
#'   \item \code{ret_extra_in} - Extra parameter for each retention pattern.
#'   \item \code{slx_nret} - Boolean for retention/discard.
#'   \item \code{slx_max_at_1_in} - Is the selectivity for the maximum size class
#'   forced to be 1?
#'   \item \code{slx_cols} - The number of parameter required for each vulnerability pattern.
#'   \item \code{slx_npar} - The number of parameter for each pattern of selectivity.
#'   \item \code{nslx_pars} - The total number of vulnerability parameters.
#'   \item \code{Selex_control} - the matrix of selectivity control.
#'   \item \code{Ret_control} - The matrix of retention control.
#'   \item \code{NumAsympRet} - The number of asymptotic vulnerability parameters.
#'   \item \code{AsympSel_control} - The asymptotic parameter control.
#'   \item \code{nslx_envpars} - The number of climate-driven selectivity parameters.
#'   \item \code{NSlx_devs_param} - The number of parameters controlling selectivity deviations.
#'   \item \code{devParPhase} - The estimation phase for the deviation parameter
#'   (when a random walk is considered).
#'   \item \code{q_controls} - The catchability control parameters.
#'   \item \code{add_cv_controls} - Additional CV control parameters for catchability.
#'   \item \code{add_cv_links} - Use of additional variance for each survey (0: ignore; >0 use).
#'   \item \code{f_controls} - The fishing mortality control.
#'   \item \code{nAgeCompType} - The size composition likelihood type.
#'   \item \code{bTailCompression} - The auto tail compression specification.
#'   \item \code{nvn_ival} - The initial value(s) for effective sample size.
#'   \item \code{nvn_phz} - The phase of estimation for the effective sample size.
#'   \item \code{iCompAggregator} - Should the data be aggregated?
#'   \item \code{lf_catch} - The type of predictions for the size composition data (survey-like / catch-like)
#'   \item \code{lf_lambda} - The lamdba for effective sample size.
#'   \item \code{lf_emphasis} - The weight (lambda) for the overall likelihood
#'   \item \code{m_type} - The type of natural mortality.
#'   \item \code{MrelFem}- How does M for females relate to that for males (0 absolute; 1 relative)
#'   \item \code{Mdev_phz_def} - The phase of estimation for natural mortality.
#'   \item \code{m_stdev} - The standard deviation used in the natural mortality deviations.
#'   \item \code{m_nNodes_sex} - The sex-specific number of nodes in natural mortality.
#'   \item \code{m_nodeyear_sex} - The sex-specific years for nodes in natural mortality.
#'   \item \code{nSizeDevs} - The number of breakpoints in natural mortality by size class.
#'   \item \code{m_size_nodeyear} - The specific years for the deviations (blocks) in
#'   natural mortality by size class.
#'   \item \code{Init_Mdev} - Use a specific initial value for natural mortality deviations?
#'   \item \code{nMdev} - The number of natural mortality deviations.
#'   \item \code{Mdev_controls} - Natural mortality deviation control.
#'   \item \code{tag_emphasis} - The emphasis (weight) for the tagging likelihood.
#'   \item \code{m_maturity} - Is there any maturity-specific natural mortality?
#'   \item \code{m_mat_controls} - Immature/mature natural mortality control if any.
#'   \item \code{rdv_syr} - The first year of recruitment estimation deviations.
#'   \item \code{rdv_eyr} - The last year of recruitment estimation deviations.
#'   \item \code{Term_molt} - Consider terminal molting in the model?
#'   \item \code{rdv_phz} - The phase for recruitment estimation.
#'   \item \code{rec_prop_phz} - The phase for recruitment sex-ratio estimation.
#'   \item \code{init_sex_ratio} - The initial value for expected sex-ratio.
#'   \item \code{rec_ini_phz} - The phase for initial recruitment estimation.
#'   \item \code{bInitializeUnfished} - The specification for the initial conditions
#'   (unfished, steady-state, free params, free params revised)
#'   \item \code{spr_lambda} - The proportion of mature male biomass for SPR reference points.
#'   \item \code{nSRR_flag} - The stock-Recruit-Relationship (none, Beverton-Holt)
#'   \item \code{BRP_rec_sexR} - Use years specified to computed average sex ratio in
#'   the calculation of average recruitment for reference points.
#'   \item \code{NyrEquil} - The year to compute equilibrium.
#'   \item \code{catch_emphasis} - The weight on catch for the likelihood component.
#'   \item \code{Penalty_fdevs} - Penalties on deviations.
#'   \item \code{Penalty_emphasis} - Penalties on priors
#'   \item \code{eof} - Logical indicating the end of the file (used for checking the reading).
#' }
#'
#' @seealso \code{\link{readGMACS.dat}},\code{\link{readGMACSdat}},
#' \code{\link{readGMACSprj}}
#'
#'
#' @examples
#' \dontrun{
#' # Stock ----
#' stock <- "SNOW_crab"
#' # GMACS input files ----
#' datfileName <- "snow_21_M09.dat"
#' ctlfileName <- "snow_21_M09.ctl"
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
#' }
#'
#'
#' @export
#' @md
#
readGMACSctl <- function(FileName = NULL,
                         verbose = TRUE,
                         DatFile = NULL,
                         nyrRetro = NULL) {

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

  # @title get.df
  #
  # @description Extract a data frame at a specific location.
  #
  # @param dat the object in which the value is searched
  # @param Loc the position of the value
  # @param nrow the number of lines in the data frame
  #
  # @return the data frame from Loc to (Loc+nrow-1) and
  # increment Loc (in the parent environment) to the end
  # of the data frame + 1.
  #
  get.df <- function(dat, Loc, nrow = NULL) {
    df <- dat[Loc:(Loc + nrow - 1)]
    assign("Loc", Loc + nrow, parent.frame())

    df <-
      strsplit(df, "[[:blank:]]+") ## Split by whitespace and collapse (+)
    df <- as.list(df) ## Must be a list for the next operation
    df <- do.call("rbind", df) ## Make it into a dataframe
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df <- utils::type.convert(df, as.is = TRUE)
    return(df)
  }

  # @title getNrowsVulCtl
  #
  # @description Find the number of rows in each vulnerability matrix (selectivity and retention).
  #
  # @param slx_period the number of vulnerability periods for each fleet
  # @param bsex Is the vulnerability sex specific (1 value for each fleet)
  # @param type_in the type of vulnerability by sex
  # @param extra_in the number of extra parameter for each pattern
  # @param N_fleet the number of fleets (fishing fleet and surveys)
  #
  # @return the number of rows for the vulnerability matrix considered
  #
  getNrowsVulCtl <- function(slx_period = NULL,
                             bsex = NULL,
                             type_in = NULL,
                             extra_in = NULL,
                             N_fleet = NULL) {
    nslx_rows_in = 0

    for (f in 1:N_fleet) {
      nperiod <- slx_period[f]

      for (h in 1:(bsex[f] + 1)) {
        type <- base::switch (.ac(nsex),
                              "1" = type_in[f],
                              "2" = type_in[h, f])
        extra <- extra_in[h, f]

        if (type == 0) {
          # parametric (SELEX_PARAMETRIC)
          nslx_rows_in <- nslx_rows_in  + nclass * nperiod

        }
        if (type == 1) {
          # coefficients (SELEX_COEFFICIENTS)
          nslx_rows_in <- nslx_rows_in  + extra * nperiod

        }
        if (type == 2) {
          # SELEX_STANLOGISTIC (logistic has 2 parameters)
          nslx_rows_in <- nslx_rows_in  + 2 * nperiod

        }
        if (type == 3) {
          # SELEX_5095LOGISTIC (logistic95 has 2 parameters)
          nslx_rows_in <- nslx_rows_in  + 2 * nperiod

        }
        if (type == 10) {
          # SELEX_ONE_PAR_LOGISTIC (logisticOne has 1 parameters)
          nslx_rows_in <- nslx_rows_in  + 1 * nperiod

        }
        if (type == 8) {
          # SELEX_DECLLOGISTIC (declining logistic has 2 + extra parameters)
          nslx_rows_in <- nslx_rows_in  + (2 + extra) * nperiod

        }
        if (type == 4) {
          # SELEX_DOUBLENORM (double normal has 3 parameters)
          nslx_rows_in <- nslx_rows_in  + 3 * nperiod

        }
        if (type == 7) {
          # SELEX_DOUBLENORM4 (double normal has 4 parameters)
          nslx_rows_in <- nslx_rows_in  + 4 * nperiod

        }
        if (type == 5) {
          # SELEX_UNIFORM1 (uniform has 1 parameter)
          nslx_rows_in <- nslx_rows_in  + 1 * nperiod

        }
        if (type == 6) {
          # SELEX_UNIFORM0 (uniform has 1 parameter)
          nslx_rows_in <- nslx_rows_in  + 1 * nperiod

        }
        if (type == 9) {
          # SELEX_CUBIC_SPLINE (spline has parameters for knots and values)
          nslx_rows_in <- nslx_rows_in  + 2 * extra * nperiod

        }
        if (type < 0) {
          # mirror has 1 parameter
          nslx_rows_in <- nslx_rows_in  + 1 * nperiod

        }
      }
    }
    return(nslx_rows_in)
  }

  # @title getNparsVulCtl
  #
  # @description Find the number of vulnerability parameters.
  #
  # @param N_fleet - the number of fleets (fishing fleet and surveys)
  # @param nslx - how many selectivities are we dealing with
  # @param slx_nsel_period_in;ret_nret_period_in - the number of vulnerability periods for each fleet
  # @param slx_bsex_in;ret_bsex_in - Is the vulnerability sex specific (1 value for each fleet)
  # @param slx_type_in;ret_type_in the type of vulnerability by sex
  #
  # @return the number of rows for the vulnerability matrix considered
  getNparsVulCtl <- function(N_fleet = NULL,
                             nslx = NULL,
                             slx_bsex_in = NULL,
                             slx_nsel_period_in = NULL,
                             slx_type_in = NULL,
                             slx_extra_in = NULL,
                             ret_bsex_in = NULL,
                             ret_nret_period_in = NULL,
                             ret_type_in = NULL,
                             ret_extra_in = NULL) {
    # count up number of parameters required
    # ===================================================== #
    slx_type <- rep(NA, nslx)
    slx_extra <- rep(NA, nslx)
    slx_cols <- rep(NA, N_fleet)
    slx_npar <- rep(NA, N_fleet)

    kk <- 1
    for (k in 1:N_fleet) {
      hh <- 1 + slx_bsex_in[k]
      for (h in 1:hh)
        for (i in 1:slx_nsel_period_in[k])
        {
          if (nsex == 1) {
            slx_type[kk] <- slx_type_in[k]
            slx_extra[kk] <- slx_extra_in[1, k]
          } else {
            slx_type[kk] <- slx_type_in[h, k]
            slx_extra[kk] <- slx_extra_in[h, k]
          }
          kk <- kk + 1
        }
    }
    for (k in 1:N_fleet) {
      hh <- 1 + ret_bsex_in[k]
      for (h in 1:hh)
        for (i in 1:ret_nret_period_in[k])
        {
          if (nsex == 1) {
            slx_type[kk] <- ret_type_in[k]
            slx_extra[kk] <- ret_extra_in[1, k]
          } else {
            slx_type[kk] <- ret_type_in[h, k]
            slx_extra[kk] <- ret_extra_in[h, k]
          }
          kk <- kk + 1
        }
    }

    for (f in 1:nslx)
    {
      if (slx_type[f] == 0)
        # SELEX_PARAMETRIC
      {
        slx_cols[f] <- nclass
        slx_npar[f] <- nclass

      }
      if (slx_type[f] == 1)
        # SELEX_COEFFICIENTS
      {
        slx_cols[f] <- slx_extra[f]
        slx_npar[f] <- slx_extra[f]

      }
      if (slx_type[f] == 2)
        # SELEX_STANLOGISTIC
      {
        slx_cols[f] <- 2
        slx_npar[f] <- 2

      }
      if (slx_type[f] == 3)
        # SELEX_5095LOGISTIC
      {
        slx_cols[f] <- 2
        slx_npar[f] <- 2

      }
      if (slx_type[f] == 10)
        # SELEX_ONE_PAR_LOGISTIC
      {
        slx_cols[f] <- 1
        slx_npar[f] <- 1

      }
      if (slx_type[f] == 8)
        # SELEX_DECLLOGISTIC
      {
        slx_cols[f] <- 2 + slx_extra[f]
        slx_npar[f] <- 2 + slx_extra[f]

      }
      if (slx_type[f] == 4)
        # SELEX_DOUBLENORM
      {
        slx_cols[f] <- 3
        slx_npar[f] <- 3

      }
      if (slx_type[f] == 7)
        # SELEX_DOUBLENORM4
      {
        slx_cols[f] <- 4
        slx_npar[f] <- 4

      }
      if (slx_type[f] == 5)
        # SELEX_UNIFORM1
      {
        slx_cols[f] <- 1
        slx_npar[f] <- 1

      }
      if (slx_type[f] == 6)
        # SELEX_UNIFORM0
      {
        slx_cols[f] <- 1
        slx_npar[f] <- 1

      }
      if (slx_type[f] == 9)
        # SELEX_CUBIC_SPLINE
      {
        slx_cols[f] <- 2 * slx_extra[f]
        slx_npar[f] <- 2 * slx_extra[f]

      }
      if (slx_type[f] < 0)
        # mirror
      {
        slx_cols[f] <- 1
        slx_npar[f] <- 1

      }
    }
    out <- list()
    out$slx_cols <- slx_cols
    out$slx_npar <- slx_npar

    return(out)
    # ===================================================== #
  }

  # @title NSlx_devs
  #
  # @description Find the number of vulnerability deviation parameters.
  #
  # @param vul - matrix of selectivity and retention control (rbind)
  #
  # @return the number of vulnerability deviations

  NSlx_devs <- function(vul = NULL) {
    NSlx_devs_param <- 0

    for (x in 1:dim(vul)[1]) {
      rd_walk <- .an(unlist(vul[x, ] %>%  dplyr::select(Rand_Walk)))

      if (rd_walk > 0) {

        Start_year_RW <- .an(unlist(vul[x, ] %>%  dplyr::select(Start_year_RW)))
        End_year_RW <- .an(unlist(vul[x, ] %>%  dplyr::select(End_year_RW)))


        if (rd_walk == 1) {
          NSlx_devs_param <- NSlx_devs_param + (End_year_RW - Start_year_RW)
        } else if (rd_walk == 2) {
          NSlx_devs_param <- NSlx_devs_param + (End_year_RW - Start_year_RW + 1)
        } else {
          cat(
            "\nOption for random walk parameterization are only 0/1/2. Please review
 the selectivity parameter control definition.\n"
          )
          stop()
        }
      } else {
        NSlx_devs_param <- NSlx_devs_param + 0
      }
    }
    return(NSlx_devs_param)
  }

  # -------------------------------------------------------------------------

  nsex <- DatFile$N_sexes
  nmature <- DatFile$N_maturity
  Start_Y <- DatFile$Start_Y
  End_Y <- DatFile$End_Y
  nyrRetro <- End_Y - nyrRetro
  nclass <- DatFile$N_sizeC
  N_fleet <- DatFile$N_fleet

  DatOut <- list()


  # 2- Read the control file and find the first line containing numeric data
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("-- Reading control file \n")
    cat("====================================================")
    cat("\n")
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
  DatOut[["Comments"]] <- Com

  # Initialize the location index
  Loc <- 1
  # -------------------------------------------------------------------------

  # Key parameter control
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading key parameter control \n")
  DatOut[["ntheta"]] <-
    get.num(dat, Loc) # Number of key parameters control

  DatOut[["theta_control"]] <-
    get.df(dat, Loc, nrow = DatOut[["ntheta"]]) # Matrix of parameters
  colnames(DatOut[["theta_control"]]) <-
    c("Init_val",
      "Lower_Bd",
      "Upper_Bd",
      "Phase",
      "Prior",
      "p1",
      "p2")

  if (verbose)
    cat("\t-> Read key parameter control \n")
  # -------------------------------------------------------------------------

  # Custom input data
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading custom input data \n")

  # Allometry
  DatOut[["lw_type"]] <-
    get.num(dat, Loc) # Length-weight relationship type
  if (DatOut[["lw_type"]] < 1 || DatOut[["lw_type"]] > 3)
    stop("length-weight type can only be 1,2 or 3")
  # DatOut[["lw_type"]] == 1 => LW relationship
  # DatOut[["lw_type"]] == 2 => vector of data
  # DatOut[["lw_type"]] == 3 => matrix of data

  # if (DatOut[["lw_type"]] == 1) {
  #   DatOut[["lw_alfa"]] <-
  #     base::switch (.ac(nsex),
  #                   # alpha Length-weight relationship
  #                   "1" = get.num(dat, Loc),
  #                   "2" = get.vec(dat, Loc))
  #   DatOut[["lw_beta"]] <-
  #     base::switch (.ac(nsex),
  #                   # beta Length-weight relationship
  #                   "1" = get.num(dat, Loc),
  #                   "2" = get.vec(dat, Loc))
  #   DatOut[["mean_wt_in"]] <- ""
  # } else {
  #   DatOut[["lw_alfa"]] <- ""
  #   DatOut[["lw_beta"]] <- ""
  #   if (nsex == 1) {
  #     # Input weight at size
  #     DatOut[["mean_wt_in"]] <-
  #       base::switch (.ac(DatOut[["lw_type"]]),
  #                     # Input weight at size
  #                     "2" = get.vec(dat, Loc),
  #                     "3" = get.df(dat, Loc, nrow = nsex * (End_Y + 1 - Start_Y + 1)))
  #   } else {
  #     DatOut[["mean_wt_in"]] <-
  #       base::switch (
  #         .ac(DatOut[["lw_type"]]),
  #         # Input weight at size
  #         "2" = get.df(dat, Loc, nrow = nsex),
  #         "3" = get.df(dat, Loc, nrow = nsex * (End_Y + 1 - Start_Y + 1))
  #       )
  #   }
  # }

  if (DatOut[["lw_type"]] == 1) {
    # vector by sex and maturity (immature/mature)
    # length-weight relationship parameters
    if(nsex == 1){
      tmp <- if(nmature == 1){
        get.vec(dat, Loc)
      } else {
        get.df(dat, Loc, nrow = nsex * nmature)
      }
    } else if (nsex > 1){
      get.df(dat, Loc, nrow = nsex * nmature)
    }
    # Split the parameters
    if(nsex==1){
      # alpha Length-weight relationship
      DatOut[["lw_alfa"]] <- if(nmature==1){
        tmp[1]
      } else {
        tmp[,1]
      }
      # beta Length-weight relationship
      DatOut[["lw_beta"]] <- if(nmature==1){
        tmp[2]
      } else {
        tmp[,2]
      }
    } else {
      # alpha Length-weight relationship
      DatOut[["lw_alfa"]] <- tmp[,1]
      # beta Length-weight relationship
      DatOut[["lw_beta"]] <- tmp[,2]
    }
    DatOut[["mean_wt_in"]] <- ""
  } else {
    DatOut[["lw_alfa"]] <- ""
    DatOut[["lw_beta"]] <- ""
    if (nsex == 1) {
      # Input weight at size
      DatOut[["mean_wt_in"]] <-
        base::switch (.ac(DatOut[["lw_type"]]),
                      # Input weight at size
                      "2" = if(nmature == 1){get.vec(dat, Loc)}else{get.df(dat, Loc, nrow = nsex * nmature)},
                      "3" = get.df(dat, Loc, nrow = nsex * nmature * (End_Y + 1 - Start_Y + 1)))
    } else {
      DatOut[["mean_wt_in"]] <-
        base::switch (
          .ac(DatOut[["lw_type"]]),
          # Input weight at size
          "2" = get.df(dat, Loc, nrow = nsex * nmature),
          "3" = get.df(dat, Loc, nrow = nsex * nmature * (End_Y + 1 - Start_Y + 1))
        )
    }
  }
  if (verbose)
    cat("\t-> Read allometry parameter control \n")

  # Fecundity for MMB/MMA calculation
  DatOut[["maturity"]] <-
    base::switch(.ac(nsex),
                 # Proportion of mature at size by sex
                 "1" = get.vec(dat, Loc),
                 "2" = get.df(dat, Loc, nrow = nsex))
  DatOut[["legal_maturity"]] <-
    base::switch(.ac(nsex),
                 # Proportion of mature at size by sex
                 "1" = get.vec(dat, Loc),
                 "2" = get.df(dat, Loc, nrow = nsex))

  DatOut[["Func_maturity_TermMolting"]] <- get.num(dat, Loc)

  if (verbose)
    cat("\t-> Read Fecundity parameter control for MMB/MMA calculation \n")
  # -------------------------------------------------------------------------

  # Growth parameter controls
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading growth parameter controls \n")

  DatOut[["bUseCustomGrowthMatrix"]] <-
    get.num(dat, Loc) # Options for the growth matrix
  if (DatOut[["bUseCustomGrowthMatrix"]] < 1 ||
      DatOut[["bUseCustomGrowthMatrix"]] > 8)
    stop("Growth matrix type can only be 1-8")

  DatOut[["bUseGrowthIncrementModel"]] <-
    get.num(dat, Loc) # Options for the growth increment model
  DatOut[["bUseCustomMoltProbability"]] <-
    get.num(dat, Loc) # molt probability function
  DatOut[["nSizeClassRec"]] <-
    base::switch(.ac(nsex),
                 # Maximum size-class for recruitment
                 "1" = get.num(dat, Loc),
                 "2" = get.vec(dat, Loc))

  # Size increment stuff ----
  DatOut[["nSizeIncVaries"]] <-
    base::switch(.ac(nsex),
                 # Number of size increment periods
                 "1" = get.num(dat, Loc),
                 "2" = get.vec(dat, Loc))
  # Year(s) with changes in growth matrix - size increment (blank if no change)
  tmpSizeIncVaries <- NULL

  for (s in 1:nsex)
    tmpSizeIncVaries <-
    c(tmpSizeIncVaries, DatOut[["nSizeIncVaries"]][s] - 1)

  if (nsex == 1 && tmpSizeIncVaries > 0) {
    if (tmpSizeIncVaries == 1) {
      DatOut[["iYrsSizeIncChanges"]] <- get.num(dat, Loc)
    } else {
      DatOut[["iYrsSizeIncChanges"]] <- get.vec(dat, Loc)
    }
  } else if (nsex > 1 && max(tmpSizeIncVaries) > 0) {

    # tmpYearsizeVaries <- get.vec(dat, Loc)
    if(min(tmpSizeIncVaries)==0){
      tmpYearsizeVaries <- get.vec(dat, Loc)
    } else {
      tmpYearsizeVaries <- get.df(dat, Loc, nrow = nsex)
    }

    tmp <- matrix(NA, nrow = nsex, ncol = max(tmpSizeIncVaries))

    for (s in 1:nsex) {
      if (tmpSizeIncVaries[s] == 0) {
        tmp[s,] <- NA

      } else if (tmpSizeIncVaries[s] == 1) {
        tmp[s,1] <- tmpYearsizeVaries[s,1]

      } else {
        if(min(tmpSizeIncVaries)==0){
          tmp[s,] <- tmpYearsizeVaries

        } else {
          tmp[s,] <- .an(tmpYearsizeVaries[s,])
        }
      }
    }
    DatOut[["iYrsSizeIncChanges"]] <- tmp
  } else {
    DatOut[["iYrsSizeIncChanges"]] <- ""
  }

  # Molting stuff ----
  DatOut[["nMoltVaries"]] <-
    base::switch(.ac(nsex),
                 # Number of molt periods
                 "1" = get.num(dat, Loc),
                 "2" = get.vec(dat, Loc))

  # Year(s) molt period changes (blank if no change)
  tmpMoltVaries <- NULL
  for (s in 1:nsex)
    tmpMoltVaries <-
    c(tmpMoltVaries, DatOut[["nMoltVaries"]][s] - 1)

  if (nsex == 1 && tmpMoltVaries > 0) {
    if (tmpMoltVaries == 1) {
      DatOut[["iYrsMoltChanges"]] <- get.num(dat, Loc)
    } else {
      DatOut[["iYrsMoltChanges"]] <- get.vec(dat, Loc)
    }

  } else if (nsex > 1 && max(tmpMoltVaries) > 0) {

    # tmpYearMoltVaries <- get.vec(dat, Loc)
    if(min(tmpMoltVaries)==0){
      tmpYearMoltVaries <- get.vec(dat, Loc)
    } else {
      tmpYearMoltVaries <- get.df(dat, Loc, nrow = nsex)
    }


    tmp <- matrix(NA, nrow = nsex, ncol = max(tmpMoltVaries))

    for (s in 1:nsex) {

      if (tmpMoltVaries[s] == 0) {
        tmp[s,] <- NA

      } else if (tmpMoltVaries[s] == 1) {
        tmp[s,1] <- tmpYearMoltVaries[s,1]

      } else {
        # tmp[s,] <- tmpYearMoltVaries
        if(min(tmpMoltVaries)==0){
          tmp[s,] <- tmpYearMoltVaries
        } else {
          tmp[s,] <- .an(tmpYearMoltVaries[s,])

        }
      }
    }
    DatOut[["iYrsMoltChanges"]] <- tmp
  } else {
    DatOut[["iYrsMoltChanges"]] <- ""
  }
  # Beta parameters ----
  DatOut[["BetaParRelative"]] <-
    get.num(dat, Loc) # Beta parameters are relative (0:No; 1:Yes)

  # Maturity probability stuff ----
  if (nmature == 2) {
    DatOut[["bUseCustomMatureProbability"]] <-
      get.num(dat, Loc) # Options for the maturity probability

    DatOut[["nMatureVaries"]] <-
      base::switch(.ac(nsex),
                   # Number of block of maturity probability
                   "1" = get.num(dat, Loc),
                   "2" = get.vec(dat, Loc))
    # Year(s) with changes in maturity probability (blank if no change)
    tmpMatureVaries <- NULL
    for (s in 1:nsex)
      tmpMatureVaries <-
      c(tmpMatureVaries, DatOut[["nMatureVaries"]][s] - 1)
    if (nsex == 1 && tmpMatureVaries > 0) {
      if (tmpMatureVaries == 1) {
        DatOut[["iYrsMatureChanges"]] <- get.num(dat, Loc)
      } else {
        DatOut[["iYrsMatureChanges"]] <- get.vec(dat, Loc)
      }

    } else if (nsex > 1 && max(tmpMatureVaries) > 0) {
      # tmpYearMatureVaries <- get.vec(dat, Loc)
      if (min(tmpMatureVaries) == 0) {
        tmpYearMatureVaries <- get.vec(dat, Loc)
      } else {
        tmpYearMatureVaries <- get.df(dat, Loc, nrow = nsex)
      }
      tmp <- matrix(NA, nrow = nsex, ncol = max(tmpMatureVaries))

      for (s in 1:nsex) {
        if (tmpMatureVaries[s] == 0) {
          tmp[s, ] <- NA

        } else if (tmpMatureVaries[s] == 1) {
          tmp[s, 1] <- tmpYearMatureVaries[s, 1]

        } else {
          # tmp[s,] <- tmpYearMatureVaries
          if (min(tmpMatureVaries) == 0) {
            tmp[s, ] <- tmpYearMatureVaries
          } else {
            tmp[s, ] <- .an(tmpYearMatureVaries[s, ])

          }
        }
      }
      DatOut[["iYrsMatureChanges"]] <- tmp
    } else {
      DatOut[["iYrsMatureChanges"]] <- ""
    }
  }

  # Growth parameters
  # +++++++++++++++++++++++++++++
  nGrwth <- 0
  nSizeIncPar <- 0
  nMaturePar <- 0
  for (s in 1:nsex)
  {
    if (DatOut[["bUseGrowthIncrementModel"]] == 1)
      nGrwth <-
        nGrwth + DatOut[["nSizeIncVaries"]][s] * 3 # LINEAR_GROWTHMODEL
    if (DatOut[["bUseGrowthIncrementModel"]] == 2)
      nGrwth <-
        nGrwth + DatOut[["nSizeIncVaries"]][s] * (nclass + 1) # INDIVIDUAL_GROWTHMODEL1
    if (DatOut[["bUseGrowthIncrementModel"]] == 3)
      nGrwth <-
        nGrwth + DatOut[["nSizeIncVaries"]][s] * (nclass + 1) # INDIVIDUAL_GROWTHMODEL2
    if (DatOut[["bUseGrowthIncrementModel"]] == 5)
      nGrwth <-
        nGrwth + DatOut[["nSizeIncVaries"]][s] * 3 # GROWTH_VARYK
    if (DatOut[["bUseGrowthIncrementModel"]] == 6)
      nGrwth <-
        nGrwth + DatOut[["nSizeIncVaries"]][s] * 3 # GROWTH_VARYLINF
    if (DatOut[["bUseGrowthIncrementModel"]] == 7)
      nGrwth <-
        nGrwth + DatOut[["nSizeIncVaries"]][s] * 4 # GROWTH_VARYKLINF
    if (DatOut[["bUseCustomMoltProbability"]] == 2)
      nSizeIncPar <-
        nSizeIncPar + DatOut[["nMoltVaries"]][s] * 2 # LOGISTIC_PROB_MOLT
    if (DatOut[["bUseCustomMoltProbability"]] == 3)
      nSizeIncPar <-
        nSizeIncPar + DatOut[["nMoltVaries"]][s] * (nclass) # FREE_PROB_MOLT
    if (nmature > 1) { # Mature specific stuff
      if (DatOut[["bUseCustomMatureProbability"]] == 2)
        nMaturePar <-
          nMaturePar + DatOut[["nMatureVaries"]][s] * 2 # LOGISTIC_PROB_MATURE
      if (DatOut[["bUseCustomMatureProbability"]] == 3)
        nMaturePar <-
          nMaturePar + DatOut[["nMatureVaries"]][s] * (nclass) # FREE_PROB_MATURE
    }
  }
  # nGrwth <- nGrwth + nSizeIncPar
  DatOut[["nGrwth"]] <- nGrwth
  DatOut[["nSizeIncPar"]] <- nSizeIncPar
  DatOut[["nMaturePar"]] <- nMaturePar

  # DatOut[["Grwth_control"]] <-
  #  get.df(dat, Loc, nrow = nGrwth) # Growth parameters control
  # DatOut[["MoltProb_control"]] <-
  #   get.df(dat, Loc, nrow = nSizeIncPar) # Growth parameters control
  # colnames(DatOut[["Grwth_control"]]) <-
  #   colnames(DatOut[["MoltProb_control"]]) <-
  #   c("Init_val",
  #     "Lower_Bd",
  #     "Upper_Bd",
  #     "Phase",
  #     "Prior",
  #     "p1",
  #     "p2")
  tmpColname <- c("Init_val",
                  "Lower_Bd",
                  "Upper_Bd",
                  "Phase",
                  "Prior",
                  "p1",
                  "p2")

  if(nGrwth > 0){
    DatOut[["Grwth_control"]] <-
      get.df(dat, Loc, nrow = nGrwth) # Growth parameters control
    colnames(DatOut[["Grwth_control"]]) <- tmpColname
  }
  if(nSizeIncPar>0){
    DatOut[["MoltProb_control"]] <-
      get.df(dat, Loc, nrow = nSizeIncPar) # Growth parameters control
    colnames(DatOut[["MoltProb_control"]]) <- tmpColname
  }
  if(nMaturePar>0){
    DatOut[["MatureProb_control"]] <-
      get.df(dat, Loc, nrow = nMaturePar) # Growth parameters control
    colnames(DatOut[["MatureProb_control"]]) <- tmpColname
  }
  # +++++++++++++++++++++++++++++

  # Custom growth-increment matrix or size-transition matrix
  # +++++++++++++++++++++++++++++
  maxSizeIncVaries = max(DatOut[["nSizeIncVaries"]])
  if (DatOut[["bUseCustomGrowthMatrix"]] == 1 ||
      DatOut[["bUseCustomGrowthMatrix"]] == 2) {
    # GROWTH_FIXEDGROWTHTRANS || GROWTH_FIXEDSIZETRANS
    DatOut[["CustomGrowthMatrix"]] <-
      get.df(dat, Loc, nrow = nclass * nsex * maxSizeIncVaries)
  } else {
    DatOut[["CustomGrowthMatrix"]] <- ""
  }
  # +++++++++++++++++++++++++++++

  # Custom molt probability matrix
  # +++++++++++++++++++++++++++++
  if (DatOut[["bUseCustomMoltProbability"]] == 0) {
    # Fixed Molt probability
    tmp <- NULL
    for(s in 1:nsex){
      tmp <- rbind(tmp,
                   get.df(dat, Loc, nrow = DatOut[["nMoltVaries"]][s])
      )
    }
    DatOut[["CustomMoltProbabilityMatrix"]] <- tmp
  } else {
    DatOut[["CustomMoltProbabilityMatrix"]] <- ""
  }
  # +++++++++++++++++++++++++++++

  # Custom maturity probability matrix
  # +++++++++++++++++++++++++++++
  if (nmature == 2) {
    if (DatOut[["bUseCustomMatureProbability"]] == 0) {
      # Fixed maturity probability
      tmp <- NULL
      for (s in 1:nsex) {
        tmp <- rbind(tmp, get.df(dat, Loc, nrow = DatOut[["nMatureVaries"]][s]))
      }
      DatOut[["CustomMatureProbabilityMatrix"]] <- tmp
    } else {
      DatOut[["CustomMatureProbabilityMatrix"]] <- ""
    }
  }
  # +++++++++++++++++++++++++++++

  if (verbose)
    cat("\t-> Read growth parameter controls \n")
  # -------------------------------------------------------------------------

  # Selectivity parameter controls
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading Vulnerability parameter controls \n")

  # Selectivity
  DatOut[["slx_nsel_period_in"]] <-
    get.vec(dat, Loc) # Number of selectivity time period per fleet
  DatOut[["slx_bsex_in"]] <-
    get.vec(dat, Loc) # Sex specific selectivity
  if (max(DatOut[["slx_bsex_in"]]) == 0) {
    DatOut[["slx_type_in"]] <-
      get.vec(dat, Loc) # Males Selectivity type
  } else {
    DatOut[["slx_type_in"]] <-
      get.df(dat, Loc, nrow = nsex) # Males & females Selectivity types
  }
  DatOut[["slx_include_in"]] <-
    get.vec(dat, Loc) # Insertion of fleet in another
  DatOut[["slx_extra_in"]] <-
    get.df(dat, Loc, nrow = nsex) # Extra parameter for each pattern

  # Retention
  DatOut[["ret_nret_period_in"]] <-
    get.vec(dat, Loc) # Number of retention time period per fleet
  DatOut[["ret_bsex_in"]] <-
    get.vec(dat, Loc) # Sex specific retention
  if (max(DatOut[["ret_bsex_in"]]) == 0) {
    DatOut[["ret_type_in"]] <- get.vec(dat, Loc) # Males retention type
  } else {
    DatOut[["ret_type_in"]] <-
      get.df(dat, Loc, nrow = nsex) # Males & females retention types
  }
  DatOut[["slx_nret"]] <-
    get.df(dat, Loc, nrow = nsex) # boolean for retention/discard
  DatOut[["ret_extra_in"]] <-
    get.df(dat, Loc, nrow = nsex) # Extra parameter for each pattern
  DatOut[["slx_max_at_1_in"]] <-
    get.vec(dat, Loc) # Selectivity for the maximum size class if forced to be 1?

  # Define the number of rows for each vulnerability matrix
  nRowsSelex <- getNrowsVulCtl(
    slx_period = DatOut[["slx_nsel_period_in"]],
    bsex = DatOut[["slx_bsex_in"]],
    type_in = DatOut[["slx_type_in"]],
    extra_in = DatOut[["slx_extra_in"]],
    N_fleet = N_fleet
  )
  nRowsRet <- getNrowsVulCtl(
    slx_period = DatOut[["ret_nret_period_in"]],
    bsex = DatOut[["ret_bsex_in"]],
    type_in = DatOut[["ret_type_in"]],
    extra_in = DatOut[["ret_extra_in"]],
    N_fleet = N_fleet
  )
  # how many selectivities are we dealing with
  nslx <- 0
  for (f in 1:DatFile$N_fleet) {
    nslx <-
      nslx + DatOut[["slx_nsel_period_in"]][f] * (1 + DatOut[["slx_bsex_in"]][f])

    nslx <-
      nslx + DatOut[["ret_nret_period_in"]][f] * (1 + DatOut[["ret_bsex_in"]][f])
  }


  # Nvulpars <- getNparsVulCtl(
  #   N_fleet = N_fleet,
  #   nslx = nslx,
  #   slx_bsex_in = DatOut[["slx_bsex_in"]],
  #   slx_nsel_period_in = DatOut[["slx_nsel_period_in"]],
  #   slx_type_in = DatOut[["slx_type_in"]],
  #   ret_bsex_in = DatOut[["ret_bsex_in"]],
  #   ret_nret_period_in = DatOut[["ret_nret_period_in"]],
  #   ret_type_in = DatOut[["ret_type_in"]]
  # )

  Nvulpars <- getNparsVulCtl(
    N_fleet = N_fleet,
    nslx = nslx,
    slx_bsex_in = DatOut[["slx_bsex_in"]],
    slx_nsel_period_in = DatOut[["slx_nsel_period_in"]],
    slx_type_in = DatOut[["slx_type_in"]],
    slx_extra_in = DatOut[["slx_extra_in"]],
    ret_bsex_in = DatOut[["ret_bsex_in"]],
    ret_nret_period_in = DatOut[["ret_nret_period_in"]],
    ret_type_in = DatOut[["ret_type_in"]],
    ret_extra_in = DatOut[["ret_extra_in"]]
  )

  # Get the outputs from getNparsVulCtl
  slx_cols <-
    Nvulpars$slx_cols
  slx_npar <-
    Nvulpars$slx_npar
  nslx_pars <- sum(slx_cols)

  DatOut[["slx_cols"]] <- slx_cols
  DatOut[["slx_npar"]] <- slx_npar
  DatOut[["nslx_pars"]] <- nslx_pars
  # Vulnerability controls
  DatOut[["Selex_control"]] <-
    get.df(dat, Loc, nrow = nRowsSelex) # Selectivity parameters control
  DatOut[["Ret_control"]] <-
    get.df(dat, Loc, nrow = nRowsRet) # Retention parameters control

  colnames(DatOut[["Selex_control"]]) <-
    colnames(DatOut[["Ret_control"]]) <-
    c(
      "Fleet",
      "Index",
      "Par_no",
      "sex",
      "Init_val",
      "Lower_Bd",
      "Upper_Bd",
      "Prior",
      "Prior_1",
      "Prior_2",
      "Phase",
      "Start_Block",
      "End_Block",
      "Env_Link",
      "Env_Index",
      "Rand_Walk",
      "Start_year_RW",
      "End_year_RW",
      "Sigma_RW"
    )

  DatOut[["NumAsympRet"]] <-
    get.num(dat, Loc) # Number of asymptotic selectivity parameter
  if (DatOut[["NumAsympRet"]] > 0) {
    DatOut[["AsympSel_control"]] <-
      get.df(dat, Loc, nrow = DatOut[["NumAsympRet"]]) # Asymptotic parameter control
    colnames(DatOut[["AsympSel_control"]]) <-
      c("Fleet",
        "Sex",
        "Year",
        "Init_val",
        "Lower_Bd",
        "Upper_Bd",
        "Phase")
  }
  # Environmental parameters selectivity definition
  nslx_envpars <- c(DatOut[["Selex_control"]]$Env_Link,DatOut[["Ret_control"]]$Env_Link)
  nslx_envpars <- length(which(nslx_envpars>0))
  DatOut[["nslx_envpars"]] <- nslx_envpars
  if (DatOut[["nslx_envpars"]]) {
    # Extract parameters
    DatOut[["SlxEnvPar"]] <- get.df(dat, Loc, nrow = nslx_envpars)
    colnames(DatOut[["SlxEnvPar"]]) <- c(
      "Init_val",
      "Lower_Bd",
      "Upper_Bd",
      "Phase"
    )
  }
  # Random walk selectivity definition
  vul <- rbind(DatOut[["Selex_control"]], DatOut[["Ret_control"]])
  DatOut[["NSlx_devs_param"]] <- NSlx_devs(vul = vul)

  # Estimation phase for the deviation parameter (random walk)
  DatOut[["devParPhase"]] <- get.num(dat, Loc)

  if (verbose)
    cat("\t-> Read Vulnerability parameter controls \n")
  # -------------------------------------------------------------------------

  # Priors for catchabilities of surveys
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading catchability parameter controls  \n")

  # length(DatFile$Survey_names)
  DatOut[["q_controls"]] <-
    get.df(dat, Loc, nrow = DatFile[["N_SurveyDF"]]) # Catchability control param
  colnames(DatOut[["q_controls"]]) <-
    c(
      "Init_val",
      "Lower_Bd",
      "Upper_Bd",
      "Phase",
      "Prior",
      "Prior_1",
      "Prior_2",
      "Q_anal",
      "CV_mult",
      "Loglik_mult"
    )

  if (verbose)
    cat("\t-> Read catchability parameter controls \n")
  # -------------------------------------------------------------------------

  # Additional survey CV control
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading additional CV controls  \n")

  DatOut[["add_cv_controls"]] <-
    get.df(dat, Loc, nrow = DatFile[["N_SurveyDF"]]) # Additional CV control param
  colnames(DatOut[["add_cv_controls"]]) <-
    c("Init_val",
      "Lower_Bd",
      "Upper_Bd",
      "Phase",
      "Prior",
      "Prior_1",
      "Prior_2")

  if (DatFile[["N_SurveyDF"]] == 1) {
    # Additional variance control for each survey (0: ignore; >0 use)
    DatOut[["add_cv_links"]] <- get.num(dat, Loc)
  } else if (DatFile[["N_SurveyDF"]] > 1) {
    DatOut[["add_cv_links"]] <- get.vec(dat, Loc)
  } else {
    DatOut[["add_cv_links"]] <- ""
  }
  if (verbose)
    cat("\t-> Read additional CV controls \n")
  # -------------------------------------------------------------------------

  # Penalties for fishing mortality control
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading penalties for mean fishing mortality rates (gear specific) \n")

  DatOut[["f_controls"]] <-
    get.df(dat, Loc, nrow = N_fleet) # Fishing mortality controls
  colnames(DatOut[["f_controls"]]) <-
    c(
      "Mean_F_male",
      "Female_Offset",
      "Pen_std_Ph1",
      "Pen_std_Ph2",
      "Ph_Mean_F_male",
      "Ph_Mean_F_female",
      "Low_bd_mean_F",
      "Up_bd_mean_F",
      "Low_bd_Y_male_F",
      "Up_bd_Y_male_F",
      "Low_bd_Y_female_F",
      "Up_bd_Y_female_F"
    )
  if (verbose)
    cat("\t-> Read fishing mortality penalty controls \n")
  # -------------------------------------------------------------------------

  # Size composition data control
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading size compositon data controls \n")

  DatOut[["nAgeCompType"]] <-
    get.vec(dat, Loc) # Size composition likelihood type
  DatOut[["bTailCompression"]] <-
    get.vec(dat, Loc) # Auto tail compression
  DatOut[["nvn_ival"]] <-
    get.vec(dat, Loc) # Initial value for effective sample size
  DatOut[["nvn_phz"]] <-
    get.vec(dat, Loc) # Phase for effective sample size
  DatOut[["iCompAggregator"]] <-
    get.vec(dat, Loc) # Should data be aggregated
  DatOut[["lf_catch"]] <-
    get.vec(dat, Loc) # 2:Survey-like predictions; 1:catch-like predictions

  DatOut[["lf_lambda"]] <- get.vec(dat, Loc) # Lambda for effective sample size
  DatOut[["lf_emphasis"]] <-
    get.vec(dat, Loc) # Weight for likelihood

  if (verbose)
    cat("\t-> Read size compositon data controls \n")
  # -------------------------------------------------------------------------


  # Natural mortality control
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading natural mortality controls \n")

  DatOut[["m_type"]] <- get.num(dat, Loc) # Type of M specification
  if (DatOut[["m_type"]] > 6)
    stop("Natural mortality type must be >6")

  if (nsex > 1) {
    # Is female M relative to M male?
    DatOut[["MrelFem"]] <- get.num(dat, Loc)
  } else {
    DatOut[["MrelFem"]] <- ""
  }
  DatOut[["Mdev_phz_def"]] <-
    get.num(dat, Loc) # Phase of estimation for M
  DatOut[["m_stdev"]] <-
    get.num(dat, Loc) # standard deviation in M deviations

  # Number of nodes for cubic spline or number of step-changes for option 3
  # DatOut[["m_nNodes_sex"]] <- get.df(dat, Loc, nsex)
  # if (dim(DatOut[["m_nNodes_sex"]])[1] == 1 &&
  #     DatOut[["m_nNodes_sex"]] == 0) {
  #   DatOut[["m_nodeyear_sex"]] <- ""
  # } else if (dim(DatOut[["m_nNodes_sex"]])[1] == 1 &&
  #            DatOut[["m_nNodes_sex"]] != 0) {
  #   #Need to be checked
  #   if(DatOut[["m_nNodes_sex"]] == 1){
  #     DatOut[["m_nodeyear_sex"]] <- get.num(dat, Loc)
  #   } else {
  #     DatOut[["m_nodeyear_sex"]] <- get.vec(dat, Loc)
  #   }
  # } else {
  #   if (nsex == 1) {
  #     if (DatOut[["m_nNodes_sex"]] == 1) {
  #       DatOut[["m_nodeyear_sex"]] <- get.num(dat, Loc)
  #     } else {
  #       DatOut[["m_nodeyear_sex"]] <- get.vec(dat, Loc)
  #     }
  #   } else {
  #     tmp <- matrix(NA, nrow = nsex, ncol = max(DatOut[["m_nNodes_sex"]]))
  #
  #     for (s in 1:nsex) {
  #       if (DatOut[["m_nNodes_sex"]][s, 1] == 1) {
  #         if (DatOut[["m_nNodes_sex"]][s, 1] == max(DatOut[["m_nNodes_sex"]])) {
  #           tmp[s, ] <- get.num(dat, Loc)
  #         } else {
  #           tmp[s, ] <-
  #             c(get.num(dat, Loc), rep(NA, max(DatOut[["m_nNodes_sex"]]) - 1))
  #         }
  #       } else {
  #         if (DatOut[["m_nNodes_sex"]][s, 1] == max(DatOut[["m_nNodes_sex"]])) {
  #           tmp[s, ] <- get.vec(dat, Loc)
  #         } else {
  #           tmp[s, ] <-
  #             c(get.vec(dat, Loc), rep(NA, DatOut[["m_nNodes_sex"]][s, 1] - 1))
  #         }
  #
  #       }
  #     }
  #     DatOut[["m_nodeyear_sex"]] <- tmp
  #   }
  # }
  DatOut[["m_nNodes_sex"]] <- get.df(dat, Loc, nsex)


  if (nsex == 1) {
    if (.an(DatOut[["m_nNodes_sex"]]) == 0) {
      DatOut[["m_nodeyear_sex"]] <- ""
    } else {
      if (DatOut[["m_nNodes_sex"]] == 1) {
        DatOut[["m_nodeyear_sex"]] <- get.num(dat, Loc)
      } else {
        DatOut[["m_nodeyear_sex"]] <- get.vec(dat, Loc)
      }
    }
  } else {
    tmp <- matrix(NA, nrow = nsex, ncol = max(DatOut[["m_nNodes_sex"]]))

    for (s in 1:nsex) {
      if (DatOut[["m_nNodes_sex"]][s, 1] == 1) {
        if (DatOut[["m_nNodes_sex"]][s, 1] == max(DatOut[["m_nNodes_sex"]])) {
          tmp[s, ] <- get.num(dat, Loc)
        } else {
          tmp[s, ] <-
            c(get.num(dat, Loc), rep(NA, max(DatOut[["m_nNodes_sex"]]) - 1))
        }
      } else {
        if (DatOut[["m_nNodes_sex"]][s, 1] == max(DatOut[["m_nNodes_sex"]])) {
          tmp[s, ] <- get.vec(dat, Loc)
        } else {
          tmp[s, ] <-
            c(get.vec(dat, Loc), rep(NA, DatOut[["m_nNodes_sex"]][s, 1] - 1))
        }

      }
    }
    DatOut[["m_nodeyear_sex"]] <- tmp
  }

  DatOut[["nSizeDevs"]] <-
    get.num(dat, Loc) # Number of breakpoints in M by size class
  if (DatOut[["nSizeDevs"]] == 0) {
    # Size positions of breakpoints in M by size class
    DatOut[["m_size_nodeyear"]] <- ""
  } else if (DatOut[["nSizeDevs"]] == 1) {
    DatOut[["m_size_nodeyear"]] <- get.num(dat, Loc)
  } else {
    DatOut[["m_size_nodeyear"]] <- get.vec(dat, Loc)
  }
  DatOut[["Init_Mdev"]] <-
    get.num(dat, Loc) # Specific initial value for natural mortality deviations

  # how many rows are in the natural mortality controls to specific initial values
  # for the M deviations
  nMdev <- base::switch(
    EXPR = .ac(DatOut[["m_type"]]),
    "0" =  0,
    # M_CONSTANT:
    "1" = nsex * (End_Y - Start_Y),
    #M_RANDOM:
    "2" = sum(DatOut[["m_nNodes_sex"]]),
    # M_CUBIC_SPLINE:
    "3" = sum(DatOut[["m_nNodes_sex"]]),
    # M_BLOCKED_CHANGES
    "4" = sum(DatOut[["m_nNodes_sex"]]) / 2,
    # M_TIME_BLOCKS1
    "6" = sum(DatOut[["m_nNodes_sex"]]) # M_TIME_BLOCKS2
  )
  if (DatFile[["N_maturity"]] > 1)
    nMdev <- nMdev * 2
  nMdev <- nMdev + DatOut[["nSizeDevs"]]
  DatOut[["nMdev"]] <- nMdev

  if (DatOut[["Init_Mdev"]] == 0) {
    DatOut[["Mdev_controls"]] <- ""
  } else {
    DatOut[["Mdev_controls"]] <-
      get.df(dat, Loc, nrow = nMdev) # Natural mortality deviations controls
    colnames(DatOut[["Mdev_controls"]]) <-
      c("Init_val", "Lower_Bd", "Upper_Bd", "Phase", "Size_spec")
  }

  if (verbose)
    cat("\t-> Read natural moratlity controls \n")
  # -------------------------------------------------------------------------

  # Tagging control
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading Tagging controls \n")

  DatOut[["tag_emphasis"]] <-
    get.num(dat, Loc) # Emphasis (likelihood weight) on tagging

  if (verbose)
    cat("\t-> Read tagging controls \n")
  # -------------------------------------------------------------------------

  # Immature/mature natural mortality
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading immature/mature natural mortality controls \n")

  DatOut[["m_maturity"]] <-
    get.num(dat, Loc) # maturity specific natural mortality?
  # (1:yes; 0:no - only for use if nmature > 1)
  DatOut[["m_mat_controls"]] <-
    get.df(dat, Loc, nrow = nsex) # immature/mature natural mortality controls
  colnames(DatOut[["m_mat_controls"]]) <-
    c("Init_val",
      "Lower_Bd",
      "Upper_Bd",
      "Phase",
      "Prior",
      "Prior_1",
      "Prior_2")

  if (verbose)
    cat("\t-> Read immature/mature natural mortality controls \n")
  # -------------------------------------------------------------------------

  # Other (additional) controls
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading Additional controls \n")

  DatOut[["rdv_syr"]] <-
    get.num(dat, Loc) # First year of recruitment estimation deviations
  DatOut[["rdv_eyr"]] <-
    get.num(dat, Loc) # Last year of recruitment estimation deviations
  DatOut[["Term_molt"]] <-
    get.num(dat, Loc) # Consider terminal molting?
  DatOut[["rdv_phz"]] <-
    get.num(dat, Loc) # Phase for recruitment estimation
  DatOut[["rec_prop_phz"]] <-
    get.num(dat, Loc) # Phase for recruitment sex-ratio estimation
  DatOut[["init_sex_ratio"]] <-
    get.num(dat, Loc) # Initial value for expected sex-ratio
  DatOut[["rec_ini_phz"]] <-
    get.num(dat, Loc) # Phase for initial recruitment estimation
  # DatOut[["verbose"]] <-
  #   get.num(dat, Loc) # Verbose flag (0: off; 1: on; 2: objective function; 3: diagnostics)
  DatOut[["bInitializeUnfished"]] <-
    get.num(dat, Loc) # Initial conditions (1: unfished, 2: steady-state, 3: free params, 4: free params revised)
  DatOut[["spr_lambda"]] <-
    get.num(dat, Loc) # Proportion of mature male biomass for SPR reference points
  DatOut[["nSRR_flag"]] <-
    get.num(dat, Loc) # Stock-Recruit-Relationship (0 = none, 1 = Beverton-Holt)
  # DatOut[["TurnOffPhase"]] <-
  #   get.num(dat, Loc) # Maximum phase (stop the estimation after this phase)
  # DatOut[["StopAfterFnCall"]] <-
  #   get.num(dat, Loc) # Maximum number of function calls
  # DatOut[["CalcRefPoints"]] <-
  #   get.num(dat, Loc) # Calculate reference points (0:no, 1: yes)
  DatOut[["BRP_rec_sexR"]] <-
    get.num(dat, Loc) # Use years specified to computed average sex ratio in the calculation of average recruitment for reference points (0 = off -i.e. Rec based on End year, 1 = on)
  DatOut[["NyrEquil"]] <-
    get.num(dat, Loc) # Years to compute equilibria

  if (verbose)
    cat("\t-> Read additional controls \n")
  # -------------------------------------------------------------------------

  # Emphasis factor (weights for likelihood) controls
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading Emphasis controls (weights for likelihood) \n")

  # Catches and deviations
  if (DatFile[["N_CatchDF"]] == 1) {
    # Weights on catches for the likelihood component
    DatOut[["catch_emphasis"]] <- get.num(dat, Loc)
  } else {
    DatOut[["catch_emphasis"]] <- get.vec(dat, Loc)
  }
  DatOut[["Penalty_fdevs"]] <-
    get.df(dat, Loc, nrow = DatFile[["N_fleet"]]) # Penalties on deviations
  colnames(DatOut[["Penalty_fdevs"]]) <-
    c("Fdev_total", "Fdov_total", "Fdev_year", "Fdov_year")


  # priors
  namEmph <- c(
    "Log_fdevs",
    "meanF",
    "Mdevs",
    "Rec_devs",
    "Initial_devs",
    "Fst_dif_dev",
    "Mean_sex-Ratio",
    "Molt_prob",
    "Free_selectivity",
    "Init_n_at_len",
    "Fdevs",
    "Fdovs",
    "Vul_devs"
  )
  DatOut$Penalty_emphasis <- NULL
  for(i in 1:length(namEmph)){
    eval(parse(text = paste0(
      "DatOut$Penalty_emphasis[['",namEmph[i],"']] <- get.num(dat,Loc)"
    )))

  }
  if (verbose)
    cat("\t-> Read Emphasis controls \n")
  # -------------------------------------------------------------------------

  # End of data file
  # -------------------------------------------------------------------------
  eof <- get.num(dat, Loc)
  if(eof != 9999){
    cat("\n\nSomething went wrong while reading the control file !!\n")
    stop()
  }
  if (verbose) {
    cat("====================================================\n")
    cat("Read of control file complete. Final value = ", eof, "\n")
    cat("\n")
  }
  DatOut[["eof"]] <- FALSE
  if (eof == 9999)
    DatOut[["eof"]] <- TRUE

  return(DatOut)
}
