#' @title Read Gmacs.rep
#'
#' @description This functions reads the gmacs.rep file (report file) as a named list
#' within R.
#
#' @param FileName (character string)- path (and name (e.g. gmacs.rep)) of the report file
#' @param verbose (logical)- (TRUE/FALSE); flag to print processing information
#' @param DatFile (list)- Object containing the .dat file - This is the output
#' of the [readGMACSdat()] function.
#' @param CtlFile (list)- Object containing the .ctl file - This is the output
#' of the [readGMACSctl()] function.
#' @param nyrRetro (integer)- Number of year for the retrospective analysis
#'
#' @return the gmacs.rep file as a named list.
#'
#' \itemize{
#'   \item \code{sourcefile} - The file source.
#'   \item \code{log_q_catch} - The estimated catchability (log space).
#'   \item \code{sdnr_MAR_cpue} - The survey-specific standard deviation and
#'   median.
#'   \item \code{sdnr_MAR_lf} - The standard deviation and median for each size
#'   composition data.
#'   \item \code{Francis_weights} - The Francis weights.
#'   \item \code{slx_capture} - The capture selectivity.
#'   \item \code{slx_retaind} - The probability of retention.
#'   \item \code{slx_discard} - The probability of discard.
#'   \item \code{Selectivity} - The size-specific selectivity for the capture,
#'   retained and discards.
#'   \item \code{retained} - The first and last year probability of retention
#'   by size class.
#'   \item \code{Growth_matrix} - The sex-specific growth transition matrix.
#'   \item \code{spr_syr} - The first year for computing Rbar.
#'   \item \code{spr_nyr} - The last year for computing Rbar.
#'   \item \code{spr_rbar} - The mean recruitment for SPR calculation.
#'   \item \code{proj_rbar} - The mean recruitment for the projections.
#'   \item \code{spr_sexr} - The sex-ratio for SPR calculation.
#'   \item \code{spr_fofl} - The fishing mortality relative to MSY for the OFL.
#'   \item \code{spr_cofl_ret} - The retained portion of the OFL.
#'   \item \code{spr_yield} - The yield summary (SSB/SSB_F0; SSB/spr_Bmsy; Fbar; OFL).
#'   \item \code{Catches_like} - The likelihood for each catch data frame.
#'   \item \code{Index_like} - The likelihood for each survey data frame.
#'   \item \code{Size_comp_like} - The likelihood for each size composition data frame.
#'   \item \code{Recruit_pen} - The recruitment penalties.
#'   \item \code{Growth_like} - The likelihood for the growth data.
#'
#'
#'
#'   \item \code{MeanF_pen} - The penalty for the mean fishing mortality.
#'   \item \code{MeanF_dev} - The penalty for the fishing mortality deviations.
#'   \item \code{M_devs} - The penalty for the natural mortality.
#'   \item \code{Rec_ini} - The penalty for the initial recruitment.
#'   \item \code{Rec_dev} - The penalty for the recruitment deviations.
#'   \item \code{Sex_ratio} - The penalty for the sex-ratio.
#'   \item \code{Molt_prob_smooth} - The penalty for the smoother of the probability
#'   of molting.
#'   \item \code{Free_sel_smooth} - The penalty for the smoother of selectivity
#'    (when the parameters are freely estimated).
#'   \item \code{Initial_estimated_numbers_at_length} - The penalty for the initial number at length.
#'   \item \code{Fdevs (flt)} - The penalty for the male fishing mortality deviations.
#'   \item \code{Fdovs (flt)} - The penalty for the female fishing mortality deviations.
#'
#' }
#'
#' @seealso \code{\link{readGMACSdat}}, \code{\link{readGMACSctl}},
#' \code{\link{readGMACSallOUT}}.
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
#' # Read the gmacs.par file ----
#' repfile  <- readGMACSrep(
#' FileName = file.path(Dir_Dvpt_Vers, "build", stock, "gmacs.rep", fsep = fsep),
#' verbose = TRUE,
#' DatFile = datFile,
#' CtlFile = ctlFile,
#' nyrRetro = GMACSdat$N_Year_Retro
#' )
#' }
#'
#' @export
#' @md
#
readGMACSrep <- function(FileName = NULL,
                         verbose = TRUE,
                         DatFile = NULL,
                         CtlFile = NULL,
                         nyrRetro = NULL) {
  nsex <- DatFile$N_sexes
  Start_Y <- DatFile$Start_Y
  End_Y <- DatFile$End_Y
  nyrRetro <- End_Y - nyrRetro
  nclass <- DatFile$N_sizeC
  N_fleet <- DatFile$N_fleet
  nmature <- DatFile$N_maturity
  nshell <- DatFile$N_shell_cdt

  nam_sex <- base::switch(
    .ac(nsex),
    "0" = "Undet_Both",
    "1" = "Male",
    "2" = c("Male", "Female")
  )
  nam_mat <- base::switch(.ac(nmature),
                          "1" = "BothMature",
                          "2" = c("Mature", "Immature"))
  nam_shell <- base::switch(.ac(nshell),
                            "1" = "Undet_shell",
                            "2" = c("New_shell", "Old_shell"))

  # 1- Internal functions
  # -------------------------------------------------------------------------

  # @title get.namVar
  #
  # @description Extract a character string at a specific location.
  #
  # @param dat the object in which the value is searched
  # @param Loc the position of the value
  #
  # @return the value and increment Loc in the parent environment.
  #
  get.namVar <- function(dat, Loc, LocDatOut) {
    assign("Loc", Loc + 1, parent.frame())
    assign("LocDatOut", LocDatOut + 1, parent.frame())
    dat[Loc]
  }

  # @title get.num & do.num
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
  do.num <- function(dat, DatOut, Loc, LocDatOut) {
    # eval(parse(text = paste0(
    #   "DatOut[['",get.namVar(dat, Loc, LocDatOut),"']] <- NA"
    # )))
    # DatOut[LocDatOut] <-
    #   get.num(dat, Loc)
    tmpnam <- get.namVar(dat, Loc, LocDatOut)
    if (tmpnam == "")
      tmpnam <- get.namVar(dat, Loc, LocDatOut)
    eval(parse(text = paste0(
      "DatOut[['", tmpnam, "']] <- get.num(dat, Loc)"
    )))

    assign("Loc", Loc, parent.frame())
    assign("LocDatOut", LocDatOut, parent.frame())

    return(DatOut)
  }

  # @title get.vec & do.vec
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
  do.vec <- function(dat, DatOut, Loc, LocDatOut) {
    # eval(parse(text = paste0(
    #   "DatOut[['",get.namVar(dat, Loc, LocDatOut),"']] <- NA"
    # )))
    # DatOut[LocDatOut] <-
    #   get.vec(dat, Loc)
    tmpnam <- get.namVar(dat, Loc, LocDatOut)
    if (tmpnam == "")
      tmpnam <- get.namVar(dat, Loc, LocDatOut)
    eval(parse(text = paste0(
      "DatOut[['", tmpnam, "']] <- get.vec(dat, Loc)"
    )))

    assign("Loc", Loc, parent.frame())
    assign("LocDatOut", LocDatOut, parent.frame())

    return(DatOut)
  }

  # @title get.df & do.df
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
  do.df <- function(dat, DatOut, Loc, LocDatOut, nrow = NULL) {
    # eval(parse(text = paste0(
    #   "DatOut[['",get.namVar(dat, Loc, LocDatOut),"']] <- NA"
    # )))
    # DatOut[LocDatOut] <-
    #   get.df(dat, Loc, nrow = nrow)
    tmpnam <- get.namVar(dat, Loc, LocDatOut)
    if (tmpnam == "")
      tmpnam <- get.namVar(dat, Loc, LocDatOut)
    eval(parse(
      text = paste0("DatOut[['", tmpnam, "']] <- get.df(dat, Loc, nrow = nrow)")
    ))

    assign("Loc", Loc, parent.frame())
    assign("LocDatOut", LocDatOut, parent.frame())

    return(DatOut)
  }

  # 2- Read the REPORT file and find the first line containing numeric data
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("\n")
    cat("====================================================\n")
    cat("-> Reading REPORT file \n")
    cat("====================================================")
    cat("\n")
  }

  dat <- readLines(FileName, warn = FALSE)

  # fleetname <- which(stringr::str_detect(dat, "^\\#.*") == F)[1]
  # Com <-
  #   grep(x = dat[seq_len(fleetname - 1)],
  #        pattern = "^#",
  #        value = TRUE)
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

  DatOut <- list()
  DatOut[["sourcefile"]] <- FileName
  # DatOut[["Comments"]] <- ifelse(test = length(Com) == 0,
  #                                yes = NA,
  #                                no = Com)

  # Initialize the location index
  Loc <- 1
  LocDatOut <- length(DatOut)
  # -------------------------------------------------------------------------

  # Useful to name colnames and rownames
  .sex <- rep(nam_sex, each = (End_Y + 1 - Start_Y + 1))
  .year <- rep(Start_Y:(End_Y + 1), nsex)
  .sizeC <- paste0("SizeC_", 1:nclass)

  # Catch data ----
  if (verbose)
    cat("-- Reading catch / survey specifications (q, sdnr, francis weights) \n")

  # log_q_catch
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  # sdnr_MAR_cpue
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = DatFile$N_SurveyDF)
  if (length(DatFile$Survey_names) == 1 &&
      DatFile$Survey_names == "") {
    rownames(DatOut$sdnr_MAR_cpue) <- rownames(DatOut$survey_q)
  } else {
    rownames(DatOut$sdnr_MAR_cpue) <- DatFile$Survey_names
  }
  colnames(DatOut$sdnr_MAR_cpue) <- c("sdnr", "MAR")

  # sdnr_MAR_lf
  # Size data: standard deviation and median
  spc_nam <- NULL
  oldk <- 0
  for (n in 1:DatFile$N_SizeFreq_df) {
    k <- CtlFile$iCompAggregator[n]

    if (oldk != k) {
      tmp_nam <- tolower(names(DatFile$SizeFreq)[n])
    } else {
      tmp_nam <- NULL
      spc_nam[oldk] <- gsub(
        x = tolower(spc_nam[oldk]),
        pattern = paste(paste0(nam_sex, "_"), collapse = "|"),
        replacement = "Aggr_"
      )
    }
    spc_nam <- c(spc_nam, tmp_nam)
    oldk <- k
  }
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(unique(CtlFile$iCompAggregator)))
  rownames(DatOut$sdnr_MAR_lf) <- spc_nam
  colnames(DatOut$sdnr_MAR_lf) <- c("sdnr", "MAR")

  # Francis weights
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  if (verbose)
    cat("\t-> Read catch / survey specifications \n")
  # -------------------------------------------------------------------------

  # Selectivity ----
  if (verbose)
    cat("-- Reading selectivity \n")

  # slx_capture
  nRowSlx <- length(Start_Y:End_Y) * nsex * N_fleet
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowSlx)
  # slx_retaind
  nRowRet <- length(Start_Y:End_Y) * nsex * N_fleet
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowRet)
  # slx_discard
  nRowDis <- length(Start_Y:End_Y) * nsex * N_fleet
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowDis)
  # Colnames
  colnames(DatOut$slx_capture) <-
    colnames(DatOut$slx_retaind) <-
    colnames(DatOut$slx_discard) <- c("year", 'sex', 'fleet', .sizeC)

  # Start and end year selectivity per fleet and sex
  nRowSel <- 2 * nsex * N_fleet
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowSel)
  colnames(DatOut$Selectivity) <- c("year", 'sex', 'fleet', .sizeC)
  # Start and end year males retained for the main fleet
  nRowSel <- 2
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowSel)
  colnames(DatOut$retained) <- c("year", 'sex', 'fleet', .sizeC)

  if (verbose)
    cat("\t-> Read selectivity \n")
  # -------------------------------------------------------------------------

  # Growth matrix----
  if (verbose)
    cat("-- Reading growth matrices \n")

  tmp <- "Growth_matrix"
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))

  for (s in 1:nsex) {
    Inc <- CtlFile$nSizeIncVaries[s]
    eval(parse(text = paste0(
      "DatOut$", tmp, "$", nam_sex[s], " <- list()"
    )))

    for (I in 1:Inc) {
      eval(parse(
        text = paste0(
          "DatOut$",
          tmp,
          "$",
          nam_sex[s],
          "$IncNo_",
          I,
          " <- get.df(dat, Loc, nrow = DatFile$N_sizeC)"
        )
      ))
      eval(parse(
        text = paste0(
          "colnames(DatOut$",
          tmp,
          "$",
          nam_sex[s],
          "$IncNo_",
          I,
          ") <- rownames(DatOut$",
          tmp,
          "$",
          nam_sex[s],
          "$IncNo_",
          I,
          ") <- .sizeC"
        )
      ))
    }
  }
  if (verbose)
    cat("\t-> Read  growth matrices \n")
  # -------------------------------------------------------------------------

  # Reference points and OFL ----
  if (verbose)
    cat("-- Reading reference points and OFL \n")

  # spr_syr
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_nyr
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_rbar
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # proj_rbar
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # spr_sexr
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # sd_fofl
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # spr_cofl_ret
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_yield
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = 101) #Number of rows defines in the .tpl
  colnames(DatOut$spr_yield) <- c("SSB/SSB_F0", "SSB/spr_Bmsy", "Fbar", "OFL")
  # Catches_like
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Index_like
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Size_comp_like
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Recruit_pen
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Growth_like
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # MeanF_pen
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # MeanF_dev
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # M_devs
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Rec_ini
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Rec_dev
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Sex_ratio
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Molt_prob_smooth
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Free_sel_smooth
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Initial estimated numbers at length
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Fdevs (flt)
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Fdovs (flt)
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  if (verbose)
    cat("\t-> Read reference points and OFL \n")
  # -------------------------------------------------------------------------

  # # Model characteristics ----
  # if (verbose)
  #   cat("-- Reading model characteristics \n")
  # # Fleet names
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = N_fleet)
  # colnames(DatOut$fleetname) <- "Fleet_names"
  # # Number of fleet
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # Number of group
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # Number of sex
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # Number of shell condition
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # Number of maturity state
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # Initial year
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # Final year
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # Number of seasons
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # Sex pointer
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # Maturity pointer
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # Shell condition pointer
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # Model years
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # Mid points
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  #
  # if (verbose)
  #   cat("\t-> Read model characteristics \n")
  # # -------------------------------------------------------------------------
  #
  # # Mean weight at size ----
  # if (verbose)
  #   cat("-- Reading mean weight at size \n")
  #
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = nsex * (End_Y + 1 - Start_Y + 1))
  # .old <- colnames(DatOut$mean_wt)
  # .new <- paste0("SizeC_", 1:nclass)
  # DatOut$mean_wt <- DatOut$mean_wt %>%
  #   dplyr::mutate(sex = .sex, year = .year) %>%
  #   dplyr::select(year, sex, dplyr::everything()) %>%
  #   dplyr::rename_with(~ .new, dplyr::all_of(.old))
  #
  # if (verbose)
  #   cat("\t-> Read mean weight at size \n")
  # # -------------------------------------------------------------------------
  #
  # # Maturity ----
  # if (verbose)
  #   cat("-- Reading maturity \n")
  #
  # DatOut <-
  #   base::switch(
  #     .ac(nsex),
  #     "1" = do.vec(dat, DatOut, Loc, LocDatOut),
  #     "2" = do.df(dat, DatOut, Loc, LocDatOut, nrow = nsex)
  #   )
  # if(nsex != 1){
  #   DatOut$maturity <- DatOut$maturity %>%
  #     dplyr::mutate(sex = nam_sex) %>%
  #     dplyr::select(sex, dplyr::everything()) %>%
  #     dplyr::rename_with(~ .new, dplyr::all_of(.old))
  # }
  #
  # if (verbose)
  #   cat("\t-> Read maturity \n")
  # # -------------------------------------------------------------------------
  #
  # # Negative loglikelihoods / Penalities / Prior density ----
  # if (verbose)
  #   cat("-- Reading Loglikelihoods / Penalities / Prior density \n")
  # # neg loglikelihoods
  # nam_likes <-
  #   c("Catch_data",
  #     "Index_data",
  #     "Size_data",
  #     "Stock_Recruit",
  #     "Tagging_data")
  #
  # nam_rowsdf <- c("CatchDF", "SurveyDF", "SizeFreq_df")
  # nam_rowsR <- c("FirstYearDevs", "InitDevs", "SexRatioDevs")
  # nam_rowsG <- if(CtlFile$bUseGrowthIncrementModel == 1 & DatFile$GrowthObsType == 1){
  #   paste(nam_sex, "Growth_IncDat",sep = "_")
  # } else if (DatFile$GrowthObsType == 2 || DatFile$GrowthObsType == 3){
  #   paste(nam_sex, "SizeClass_change",sep = "_")
  # }
  #
  # nlikes <- length(nam_likes)
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  #
  # for (l in 1:nlikes) {
  #   namRows <- if(nam_likes[l] %in% c("Catch_data","Index_data","Size_data")){
  #     eval(parse(text = paste0(
  #       "paste0('",nam_rowsdf[l],"', 1:DatFile$N_",nam_rowsdf[l],")"
  #     )))
  #   } else if(nam_likes[l] == "Stock_Recruit"){
  #     nam_rowsR
  #   } else {
  #     nam_rowsG
  #   }
  #   if(nam_likes[l] == "Size_data")
  #     namRows <- namRows[unique(CtlFile$iCompAggregator)]
  #
  #   eval(parse(text = paste0(
  #     "DatOut$", tmp, "$", nam_likes[l], " <- get.vec(dat, Loc)"
  #   )))
  #
  #   eval(parse(text = paste0(
  #     "DatOut$", tmp, "$", nam_likes[l], " <- as.data.frame(DatOut$",tmp, "$",
  #     nam_likes[l], ", row.names = namRows)"
  #   )))
  #   eval(parse(text = paste0(
  #     "colnames(DatOut$", tmp, "$", nam_likes[l], ") <- 'nloglike'"
  #   )))
  # }
  # # nlogPenalty
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # namPenal <- c(
  #   "Log_fdevs",
  #   "meanF",
  #   "Mdevs",
  #   "Rec_devs",
  #   "Initial_devs",
  #   "Fst_dif_dev",
  #   "Mean_sex-Ratio",
  #   "Molt_prob",
  #   "Free_selectivity",
  #   "Init_n_at_len",
  #   "Fvecs",
  #   "Fdovs",
  #   "Vul_devs"
  # )
  # DatOut$nlogPenalty <- as.data.frame(DatOut$nlogPenalty, row.names = namPenal)
  # colnames(DatOut$nlogPenalty) <- "nlogPenalty"
  #
  # # priorDensity
  # # priorDensity for each estimated parameters
  # # NVarpar values - Need to implement with the names
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  #
  # if (verbose)
  #   cat("\t-> Read Loglikelihoods / Penalties / Prior density \n")
  # # -------------------------------------------------------------------------
  #
  # # Catch data ----
  # if (verbose)
  #   cat("-- Reading catch data and derivates \n")
  # namCatch <- c("Year","Season",'Fleet',"Sex","Obs","CV","Partition","Units","Multiplier","Effort","Discard_mortality")
  #
  # # dCatchData
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = sum(DatFile$Nrows_CatchDF))
  # colnames(DatOut$dCatchData) <- namCatch
  #
  # # obs_catch
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (c in 1:DatFile$N_CatchDF) {
  #   eval(parse(text = paste0(
  #     "DatOut$",
  #     tmp,
  #     "$",
  #     names(DatFile$Catch)[c],
  #     " <- get.vec(dat, Loc)"
  #   )))
  # }
  # # obs_effort
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (c in 1:DatFile$N_CatchDF) {
  #   eval(parse(text = paste0(
  #     "DatOut$",
  #     tmp,
  #     "$",
  #     names(DatFile$Catch)[c],
  #     " <- get.vec(dat, Loc)"
  #   )))
  # }
  # # pre_catch
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (c in 1:DatFile$N_CatchDF) {
  #   eval(parse(text = paste0(
  #     "DatOut$",
  #     tmp,
  #     "$",
  #     names(DatFile$Catch)[c],
  #     " <- get.vec(dat, Loc)"
  #   )))
  # }
  # # res_catch
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (c in 1:DatFile$N_CatchDF) {
  #   eval(parse(text = paste0(
  #     "DatOut$",
  #     tmp,
  #     "$",
  #     names(DatFile$Catch)[c],
  #     " <- get.vec(dat, Loc)"
  #   )))
  # }
  #
  # # log_q_catch
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # obs_catch_out
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (c in 1:DatFile$N_CatchDF) {
  #   eval(parse(text = paste0(
  #     "DatOut$",
  #     tmp,
  #     "$",
  #     names(DatFile$Catch)[c],
  #     " <- get.vec(dat, Loc)"
  #   )))
  # }
  # # obs_catch_effort
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (c in 1:DatFile$N_CatchDF) {
  #   eval(parse(text = paste0(
  #     "DatOut$",
  #     tmp,
  #     "$",
  #     names(DatFile$Catch)[c],
  #     " <- get.vec(dat, Loc)"
  #   )))
  # }
  # # pre_catch_out
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (c in 1:DatFile$N_CatchDF) {
  #   eval(parse(text = paste0(
  #     "DatOut$",
  #     tmp,
  #     "$",
  #     names(DatFile$Catch)[c],
  #     " <- get.vec(dat, Loc)"
  #   )))
  # }
  # # res_catch_out
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (c in 1:DatFile$N_CatchDF) {
  #   eval(parse(text = paste0(
  #     "DatOut$",
  #     tmp,
  #     "$",
  #     names(DatFile$Catch)[c],
  #     " <- get.vec(dat, Loc)"
  #   )))
  # }
  # # dCatchData_out
  # DatOut <-
  #   do.df(dat,
  #         DatOut,
  #         Loc,
  #         LocDatOut,
  #         nrow = length(Start_Y:End_Y) * DatFile$N_CatchDF)
  # colnames(DatOut$dCatchData_out) <- namCatch
  #
  # if (verbose)
  #   cat("\t-> Read catch data and derivates \n")
  # # -------------------------------------------------------------------------
  #
  #
  # # Survey data ----
  # if (verbose)
  #   cat("-- Reading survey data and derivates \n")
  # # dSurveyData
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = DatFile$Nrows_SvDF)
  # colnames(DatOut$dSurveyData) <- c("Index","Year","Season","Fleet","Sex","Maturity","Obs","CV","Units","CPUE_time")
  # # cpue_cv_add
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # obs_cpue
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # pre_cpue
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # res_cpue
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  #
  # # survey_q
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # if(length(DatFile$Survey_names)==1 && DatFile$Survey_names == ""){
  #   DatOut$survey_q <- as.data.frame(DatOut$survey_q, row.names = paste0("CPUE_", 1:length(DatOut$survey_q)))
  # } else {
  #   DatOut$survey_q <- as.data.frame(DatOut$survey_q, row.names = DatFile$Survey_names)
  # }
  #
  # # sdnr_MAR_cpue
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = DatFile$N_SurveyDF)
  #
  # if(length(DatFile$Survey_names)==1 && DatFile$Survey_names == ""){
  #   rownames(DatOut$sdnr_MAR_cpue) <- rownames(DatOut$survey_q)
  # } else {
  #   rownames(DatOut$sdnr_MAR_cpue) <- DatFile$Survey_names
  # }
  # colnames(DatOut$sdnr_MAR_cpue) <- c("sdnr", "MAR")
  #
  # if (verbose)
  #   cat("\t-> Read survey data and derivates \n")
  # # -------------------------------------------------------------------------
  #
  # # Size composition data ----
  # if (verbose)
  #   cat("-- Reading size composition \n")
  #
  # # namSizeC <- c('Year','Season','Fleet','Sex','Type','Shell','Maturity','Stage1_EffN',.new)
  # namSizeC <- c('Year','Season','Fleet','Sex','Type','Shell','Maturity','Stage1_EffN')
  #
  # # d3_SizeComps_in
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  #
  # for (s in 1:DatFile$N_SizeFreq_df) {
  #   namSize <- paste0("SizeC_", 1:DatFile$Nbins_SiseFreq[s])
  #   namSizeC2 <- c(namSizeC, namSize)
  #
  #   eval(parse(
  #     text = paste0(
  #       "DatOut$",
  #       tmp,
  #       "$",
  #       names(DatFile$SizeFreq)[s],
  #       " <- get.df(dat, Loc, nrow = ",
  #       DatFile$Nrows_SiseFreqDF[s],
  #       ")"
  #     )
  #   ))
  #   eval(parse(
  #     text = paste0(
  #       "colnames(DatOut$",
  #       tmp,
  #       "$",
  #       names(DatFile$SizeFreq)[s],
  #       ") <- namSizeC2"
  #     )
  #   ))
  # }
  #
  # # d3_obs_size_comps_out
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (s in 1:DatFile$N_SizeFreq_df) {
  #   namSize <- paste0("SizeC_", 1:DatFile$Nbins_SiseFreq[s])
  #
  #   eval(parse(
  #     text = paste0(
  #       "DatOut$",
  #       tmp,
  #       "$",
  #       names(DatFile$SizeFreq)[s],
  #       " <- get.df(dat, Loc, nrow = ",
  #       DatFile$Nrows_SiseFreqDF[s],
  #       ")"
  #     )
  #   ))
  #   eval(parse(
  #     text = paste0(
  #       "colnames(DatOut$",
  #       tmp,
  #       "$",
  #       names(DatFile$SizeFreq)[s],
  #       ") <- namSize"
  #     )
  #   ))
  # }
  #
  # # d3_pre_size_comps_out
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (s in 1:DatFile$N_SizeFreq_df) {
  #   namSize <- paste0("SizeC_", 1:DatFile$Nbins_SiseFreq[s])
  #
  #   eval(parse(
  #     text = paste0(
  #       "DatOut$",
  #       tmp,
  #       "$",
  #       names(DatFile$SizeFreq)[s],
  #       " <- get.df(dat, Loc, nrow = ",
  #       DatFile$Nrows_SiseFreqDF[s],
  #       ")"
  #     )
  #   ))
  #   eval(parse(
  #     text = paste0(
  #       "colnames(DatOut$",
  #       tmp,
  #       "$",
  #       names(DatFile$SizeFreq)[s],
  #       ") <- namSize"
  #     )
  #   ))
  # }
  #
  # # d3_res_size_comps_out
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (s in 1:DatFile$N_SizeFreq_df) {
  #   namSize <- paste0("SizeC_", 1:DatFile$Nbins_SiseFreq[s])
  #
  #   eval(parse(
  #     text = paste0(
  #       "DatOut$",
  #       tmp,
  #       "$",
  #       names(DatFile$SizeFreq)[s],
  #       " <- get.df(dat, Loc, nrow = ",
  #       DatFile$Nrows_SiseFreqDF[s],
  #       ")"
  #     )
  #   ))
  #   eval(parse(
  #     text = paste0(
  #       "colnames(DatOut$",
  #       tmp,
  #       "$",
  #       names(DatFile$SizeFreq)[s],
  #       ") <- namSize"
  #     )
  #   ))
  # }
  #
  # # Specific for length comp
  # iCompAggregator <- CtlFile$iCompAggregator
  # Nbins_SiseFreq <- DatFile$Nbins_SiseFreq
  # Nrows_SiseFreqDF <- DatFile$Nrows_SiseFreqDF
  # N_SizeFreq_df <- DatFile$N_SizeFreq_df
  #
  # Nlen <- data.frame(cbind(iCompAggregator,
  #                          Nbins_SiseFreq,
  #                          Nrows_SiseFreqDF))
  # Nlen$Nlen <- -1
  # oldk <- 0
  # for (n in 1:N_SizeFreq_df) {
  #   k <- iCompAggregator[n]
  #
  #   if (oldk != k) {
  #     Nlen$Nlen[n] <- Nlen$Nbins_SiseFreq[n]
  #   } else {
  #     Nlen$Nlen[n-1] <- Nlen$Nbins_SiseFreq[n-1] + Nlen$Nbins_SiseFreq[n]
  #     Nlen$Nlen[n] <- Nlen$Nlen[n-1]
  #   }
  #   oldk <- k
  # }
  # NlenGen <- unique(Nlen[,!colnames(Nlen)%in%"Nbins_SiseFreq"])
  #
  # # d3_obs_size_comps_X
  # oldk <- 0
  # Tottmp <- NULL
  # for (n in 1:DatFile$N_SizeFreq_df) {
  #   k <- iCompAggregator[n]
  #
  #   Nbins <- NlenGen[k,"Nlen"]
  #   if(Nbins>nclass){
  #     namSize <- c(paste0("SizeC_obs_", 1:nclass),paste0("SizeC_obs_Aggr", 1:(Nbins - nclass)))
  #   } else {
  #     namSize <- paste0("SizeC_obs_", 1:Nbins)
  #   }
  #
  #   if (oldk != k) {
  #     tmp <- get.namVar(dat, Loc, LocDatOut)
  #     tmp <- names(DatOut$d3_SizeComps_in)[k]
  #     eval(parse(
  #       text = paste0(
  #         "Tottmp$",
  #         tmp,
  #         " <- get.df(dat, Loc, nrow = ",
  #         DatFile$Nrows_SiseFreqDF[n],
  #         ")"
  #       )
  #     ))
  #     eval(parse(
  #       text = paste0(
  #         "colnames(Tottmp$",
  #         tmp,
  #         ") <- namSize"
  #       )
  #     ))
  #     eval(parse(
  #       text = paste0(
  #         "Tottmp$",tmp,"<- Tottmp$",tmp," %>%
  #       dplyr::mutate(sex = DatOut$d3_SizeComps_in[[",n,"]]$Sex,
  #                     year = DatOut$d3_SizeComps_in[[",n,"]]$Year) %>%
  #       dplyr::select(year, sex, dplyr::everything())"
  #       )
  #     ))
  #   }
  #   oldk <- k
  # }
  # DatOut[["d3_obs_size_comps"]] <- Tottmp
  #
  #
  # # d3_pre_size_comps_X
  # oldk <- 0
  # Tottmp <- NULL
  # for (n in 1:DatFile$N_SizeFreq_df) {
  #   k <- CtlFile$iCompAggregator[n]
  #
  #   Nbins <- NlenGen[k,"Nlen"]
  #   if(Nbins>nclass){
  #     namSize <- c(paste0("SizeC_obs_", 1:nclass),paste0("SizeC_obs_Aggr", 1:(Nbins - nclass)))
  #   } else {
  #     namSize <- paste0("SizeC_obs_", 1:Nbins)
  #   }
  #
  #   if (oldk != k) {
  #     tmp <- get.namVar(dat, Loc, LocDatOut)
  #     tmp <- names(DatOut$d3_SizeComps_in)[k]
  #     eval(parse(
  #       text = paste0(
  #         "Tottmp$",
  #         tmp,
  #         " <- get.df(dat, Loc, nrow = ",
  #         DatFile$Nrows_SiseFreqDF[n],
  #         ")"
  #       )
  #     ))
  #     eval(parse(
  #       text = paste0(
  #         "colnames(Tottmp$",
  #         tmp,
  #         ") <- namSize"
  #       )
  #     ))
  #     eval(parse(
  #       text = paste0(
  #         "Tottmp$",tmp,"<- Tottmp$",tmp," %>%
  #       dplyr::mutate(sex = DatOut$d3_SizeComps_in[[",n,"]]$Sex,
  #                     year = DatOut$d3_SizeComps_in[[",n,"]]$Year) %>%
  #       dplyr::select(year, sex, dplyr::everything())"
  #       )
  #     ))
  #   }
  #   oldk <- k
  # }
  # DatOut[["d3_pre_size_comps"]] <- Tottmp
  #
  # # d3_res_size_comps_X
  # oldk <- 0
  # Tottmp <- NULL
  # for (n in 1:DatFile$N_SizeFreq_df) {
  #   k <- CtlFile$iCompAggregator[n]
  #
  #   Nbins <- NlenGen[k,"Nlen"]
  #   if(Nbins>nclass){
  #     namSize <- c(paste0("SizeC_obs_", 1:nclass),paste0("SizeC_obs_Aggr", 1:(Nbins - nclass)))
  #   } else {
  #     namSize <- paste0("SizeC_obs_", 1:Nbins)
  #   }
  #
  #   if (oldk != k) {
  #     tmp <- get.namVar(dat, Loc, LocDatOut)
  #     tmp <- names(DatOut$d3_SizeComps_in)[k]
  #     eval(parse(
  #       text = paste0(
  #         "Tottmp$",
  #         tmp,
  #         " <- get.df(dat, Loc, nrow = ",
  #         DatFile$Nrows_SiseFreqDF[n],
  #         ")"
  #       )
  #     ))
  #     eval(parse(
  #       text = paste0(
  #         "colnames(Tottmp$",
  #         tmp,
  #         ") <- namSize"
  #       )
  #     ))
  #     eval(parse(
  #       text = paste0(
  #         "Tottmp$",tmp,"<- Tottmp$",tmp," %>%
  #       dplyr::mutate(sex = DatOut$d3_SizeComps_in[[",n,"]]$Sex,
  #                     year = DatOut$d3_SizeComps_in[[",n,"]]$Year) %>%
  #       dplyr::select(year, sex, dplyr::everything())"
  #       )
  #     ))
  #   }
  #   oldk <- k
  # }
  # DatOut[["d3_res_size_comps_X"]] <- Tottmp
  #
  # # size_comp_sample_size
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  #
  # oldk <- 0
  # for (n in 1:DatFile$N_SizeFreq_df) {
  #   k <- CtlFile$iCompAggregator[n]
  #   if (oldk != k) {
  #     namtmp <- names(DatOut$d3_SizeComps_in)[k]
  #     eval(parse(
  #       text = paste0("DatOut$", tmp, "$", namtmp,
  #                     " <- get.vec(dat, Loc)")
  #     ))
  #
  #   }
  #   oldk <- k
  # }
  #
  # # sdnr_MAR_lf
  # # Size data: standard deviation and median
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(unique(CtlFile$iCompAggregator)))
  # rownames(DatOut$sdnr_MAR_lf) <- names(DatOut$d3_SizeComps_in)[unique(CtlFile$iCompAggregator)]
  # colnames(DatOut$sdnr_MAR_lf) <- c("sdnr", "MAR")
  # if (verbose)
  #   cat("\t-> Read size composition \n")
  # # -------------------------------------------------------------------------
  #
  # # Natural, Fishing and Total mortality ----
  # if (verbose)
  #   cat("-- Reading natural, fishing and total mortality \n")
  # # m_prop
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro))
  # rownames(DatOut$m_prop) <- Start_Y:nyrRetro
  # colnames(DatOut$m_prop) <- paste0("Season_", 1:DatFile$N_seasons)
  #
  # # M
  # nRowM <- length(Start_Y:nyrRetro)
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  #
  # for(s in 1:nsex){
  #   for(m in 1:nmature){
  #     eval(parse(text = paste0(
  #       "DatOut$", tmp, "$",nam_sex[s],"$",nam_mat[m]," <- get.df(dat, Loc, nrow = nRowM)"
  #     )))
  #     eval(parse(text = paste0(
  #       "colnames(DatOut$", tmp, "$",nam_sex[s],"$",nam_mat[m],") <- .new"
  #     )))
  #     eval(parse(text = paste0(
  #       "rownames(DatOut$", tmp, "$",nam_sex[s],"$",nam_mat[m],") <- .ac(Start_Y:nyrRetro)"
  #     )))
  #
  #   }
  # }
  #
  # # xi
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # DatOut$xi <- as.data.frame(DatOut$xi, row.names = .ac(Start_Y:nyrRetro))
  # # log_fbar
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # if(length(DatFile$Survey_names)==1 && DatFile$Survey_names=="") {
  #   DatOut$log_fbar <- as.data.frame(DatOut$log_fbar,
  #                                    row.names = DatFile$F_Fleet_names)
  # } else {
  #   DatOut$log_fbar <- as.data.frame(DatOut$log_fbar,
  #                                    row.names = c(DatFile$F_Fleet_names, DatFile$Survey_names))
  # }
  #
  # # ft
  # nRowft <- N_fleet * nsex * length(Start_Y:nyrRetro)
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowft)
  # colnames(DatOut$ft) <- paste0("Season_", 1:DatFile$N_seasons)
  # if(length(DatFile$Survey_names)==1 && DatFile$Survey_names=="") {
  #   DatOut$ft <- DatOut$ft %>%
  #     dplyr::mutate(fleet = rep(DatFile$F_Fleet_names, each = length(Start_Y:nyrRetro)*nsex),
  #                   sex = rep(rep(nam_sex,each = length(Start_Y:nyrRetro)),N_fleet),
  #                   year = rep(Start_Y:nyrRetro, N_fleet*nsex)) %>%
  #     dplyr::select(year, sex, fleet, dplyr::everything())
  # } else {
  #   DatOut$ft <- DatOut$ft %>%
  #     dplyr::mutate(fleet = rep(c(DatFile$F_Fleet_names, DatFile$Survey_names), each = length(Start_Y:nyrRetro)*nsex),
  #                   sex = rep(rep(nam_sex,each = length(Start_Y:nyrRetro)),N_fleet),
  #                   year = rep(Start_Y:nyrRetro, N_fleet*nsex)) %>%
  #     dplyr::select(year, sex, fleet, dplyr::everything())
  # }
  #
  # # F
  # nRowF <- nsex * length(Start_Y:nyrRetro) * DatFile$N_seasons
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowF)
  # colnames(DatOut$F) <- .new
  # DatOut$F <- DatOut$F %>%
  #   dplyr::mutate(sex = rep(nam_sex, each=DatFile$N_seasons*length(Start_Y:nyrRetro)),
  #                 # year = rep(Start_Y:nyrRetro, DatFile$N_seasons*nsex),
  #                 year = rep(rep(Start_Y:nyrRetro, each = DatFile$N_seasons),nsex),
  #                 # season = rep(1:DatFile$N_seasons, each = length(Start_Y:nyrRetro), nsex)
  #                 season = rep(1:DatFile$N_seasons, length(Start_Y:nyrRetro)* nsex)) %>%
  #   dplyr::select(year, sex, season,dplyr::everything())
  #
  # # selectivity
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # nam_year <- c("Start_Y", "End_Y")
  # for (y in 1:2) {
  #   eval(parse(
  #     text = paste0(
  #       "DatOut$",
  #       tmp,
  #       "$",
  #       nam_year[y],
  #       " <- get.df(dat, Loc, nrow = nsex*N_fleet)"
  #     )
  #   ))
  #   eval(parse(
  #     text = paste0(
  #       "colnames(DatOut$",
  #       tmp,
  #       "$",
  #       nam_year[y],
  #       ") <- c('year', 'sex', 'fleet', .new)"
  #     )
  #   ))
  # }
  #
  # # Retained
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # nam_year <- c("Start_Y", "End_Y")
  # for (y in 1:2) {
  #   eval(parse(text = paste0(
  #     "DatOut$", tmp, "$", nam_year[y],
  #     " <- get.vec(dat, Loc)"
  #   )))
  #
  #   eval(parse(text = paste0(
  #     "DatOut$", tmp, "$", nam_year[y],
  #     " <- t(as.data.frame(DatOut$", tmp, "$", nam_year[y],", byrow = FALSE))"
  #   )))
  #   eval(parse(text = paste0(
  #     "colnames(DatOut$", tmp, "$", nam_year[y],
  #     ") <- c('year', 'sex', 'fleet', .new)"
  #   )))
  #   eval(parse(text = paste0(
  #     "rownames(DatOut$", tmp, "$", nam_year[y],
  #     ") <- NULL"
  #   )))
  # }
  #
  # if (verbose)
  #   cat("\t-> Read Natural mortality, fishing and total mortality \n")
  # # -------------------------------------------------------------------------
  #
  #
  # # Recuitment ----
  # if (verbose)
  #   cat("-- Reading Recuitment \n")
  #
  # # rec_sdd
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nsex)
  # colnames(DatOut$rec_sdd) <- .new; rownames(DatOut$rec_sdd)<-nam_sex
  # # rec_ini
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # rec_dev
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # logit_rec_prop
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # recruits
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nsex)
  # DatOut$recruits <- data.frame(t(DatOut$recruits))
  # colnames(DatOut$recruits) <- nam_sex
  # DatOut$recruits <- DatOut$recruits %>%
  #   dplyr::mutate(year = Start_Y:nyrRetro)
  # # res_recruit
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  #
  # if (verbose)
  #   cat("\t-> Read Recruitment \n")
  # # -------------------------------------------------------------------------
  #
  #
  # # SSB and N----
  # if (verbose)
  #   cat("-- Reading SSB and N \n")
  # # ssb
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # dyn_Bzero
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # N_initial
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = DatOut$n_grp)
  # .old <- colnames(DatOut$N_initial)
  # DatOut$N_initial <- DatOut$N_initial %>%
  #   dplyr::mutate(sex = rep(nam_sex, each=nmature*nshell),
  #                 maturity = rep(rep(nam_mat, each = nshell), nsex),
  #                 shell_cond = rep(nam_shell, nmature* nsex)) %>%
  #   dplyr::select(sex, maturity, shell_cond, dplyr::everything()) %>%
  #   dplyr::rename_with(~ .new, dplyr::all_of(.old))
  #
  # # N_total
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # # N_males
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # # N_females
  # # if(nsex>1)
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # # N_males_new
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # # N_females_new
  # # if(nsex>1)
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # # N_males_old
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # # N_females_old
  # # if(nsex>1)
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # # N_males_mature
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # # N_females_mature
  # # if(nsex>1)
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  #
  # # Rename column and add years
  # namN <- which(stringr::str_detect(string = names(DatOut[!names(DatOut)%in%"N_initial"]), pattern = "N_"))+1
  # for(n in 1:length(namN)){
  #   colnames(DatOut[[namN[n]]]) <- .new
  #   DatOut[[namN[n]]] <- DatOut[[namN[n]]] %>%
  #     dplyr::mutate(year = Start_Y:(nyrRetro+1)) %>%
  #     dplyr::select(year, dplyr::everything())
  # }
  #
  # if (verbose)
  #   cat("\t-> Read SSB and N \n")
  # # -------------------------------------------------------------------------
  #
  #
  # # Molting and growth ----
  # if (verbose)
  #   cat("-- Reading molting and growth \n")
  #
  # # molt_increment
  # nRowMoltInc <- max(CtlFile$nSizeIncVaries) * nsex
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowMoltInc)
  # .old <- colnames(DatOut$molt_increment)
  # DatOut$molt_increment <- DatOut$molt_increment %>%
  #   dplyr::mutate(sex = rep(nam_sex, each=max(CtlFile$nSizeIncVaries)),
  #                 SizeIncPeriod = rep(CtlFile$nSizeIncVaries, each=max(CtlFile$nSizeIncVaries))) %>%
  #   dplyr::select(sex, SizeIncPeriod, dplyr::everything()) %>%
  #   dplyr::rename_with(~ .new, dplyr::all_of(.old))
  #
  # # dPreMoltSize
  # if (DatFile$NGrowthObs > 0) {
  #   DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # } else {
  #   Loc <- Loc + 2
  #   LocDatOut <- LocDatOut + 2
  # }
  # # iMoltIncSex
  # if (DatFile$NGrowthObs > 0) {
  #   DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # } else {
  #   Loc <- Loc + 2
  #   LocDatOut <- LocDatOut + 2
  # }
  # # dMoltInc
  # if (DatFile$NGrowthObs > 0) {
  #   DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # } else {
  #   Loc <- Loc + 2
  #   LocDatOut <- LocDatOut + 2
  # }
  #
  # # molt_probability
  # # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro)*nsex)
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  # for (s in 1:nsex) {
  #   eval(parse(
  #     text = paste0(
  #       "DatOut$",
  #       tmp,
  #       "$",
  #       nam_sex[s],
  #       " <- get.df(dat, Loc, nrow = length(Start_Y:nyrRetro))"
  #     )
  #   ))
  #   eval(parse(
  #     text = paste0(
  #       "colnames(DatOut$",
  #       tmp,
  #       "$",
  #       nam_sex[s],
  #       ") <- .new"
  #     )
  #   ))
  #   eval(parse(
  #     text = paste0(
  #       "DatOut$",
  #       tmp,
  #       "$",
  #       nam_sex[s],
  #       "<- DatOut$",
  #       tmp,
  #       "$",
  #       nam_sex[s]," %>% dplyr::mutate(year = Start_Y:End_Y) %>%
  #       dplyr::select(year, dplyr::everything())"
  #     )
  #   ))
  # }
  #
  # if (verbose)
  #   cat("\t-> Read molting and growth \n")
  # # -------------------------------------------------------------------------
  #
  # End of data file
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("====================================================\n")
    cat("Read of gmacs.rep file complete.")
    cat("\n")
  }

  return(DatOut)
}
