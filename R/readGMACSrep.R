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
#' @seealso \code{\link{readGMACSdat}}, \code{\link{readGMACSctl}}, \code{\link{readGMACSallOUT}}
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

  fleetname <- which(stringr::str_detect(dat, "^\\#.*") == F)[1]
  Com <-
    grep(x = dat[seq_len(fleetname - 1)],
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
  # dat <- dat[dat != ""]

  DatOut <- list()
  DatOut[["sourcefile"]] <- FileName
  DatOut[["Comments"]] <- ifelse(test = length(Com) == 0,
                                 yes = NA,
                                 no = Com)

  # Initialize the location index
  Loc <- 1
  LocDatOut <- length(DatOut)
  # -------------------------------------------------------------------------

  # Model characteristics ----
  if (verbose)
    cat("-- Reading model characteristics \n")
  # Fleet names
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = N_fleet)
  # Number of fleet
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Number of group
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Number of sex
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Number of shell condition
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Number of maturity state
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Initial year
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Final year
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Number of seasons
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # Sex pointer
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Maturity pointer
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Shell condition pointer
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Model years
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # Mid points
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  if (verbose)
    cat("\t-> Read model characteristics \n")
  # -------------------------------------------------------------------------

  # Mean weight at size ----
  if (verbose)
    cat("-- Reading mean weight at size \n")

  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = nsex * (End_Y + 1 - Start_Y + 1))

  if (verbose)
    cat("\t-> Read mean weight at size \n")
  # -------------------------------------------------------------------------

  # Maturity ----
  if (verbose)
    cat("-- Reading maturity \n")

  DatOut <-
    base::switch(
      .ac(nsex),
      "1" = do.vec(dat, DatOut, Loc, LocDatOut),
      "2" = do.df(dat, DatOut, Loc, LocDatOut, nrow = nsex)
    )

  if (verbose)
    cat("\t-> Read maturity \n")
  # -------------------------------------------------------------------------

  # Negative loglikelihoods / Penalities / Prior density ----
  if (verbose)
    cat("-- Reading Loglikelihoods / Penalities / Prior density \n")
  # neg loglikelihoods
  nam_likes <-
    c("Catch_data",
      "Index_data",
      "Size_data",
      "Stock_Recruit",
      "Tagging_data")
  nlikes <- length(nam_likes)
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (l in 1:nlikes) {
    eval(parse(text = paste0(
      "DatOut$", tmp, "$", nam_likes[l], " <- get.vec(dat, Loc)"
    )))
  }
  # nlogPenalty
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # priorDensity
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  if (verbose)
    cat("\t-> Read Loglikelihoods / Penalties / Prior density \n")
  # -------------------------------------------------------------------------


  # Catch data ----
  if (verbose)
    cat("-- Reading catch data and derivates \n")

  # dCatchData
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = sum(DatFile$Nrows_CatchDF))
  # obs_catch
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (c in 1:DatFile$N_CatchDF) {
    eval(parse(text = paste0(
      "DatOut$",
      tmp,
      "$",
      names(DatFile$Catch)[c],
      " <- get.vec(dat, Loc)"
    )))
  }
  # obs_effort
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (c in 1:DatFile$N_CatchDF) {
    eval(parse(text = paste0(
      "DatOut$",
      tmp,
      "$",
      names(DatFile$Catch)[c],
      " <- get.vec(dat, Loc)"
    )))
  }
  # pre_catch
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (c in 1:DatFile$N_CatchDF) {
    eval(parse(text = paste0(
      "DatOut$",
      tmp,
      "$",
      names(DatFile$Catch)[c],
      " <- get.vec(dat, Loc)"
    )))
  }
  # res_catch
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (c in 1:DatFile$N_CatchDF) {
    eval(parse(text = paste0(
      "DatOut$",
      tmp,
      "$",
      names(DatFile$Catch)[c],
      " <- get.vec(dat, Loc)"
    )))
  }
  # log_q_catch
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # obs_catch_out
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (c in 1:DatFile$N_CatchDF) {
    eval(parse(text = paste0(
      "DatOut$",
      tmp,
      "$",
      names(DatFile$Catch)[c],
      " <- get.vec(dat, Loc)"
    )))
  }
  # obs_catch_effort
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (c in 1:DatFile$N_CatchDF) {
    eval(parse(text = paste0(
      "DatOut$",
      tmp,
      "$",
      names(DatFile$Catch)[c],
      " <- get.vec(dat, Loc)"
    )))
  }
  # pre_catch_out
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (c in 1:DatFile$N_CatchDF) {
    eval(parse(text = paste0(
      "DatOut$",
      tmp,
      "$",
      names(DatFile$Catch)[c],
      " <- get.vec(dat, Loc)"
    )))
  }
  # res_catch_out
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (c in 1:DatFile$N_CatchDF) {
    eval(parse(text = paste0(
      "DatOut$",
      tmp,
      "$",
      names(DatFile$Catch)[c],
      " <- get.vec(dat, Loc)"
    )))
  }
  # dCatchData_out
  DatOut <-
    do.df(dat,
          DatOut,
          Loc,
          LocDatOut,
          nrow = length(Start_Y:End_Y) * DatFile$N_CatchDF)

  if (verbose)
    cat("\t-> Read catch data and derivates \n")
  # -------------------------------------------------------------------------


  # Survey data ----
  if (verbose)
    cat("-- Reading survey data and derivates \n")
  # dSurveyData
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = DatFile$Nrows_SvDF)
  # cpue_cv_add
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # obs_cpue
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # pre_cpue
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # res_cpue
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # survey_q
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # sdnr_MAR_cpue
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = DatFile$N_SurveyDF)

  if (verbose)
    cat("\t-> Read survey data and derivates \n")
  # -------------------------------------------------------------------------

  # Size composition data ----
  if (verbose)
    cat("-- Reading size composition \n")

  # d3_SizeComps_in
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (s in 1:DatFile$N_SizeFreq_df) {
    eval(parse(
      text = paste0(
        "DatOut$",
        tmp,
        "$",
        names(DatFile$SizeFreq)[s],
        " <- get.df(dat, Loc, nrow = ",
        DatFile$Nrows_SiseFreqDF[s],
        ")"
      )
    ))
  }

  # d3_obs_size_comps_out
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (s in 1:DatFile$N_SizeFreq_df) {
    eval(parse(
      text = paste0(
        "DatOut$",
        tmp,
        "$",
        names(DatFile$SizeFreq)[s],
        " <- get.df(dat, Loc, nrow = ",
        DatFile$Nrows_SiseFreqDF[s],
        ")"
      )
    ))
  }


  # d3_pre_size_comps_out
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (s in 1:DatFile$N_SizeFreq_df) {
    eval(parse(
      text = paste0(
        "DatOut$",
        tmp,
        "$",
        names(DatFile$SizeFreq)[s],
        " <- get.df(dat, Loc, nrow = ",
        DatFile$Nrows_SiseFreqDF[s],
        ")"
      )
    ))
  }

  # d3_res_size_comps_out
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (s in 1:DatFile$N_SizeFreq_df) {
    eval(parse(
      text = paste0(
        "DatOut$",
        tmp,
        "$",
        names(DatFile$SizeFreq)[s],
        " <- get.df(dat, Loc, nrow = ",
        DatFile$Nrows_SiseFreqDF[s],
        ")"
      )
    ))
  }
  # d3_obs_size_comps_X
  oldk <- 0
  for (n in 1:DatFile$N_SizeFreq_df) {
    k <- CtlFile$iCompAggregator[n]
    if (oldk != k) {
      tmp <- get.namVar(dat, Loc, LocDatOut)
      eval(parse(
        text = paste0(
          "DatOut$",
          tmp,
          " <- get.df(dat, Loc, nrow = ",
          DatFile$Nrows_SiseFreqDF[n],
          ")"
        )
      ))
    }
    oldk <- k
  }

  # d3_pre_size_comps_X
  oldk <- 0
  for (n in 1:DatFile$N_SizeFreq_df) {
    k <- CtlFile$iCompAggregator[n]
    if (oldk != k) {
      tmp <- get.namVar(dat, Loc, LocDatOut)
      eval(parse(
        text = paste0(
          "DatOut$",
          tmp,
          " <- get.df(dat, Loc, nrow = ",
          DatFile$Nrows_SiseFreqDF[n],
          ")"
        )
      ))
    }
    oldk <- k
  }

  # d3_res_size_comps_X
  oldk <- 0
  for (n in 1:DatFile$N_SizeFreq_df) {
    k <- CtlFile$iCompAggregator[n]
    if (oldk != k) {
      tmp <- get.namVar(dat, Loc, LocDatOut)
      eval(parse(
        text = paste0(
          "DatOut$",
          tmp,
          " <- get.df(dat, Loc, nrow = ",
          DatFile$Nrows_SiseFreqDF[n],
          ")"
        )
      ))
    }
    oldk <- k
  }

  # size_comp_sample_size
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))

  oldk <- 0
  for (n in 1:DatFile$N_SizeFreq_df) {
    k <- CtlFile$iCompAggregator[n]
    if (oldk != k) {
      eval(parse(
        text = paste0("DatOut$", tmp, "$Size_Comp_", k,
                      " <- get.vec(dat, Loc)")
      ))
    }
    oldk <- k
  }

  # sdnr_MAR_lf
  # Size data: standard deviation and median
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(unique(CtlFile$iCompAggregator)))

  if (verbose)
    cat("\t-> Read size composition \n")
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
  # slx_control_in
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = CtlFile$nslx_pars)
  # slx_control
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(CtlFile$slx_npar))

  if (verbose)
    cat("\t-> Read selectivity \n")
  # -------------------------------------------------------------------------

  # Natural, Fishing and Total mortality ----
  if (verbose)
    cat("-- Reading natural, fishing and total mortality \n")
  # m_prop
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro))
  # M
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) * nsex)
  nRowM <- length(Start_Y:nyrRetro)
  nam_sexes <- c("Males", "Females")
  nam_mat <- base::switch(.ac(DatFile$N_maturity),
                          "1" = "Both",
                          "2" = c("Mature","Immature"))

  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))

  for(s in 1:nsex){
    for(m in 1:DatFile$N_maturity){
      eval(parse(text = paste0(
        "DatOut$", tmp, "$",nam_sexes[s],"$",nam_mat[m]," <- get.df(dat, Loc, nrow = nRowM)"
      )))
    }
  }

  # xi
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # log_fbar
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # ft
  nRowft <- N_fleet * nsex * length(Start_Y:nyrRetro)
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowft)
  # F
  nRowF <- nsex * length(Start_Y:nyrRetro) * DatFile$N_seasons
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowF)
  # selectivity
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  nam_year <- c("Start_Y", "End_Y")
  for (y in 1:2) {
    eval(parse(
      text = paste0(
        "DatOut$",
        tmp,
        "$",
        nam_year[y],
        " <- get.df(dat, Loc, nrow = nsex*N_fleet)"
      )
    ))
  }
  # Retained
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  nam_year <- c("Start_Y", "End_Y")
  for (y in 1:2) {
    eval(parse(text = paste0(
      "DatOut$", tmp, "$", nam_year[y],
      " <- get.vec(dat, Loc)"
    )))
  }

  if (verbose)
    cat("\t-> Read Natural mortality, fishing and total mortality \n")
  # -------------------------------------------------------------------------


  # Recuitment ----
  if (verbose)
    cat("-- Reading Recuitment \n")

  # rec_sdd
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nsex)
  # rec_ini
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # rec_dev
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # logit_rec_prop
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # recruits
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nsex)
  # res_recruit
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  if (verbose)
    cat("\t-> Read Recruitment \n")
  # -------------------------------------------------------------------------


  # SSB and N----
  if (verbose)
    cat("-- Reading SSB and N \n")
  # ssb
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # dyn_Bzero
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # N_initial
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = DatOut$n_grp)
  # N_total
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_males
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_females
  # if(nsex>1)
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_males_new
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_females_new
  # if(nsex>1)
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_males_old
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_females_old
  # if(nsex>1)
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_males_mature
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_females_mature
  # if(nsex>1)
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)

  if (verbose)
    cat("\t-> Read SSB and N \n")
  # -------------------------------------------------------------------------


  # Molting and growth ----
  if (verbose)
    cat("-- Reading molting and growth \n")

  # molt_increment
  nRowMoltInc <- max(CtlFile$nSizeIncVaries) * nsex
  DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = nRowMoltInc)
  # dPreMoltSize
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # iMoltIncSex
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # dMoltInc
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  # dPreMoltSize
  if (DatFile$NGrowthObs > 0) {
    DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  } else {
    Loc <- Loc + 2
    LocDatOut <- LocDatOut + 2
  }
  # iMoltIncSex
  if (DatFile$NGrowthObs > 0) {
    DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  } else {
    Loc <- Loc + 2
    LocDatOut <- LocDatOut + 2
  }
  # dMoltInc
  if (DatFile$NGrowthObs > 0) {
    DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  } else {
    Loc <- Loc + 2
    LocDatOut <- LocDatOut + 2
  }

  # molt_probability
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro)*nsex)
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  nam_sex <- c("Males", "Females")
  for (s in 1:nsex) {
    eval(parse(
      text = paste0(
        "DatOut$",
        tmp,
        "$",
        nam_sex[s],
        " <- get.df(dat, Loc, nrow = length(Start_Y:nyrRetro))"
      )
    ))
  }

  # Growth matrix
  tmp <- get.namVar(dat, Loc, LocDatOut)
  tmp <- unlist(stringr::str_split(string = tmp, pattern = "_"))
  tmp <- paste0(tmp[1], "_", tmp[2])

  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  nam_sex <- c("Males", "Females")

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
      tmpbis <- get.namVar(dat, Loc, LocDatOut)
    }
  }

  # Reset Loc and LocDatOut because of the loop on the growth matrix
  Loc <- Loc - 1
  LocDatOut <- LocDatOut - 1

  if (verbose)
    cat("\t-> Read molting and growth \n")
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
  # ssbF0
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_bmsy
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_depl
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_fofl
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # sd_fmsy
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # sd_fofl
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # spr_cofl
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_cofl_ret
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_yield
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = 101) #Number of rows defines in the .tpl
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

  # End of data file
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("====================================================\n")
    cat("Read of gmacs.rep file complete.")
    cat("\n")
  }

  return(DatOut)
}
