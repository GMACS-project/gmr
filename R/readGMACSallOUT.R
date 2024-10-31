#' @title Read GmacsAll.out
#'
#' @description This functions reads the gmacsall.out file as a named list
#' within R.
#
#' @param FileName (character string)- path and name (by default, gmacsAll.out) of the file to read.
#' @param verbose (logical)- (TRUE/FALSE); flag to print processing information
#' @param DatFile (list)- Object containing the .dat file - This is the output
#' of the [readGMACSdat()] function.
#' @param CtlFile (list)- Object containing the .ctl file - This is the output
#' of the [readGMACSctl()] function.
#' @param GmacsFile (list)- Object containing the gmacs.dat file - This is the output
#' of the [readGMACS.dat()] function.
#' @param nyrRetro (integer)- Number of year for the retrospective analysis
#'
#' @return the gmacsall.out file as a named list.
#'
#' \itemize{
#'   \item \code{sourcefile} - The file source.
#'   \item \code{Comments} - Specifications about the GMACS version used and the stock
#'   assessed.
#'   \item \code{Stock_info} - A named list with information relative to the stock assessed.
#'   Objects are the following:
#'   \itemize{
#'      \item \code{Stock} - The name of the stock assessed.
#'      \item \code{Year_range} - The start and end year of the assessment.
#'      \item \code{Number_of_seasons} - The number of season.
#'      \item \code{Number_of_fleets} - The number of fleet.
#'      \item \code{Fleets} - The names of the fleets (fishing and surveys).
#'      \item \code{Number_of_sexes} - The number of sex.
#'      \item \code{Number_of_shell_conditions} - The number of shell conditions.
#'      \item \code{Number_of_maturity_states} - The number of maturity states.
#'      \item \code{Weight_unit_is} - The unit for weight.
#'      \item \code{Numbers_unit_is} - The unit for numbers.
#'   }
#'   \item \code{Lik_type} - The likelihood for each data component (raw and weighted).
#'   \item \code{Penalties} - The total penalties.
#'   \item \code{Priors} - The likelihood for priors.
#'   \item \code{Initial_size_structure} - The likelihood for the initial size
#'   composition.
#'   \item \code{Total} - The total likelihood.
#'   \item \code{Lik_type_fleet} - The likelihood (raw, emphasis, net) for each data source
#'   including recruitment (deviations and sex-ratio).
#'   \item \code{Penalties_type} - The Penalties and emphasis for each "priors".
#'   \item \code{Maximum_gradient} - The maximum gradient.
#'   \item \code{Param} - A named list with the estimated parameters. Objects are the following:
#'    \itemize{
#'      \item \code{theta} - The key parameter controls (core parameters - theta parameters).
#'      \item \code{Grwth} - The growth parameters.
#'      \item \code{Vul} - The vulnerability (selectivity and retention) parameters.
#'      \item \code{Envpar_Slx} - The environmental-linked selectivity parameters.
#'      \item \code{Slx_Devs} - The selectivity deviations.
#'      \item \code{Fbar} - The mean fishing mortality parameters.
#'      \item \code{Fdev} - The fishing fleet-specific weights for male.
#'      \item \code{Foff} - The female fishing mortality offset to male F.
#'      \item \code{Fdov} - The fishing fleet-specific weights for female.
#'      \item \code{rec_ini} - The initial recruitment by size-class.
#'      \item \code{rec_dev_est} - The year-specific recruitment deviations.
#'      \item \code{logit_rec_prop_est} - The year-specific sex-ratio recruitment.
#'      \item \code{Mdev} - The natural mortality deviation.
#'      \item \code{EffSamp_size} - The effective sample size.
#'      \item \code{survey_Q} - The survey-specific catchability.
#'      \item \code{log_add_cvt} - The survey-specific additional CV.
#'    }
#'   \item \code{Management_Quantities} - The estimated management quantities
#'   (SPR; MSY, OFL, ...) and recruitment.
#'   \item \code{Overall_Summ} - Overall summary per year (SSB, SSA, Dynamic B0,
#'   Recruitment, mortality, ...).
#'   \item \code{mean_weight} - The sex-specific mean weight per size-class for
#'   each year and the mid-point used in the model.
#'   \item \code{maturity} - The sex-specific proportion of mature per size-class.
#'   \item \code{dCatchData} - The catch data with predicted values.
#'   \item \code{log_q_catch} - The estimated catchability (log space).
#'   \item \code{dSurveyData} - The survey data with predicted values.
#'   \item \code{sdnr_MAR_cpue} - The survey-specific standard devaiation and median.
#'   \item \code{Size_fit_summary} - The summary of size-specific fit for each fleet.
#'   \item \code{sdnr_MAR_lf} - The standard deviation and median for each size composition data.
#'   \item \code{Francis_weights} - The francis weights.
#'   \item \code{Selectivity} - The size-specific selectivity for the capture, retained and discards.
#'   \item \code{Select_control} - The selectivity controls.
#'   \item \code{m_prop} - The proportion of natural mortality per season and year.
#'   \item \code{M_size_class} - The sex- and mature-state specific natural mortality
#'   per size-class for each year.
#'   \item \code{Fully_selected_fishing_F_Fl_Yr_Sex} - The sex-specific fully-selected
#'    fishing mortality for each season and each year.
#'   \item \code{Fully_F} - The fully-selected fishing mortality for each fleet.
#'   \item \code{F_SizeC_Continuous} - The fully-selected fishing mortality by
#'   size class (continuous).
#'   \item \code{F_SizeC_Discrete} - The fully-selected fishing mortality by
#'   size class (discrete).
#'   \item \code{TotMorta_SizeC_Continuous} - The total mortality by size class
#'    (continuous).
#'   \item \code{TotMorta_SizeC_Discrete} - The total mortality by size class (discrete).
#'   \item \code{N_at_size} - The matrix of number-at-size.
#'   \item \code{molt_probability} - The sex-specific probability of molting
#'   for each year by size class.
#'   \item \code{Growth_transition_Matrix} - The sex-specific growth transition matrix.
#'   \item \code{Size_transition_Matrix} - The sex-specific size transition matrix.
#'   \item \code{fhitfut} - A season-specific matrix for F by fleet.
#'   \item \code{spr_syr} - The first year for computing Rbar.
#'   \item \code{spr_nyr} - The last year for computing Rbar.
#'   \item \code{spr_rbar} - The mean recruitment for SPR calculation.
#'   \item \code{proj_rbar} - The mean recruitment for the projections.
#'   \item \code{spr_sexr} - The sex-ratio for SPR calculation.
#'   \item \code{SR_alpha_prj} - The alpha parameter of the stock recruitment
#'   relationship used in the projections.
#'   \item \code{SR_beta_prj} - The beta parameter of the stock recruitment
#'   relationship used in the projections.
#'   \item \code{spr_fofl} - The fishing mortality relative to MSY for the OFL.
#'   \item \code{spr_cofl_ret} - The retained portion of the OFL.
#'   \item \code{nloglike} - A list containing the negative log-likelihood for all
#'   data sources (catch, survey, size composition, growth data (tagging data),
#'   and recruitment)
#'   \item \code{nlogPenalty} - The log penalties.
#'   \item \code{priorDensity} - The values of the prior densities.
#' }
#'
#' @seealso \code{\link{readGMACSdat}}, \code{\link{readGMACSctl}}, \code{\link{readGMACSrep}}
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
#' # Read the Gmacsall.out file ----
#' GMACSalloutfile <-
#'   readGMACSallOUT(
#'     FileName = file.path(Dir_Dvpt_Vers, "build", stock, "Gmacsall.out", fsep = fsep),
#'     verbose = TRUE,
#'     DatFile = datFile,
#'     CtlFile = ctlFile,
#'     GmacsFile = GMACSdat,
#'     nyrRetro = GMACSdat$N_Year_Retro
#'   )
#' }
#'
#' @export
#' @md
#
readGMACSallOUT <- function(FileName = NULL,
                            verbose = TRUE,
                            DatFile = NULL,
                            CtlFile = NULL,
                            GmacsFile = NULL,
                            nyrRetro = NULL) {
  # 1- Internal functions ----

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
  do.df <-
    function(dat,
             DatOut,
             Loc,
             LocDatOut,
             nrow = NULL,
             varname = NULL) {
      if (is.null(varname)) {
        tmpnam <- get.namVar(dat, Loc, LocDatOut)
      } else {
        tmpnam <- varname
      }
      # if (tmpnam == "")
      #    <- get.namVar(dat, Loc, LocDatOut)
      eval(parse(
        text = paste0("DatOut[['", tmpnam, "']] <- get.df(dat, Loc, nrow = nrow)")
      ))

      # eval(parse(
      #   text = paste0("DatOut[['", varname, "']] <- get.df(dat, Loc, nrow = nrow)")
      # ))

      assign("Loc", Loc, parent.frame())
      assign("LocDatOut", LocDatOut, parent.frame())

      return(DatOut)
    }

  # @title getLine & DoLine
  #
  # @description Extract a line at a specific location and split it accorind
  #  the pattern ";"
  #
  # @param dat the object in which the value is searched
  # @param DatOut the list() created and containing the data
  # @param Loc the position in the dat object
  # @param LocDatOut in the DatOut object
  #
  # @return the data by either filling in the DatOut object or simply as an object
  #
  DoLine <- function(dat, DatOut, Loc, LocDatOut, fill = FALSE) {
    assign("Loc", Loc + 1, parent.frame())
    assign("LocDatOut", LocDatOut + 1, parent.frame())

    tmp <- getLine(dat, Loc, LocDatOut)
    if (fill) {
      eval(parse(
        text = paste("DatOut[[tmp[['nam_var']]]] <- tmp[['vals']]", sep = "")
      ))
      return(DatOut)
    } else {
      return(tmp)
    }
  }
  getLine <- function(dat, Loc, LocDatOut) {
    assign("Loc", Loc + 1, parent.frame())
    assign("LocDatOut", LocDatOut + 1, parent.frame())
    out <- list()

    tmp <- dat[Loc]
    tmp <- unlist(stringr::str_split(string = tmp, pattern = ":"))

    tmp1 <-
      unlist(stringr::str_split(string = tmp[1], pattern = " "))
    tmp1 <- tmp1[!tmp1 == ""]
    out$nam_var <- ifelse(
      test = length(tmp1 > 1),
      yes = paste0(tmp1, collapse = "_"),
      no = tmp1
    )
    tmp <- unlist(stringr::str_split(string = tmp[2], pattern = " "))
    tmp <- stringr::str_remove_all(string = tmp, pattern = "\t")
    tpm <- stringi::stri_remove_empty(tmp)
    suppressWarnings(out$vals <-
                       ifelse(is.na(.an(tmp[!tmp == ""])), yes = tmp[!tmp == ""], no = .an(tmp[!tmp ==
                                                                                                 ""])))
    return(out)
  }

  # @title DoMultLine
  #
  # @description Extract multiple lines when reading the likelihood components
  #
  # @param dat the object in which the value is searched
  # @param DatOut the list() created and containing the data
  # @param Loc the position in the dat object
  # @param LocDatOut in the DatOut object
  # @param row_nam row names in the data frame that is created
  # @param var_DatOut name of the variable in the DatOut object that will hold the
  # dataframe
  #
  # @return the data frame
  #
  DoMultLine <- function(dat,
                         DatOut,
                         Loc,
                         LocDatOut,
                         row_nam = NULL,
                         var_DatOut = NULL) {
    tmp <- get.namVar(dat, Loc, LocDatOut)
    tmp <-
      stringr::str_replace_all(string = tmp,
                               pattern = "-",
                               replacement = "_")
    tmp <-
      stringr::str_replace_all(string = tmp,
                               pattern = " ",
                               replacement = "_")

    line_out <- NULL

    if (grepl("Recruitment", tmp, fixed = TRUE) ||
        grepl("Growth", tmp, fixed = TRUE)) {
      nbLine <- 1
      tmp_line <- DoLine(dat, DatOut, Loc, LocDatOut, fill = FALSE)
      eval(parse(
        text =
          paste(
            "line_out <- data.frame(row.names = row_nam,",
            tmp_line$nam_var,
            "= tmp_line$vals)",
            sep = ""
          )
      ))
    } else {
      nbLine <- 3
      for (l in 1:nbLine) {
        eval(parse(
          text = paste("tmp_line_", l, "<-getLine(dat, Loc, LocDatOut)", sep = "")
        ))
      }
      eval(parse(
        text =
          paste(
            "line_out <- data.frame(row.names = row_nam,",
            tmp_line_1$nam_var,
            "= tmp_line_1$vals,",
            tmp_line_2$nam_var,
            "= tmp_line_2$vals,",
            tmp_line_3$nam_var,
            "= tmp_line_3$vals)",
            sep = ""
          )
      ))
    }
    eval(parse(text = paste0(
      "DatOut$", var_DatOut, "$", tmp, " <- line_out"
    )))
    assign("DatOut", DatOut, parent.frame())

    assign("Loc", Loc, parent.frame())
    assign("LocDatOut", LocDatOut, parent.frame())
  }

  # @title fill_para
  #
  # @description Extract multiple lines when reading the tetha parameters
  #
  # @param dat the object in which the value is searched
  # @param Loc the position in the dat object
  # @param nrow the number of lines in returned the data frame
  #
  # @return the data frame
  #
  fill_para <-
    function(dat,
             Loc,
             nrow,
             n_split = 2,
             par_nam = TRUE) {
      df <- dat[Loc:(Loc + nrow - 1)]
      assign("Loc", Loc + nrow, parent.frame())

      param <-
        stringr::str_split_fixed(string = df,
                                 pattern = ":",
                                 n = n_split)[, n_split]
      param <-
        stringr::str_split_fixed(string = param,
                                 pattern = " ",
                                 n = 2)[, 2]
      param <- stringr::str_squish(param)
      param <- stringr::str_split(string = param, pattern = " ")

      if (par_nam) {
        nam_param <-
          stringr::str_split_fixed(string = df,
                                   pattern = ":",
                                   n = n_split)[, 1]
        nam_param <-
          stringr::str_split_fixed(string = nam_param,
                                   pattern = " ",
                                   n = n_split)[, 2]
        if (any(stringr::str_detect(string = nam_param, pattern = "([()])"))) {
          nam_param <-
            gsub(pattern = "([(])",
                 replacement = "_",
                 x = nam_param)

          nam_param <-
            stringr::str_remove_all(string = nam_param, " |([)])")
        } else{
          nam_param <- stringr::str_squish(nam_param)
        }
      }
      outbound <- NULL
      outbound <-
        suppressWarnings(which(stringr::str_detect(
          string = param, pattern = "[*]"
        )))
      if (length(outbound) > 0) {
        for (o in 1:length(outbound)) {
          param[[outbound[o]]] <-
            c(param[[outbound[o]]][1:4], param[[outbound[o]]][6:8])
        }
      }

      for (i in 1:length(param)) {
        if (length(param[[i]]) != 8)
          param[[i]] <-
            c(param[[i]], rep(NA, (8 - length(param[[i]]))))
      }

      param <- do.call("rbind", param) ## Make it into a dataframe
      param <- as.data.frame(param, stringsAsFactors = FALSE)
      param <- utils::type.convert(param, as.is = TRUE)
      if (par_nam)
        param <- cbind(nam_param, param)
      return(param)
    }
  # -------------------------------------------------------------------------

  # 2- Read Gmacsall.out file ----
  # Read the Gmacsall.out file and find the first line containing numeric data

  if (verbose) {
    cat("\n====================================================\n")
    cat("-> Reading GmacsAll.out \n")
    cat("====================================================")
    cat("\n")
  }

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

  # Read Report file
  dat <- readLines(FileName, warn = FALSE)

  # start <- which(stringr::str_detect(dat, "#Likelihoods_by_type") == T)[1]
  start <-
    which(stringr::str_detect(dat, "#Stock being assessed") == T)[1]

  Com <-
    grep(x = dat[seq_len(start - 1)],
         pattern = "^#",
         value = TRUE)
  Stock <-
    stringr::str_split_fixed(string = dat[start],
                             pattern = ":",
                             n = 2)[, 2]

  # -------------------------------------------------------------------------

  # 3- Prepare the data ----
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
  DatOut[["Comments"]] <- ifelse(test = length(Com) == 0,
                                 yes = NA,
                                 no = Com)
  # Initialize the location index
  Loc <- 1
  LocDatOut <- length(DatOut)
  # -------------------------------------------------------------------------

  # 4- Fill in DatOut ----

  if (verbose)
    cat("-- General information \n")
  # Year range|Number of season|Number of fleets|Fleets names|Number of sexes|
  # Number of shell condition|Number of maturity states|Weight unit|Number unit
  Stock_info <- NULL
  Stock_info[["Stock"]] <- stringr::str_squish(Stock)

  for (i in 1:9) {
    Stock_info <- DoLine(dat, Stock_info, Loc, LocDatOut, fill = TRUE)
  }
  suppressWarnings(Stock_info[["Year_range"]] <- .an(Stock_info[["Year_range"]])[!is.na(.an(Stock_info[["Year_range"]]))])
  DatOut[["Stock_info"]] <- Stock_info

  # Negative loglikelihoods / Penalities / Prior density ----
  if (verbose)
    cat("-- Reading likelihoods by type \n")
  # neg loglikelihoods
  nam_likes <-
    c("Catch_data",
      "Index_data",
      "Size_data",
      "Stock_Recruit",
      "Tagging_data")
  nlikes <- length(nam_likes)
  nam_type <- c("raw", "weighted")
  Lik_type <- NULL

  for (l in 1:nlikes) {
    tmp <- getLine(dat, Loc, LocDatOut)

    eval(parse(
      text = paste(
        "Lik_type <- rbind(Lik_type,data.frame(row.names = tmp$nam_var,",
        nam_type[1],
        " = tmp$vals[1],",
        nam_type[2],
        " = tmp$vals[2]))",
        sep = ""
      )
    ))
  }
  DatOut[["Lik_type"]] <- Lik_type
  # nlogPenalty
  DatOut <- DoLine(dat, DatOut, Loc, LocDatOut, fill = TRUE)
  # priors
  DatOut <- DoLine(dat, DatOut, Loc, LocDatOut, fill = TRUE)
  # Initial size structure
  DatOut <- DoLine(dat, DatOut, Loc, LocDatOut, fill = TRUE)
  # Total
  DatOut <- DoLine(dat, DatOut, Loc, LocDatOut, fill = TRUE)

  if (verbose)
    cat("\t-> Read Loglikelihoods / Penalties / Prior density \n")
  # -------------------------------------------------------------------------

  # Likelihoods by type and fleet ----
  if (verbose)
    cat("-- Reading likelihoods by type and fleet \n")

  var_DatOut <- "Lik_type_fleet"
  # Catches
  row_nam <- names(DatFile$Catch)
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Index
  row_nam <- unique(names(DatFile$Surveys))
  row_nam <-
    stringr::str_replace_na(string = row_nam, replacement = "")
  if (length((unique(row_nam))) == 1 && unique(row_nam) == "") {
    row_nam <- paste0("Cpue_", 1:DatFile$N_SurveyDF)
  } else {
    row_nam[which(row_nam == "")] <- paste0("Cpue_", which(row_nam == ""))
  }
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Size-Composition
  row_nam <- NULL
  oldk <- 0

  for (n in 1:DatFile$N_SizeFreq_df) {
    k <- CtlFile$iCompAggregator[n]
    if (oldk != k) {
      row_nam <- c(row_nam, paste0(
        names(DatFile$SizeFreq)[k],
        "_seas",
        unique(DatFile$SizeFreq[[n]]$seas)
      ))
    }
    oldk <- k
  }
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Recruitment penalties
  row_nam <- c("Post_First_YrDevs", "Init_Devs", "SexRatio_Devs")
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Tagging
  if (DatFile$N_sexes == 1) {
    row_nam <- "Both sex"
  } else {
    row_nam <- c("Male", "Female")
  }
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Growth Likelihood
  if (DatFile$N_sexes == 1) {
    row_nam <- "Both sex"
  } else {
    row_nam <- c("Male", "Female")
  }
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)
  if (verbose)
    cat("\t-> Read likelihoods by type and fleet \n")
  # -------------------------------------------------------------------------

  # Penalties ----
  if (verbose)
    cat("-- Reading penalties \n")

  nam_Penal <-
    c(
      "Mean_Fbar",
      "MeanF_dev",
      "M_devs",
      "Rec_ini",
      "Rec_dev",
      "Sex_ratio",
      "Molt_Prob",
      "Free_Selectivity",
      "Init_N_at_len",
      "Fdevs",
      "Fdovs",
      "SelDevs"
    )
  nPenal <- length(nam_Penal)
  pen_out <- NULL
  for (p in 1:nPenal) {
    tmp <- getLine(dat, Loc, LocDatOut)
    pen_out <- cbind(pen_out, tmp$vals)
  }
  pen_out <-
    data.frame(pen_out,
               row.names = c("logPenalty", "Penalty_emphasis", "Mult"))
  colnames(pen_out) <- nam_Penal
  DatOut[["Penalties_type"]] <- as.data.frame(t(pen_out))

  if (verbose)
    cat("\t-> Read penalties \n")
  # -------------------------------------------------------------------------

  # Maximum gradient ----
  if (verbose)
    cat("-- Reading Maximum gradient \n")

  DatOut[["Maximum_gradient"]] <-
    DoLine(dat, DatOut, Loc, LocDatOut)$vals

  if (verbose)
    cat("\t-> Read Maximum gradient \n")
  # -------------------------------------------------------------------------
  # Extract theta parameters
  # -------------------------------------------------------------------------

  nam_para <-
    c(
      "Parameter_count",
      "Parameter",
      "Estimate",
      "Phase",
      "Lower_Bd",
      "Upper_Bd",
      "Penalty",
      "Gradient",
      "Std_error",
      "Etimated_Param_count"
    )
  Param <- NULL

  if (verbose)
    cat("-- Reading theta parameters \n")

  theta <- fill_para(dat, Loc, nrow = CtlFile$ntheta, n_split = 2)
  theta <-
    as.data.frame(cbind(paste("theta", 1:CtlFile$ntheta, sep = "_"), theta))
  colnames(theta) <- nam_para
  Param[["theta"]] <- theta # theta parameters

  if (verbose)
    cat("\t-> Read theta parameters \n")
  # -------------------------------------------------------------------------

  # Extract growth parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading growth parameters \n")

  Grwth <-
    fill_para(
      dat,
      Loc,
      nrow = (CtlFile$nGrwth + CtlFile$nSizeIncPar),
      n_split = 2
    )
  Grwth <-
    as.data.frame(cbind(paste(
      "Grwth", 1:(CtlFile$nGrwth + CtlFile$nSizeIncPar), sep = "_"
    ), Grwth))
  colnames(Grwth) <- nam_para
  Param[["Grwth"]] <- Grwth # growth parameters

  if (verbose)
    cat("\t-> Read growth parameters \n")
  # -------------------------------------------------------------------------

  # Extract vulnerability parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading vulnerability parameters \n")

  Vul <- fill_para(dat, Loc, nrow = CtlFile$nslx_pars, n_split = 2)
  Vul <-
    as.data.frame(cbind(paste(
      "log_slx_pars", 1:CtlFile$nslx_pars, sep = "_"
    ), Vul))
  colnames(Vul) <- nam_para
  Param[["Vul"]] <- Vul # vulnerability parameters

  if (verbose)
    cat("\t-> Read vulnerability parameters \n")
  # -------------------------------------------------------------------------


  # Extract the asymptotic retention parameters
  # -------------------------------------------------------------------------
  if (CtlFile$NumAsympRet > 0) {
    if (verbose)
      cat("-- Reading the number of asymptotic retention parameters \n")

    Asympt <- fill_para(dat, Loc, nrow = CtlFile$NumAsympRet)
    Asympt <-
      as.data.frame(cbind(paste(
        "Asymret", 1:CtlFile$NumAsympRet, sep = "_"
      ), Asympt))
    colnames(Asympt) <- nam_para
    Param[["Asympt"]] <- Asympt # asymptotic retention parameters

    if (verbose)
      cat("\t-> Read the asymptotic retention parameters \n")
  }
  # -------------------------------------------------------------------------

  # Extract the time-varying parameters for the vulnerability
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading environmental parameters for vulnerability  \n")
  if (CtlFile$nslx_envpars > 0) {
    Envpar_Slx <- fill_para(dat, Loc, nrow = CtlFile$nslx_envpars)
    Envpar_Slx <-
      as.data.frame(cbind(
        paste("Envpar_Slx", 1:CtlFile$nslx_envpars, sep = "_"),
        Envpar_Slx
      ))
  } else {
    Envpar_Slx <- fill_para(dat, Loc, nrow = 1)
    Envpar_Slx <-
      as.data.frame(cbind(paste("Envpar_Slx", 0, sep = "_"), Envpar_Slx))
  }
  colnames(Envpar_Slx) <- nam_para
  Param[["Envpar_Slx"]] <-
    Envpar_Slx # Estimated environmental parameters

  if (verbose)
    cat("\t-> Read the environmental parameters for vulnerability \n")
  # -------------------------------------------------------------------------

  # Extract vulnerability deviations
  # -------------------------------------------------------------------------
  # These deviations include the environmental impacts if applicable
  if (verbose)
    cat("-- Reading vulnerabity deviations  \n")

  if (CtlFile$NSlx_devs_param > 0) {
    Slx_Devs <- fill_para(dat, Loc, nrow = CtlFile$NSlx_devs_param)
    Slx_Devs <-
      as.data.frame(cbind(
        paste("Slx_Devs", 1:CtlFile$NSlx_devs_param, sep = "_"),
        Slx_Devs
      ))
  } else {
    Slx_Devs <- fill_para(dat, Loc, nrow = 1)
    Slx_Devs <-
      as.data.frame(cbind(paste("Slx_Devs", 0, sep = "_"), Slx_Devs))
  }
  colnames(Slx_Devs) <- nam_para
  Param[["Slx_Devs"]] <- Slx_Devs # Estimated selectivty deviations

  if (verbose)
    cat("\t-> Read the deviations for vulnerability \n")
  # -------------------------------------------------------------------------

  # Extract the mean fishing mortality rate parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the mean fishing mortality rate parameters \n")

  Fbar <- fill_para(dat, Loc, nrow = N_fleet)
  Fbar <-
    as.data.frame(cbind(paste("log_fbar", 1:N_fleet, sep = "_"), Fbar))
  colnames(Fbar) <- nam_para
  Param[["Fbar"]] <- Fbar # mean F parameters

  if (verbose)
    cat("\t-> Read the mean fishing mortality rate parameters \n")
  # -------------------------------------------------------------------------

  # Extract the male mean fishing mortality rate deviations
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the male mean fishing mortality rate deviations \n")

  # Find the number of parameter for each fleet
  findnPara <- NULL
  for (i in seq(DatFile$Catch))
    assign("findnPara", rbind(findnPara, DatFile$Catch[[i]]))
  findnPara <- unique(findnPara[, c("year", "seas", "fleet")])
  findnPara$nam <-
    paste("_Yr",
          findnPara$year,
          "_Seas",
          findnPara$seas,
          "_Fl",
          findnPara$fleet,
          sep = "")

  nFparams <- findnPara %>%
    dplyr::group_by(fleet) %>%
    dplyr::summarise(n = dplyr::n())
  # Extract data
  Fdev <- list()
  for (f in 1:N_fleet) {
    nrow_F <- ifelse(test = f %in% nFparams$fleet,
                     yes = nFparams[which(nFparams$fleet %in% f), "n", drop = TRUE],
                     no = 0)

    if (nrow_F != 0) {
      Fdev[[f]] <- fill_para(dat, Loc, nrow = nrow_F)
      ID <- findnPara[findnPara$fleet == f, "nam"]
      Fdev[[f]] <-
        as.data.frame(cbind(paste("log_Fdev", ID, sep = ""), Fdev[[f]]))
      colnames(Fdev[[f]]) <- nam_para
    }
  }
  # Fdev <- vctrs::list_drop_empty(Fdev)
  # names(Fdev) <- paste0("log_Fdev_Fl_",1:length(nFparams$fleet))
  names(Fdev) <- paste0("log_Fdev_", DatFile$F_Fleet_names)
  Param[["Fdev"]] <-
    Fdev # deviations of the male mean F parameters

  if (verbose)
    cat("\t-> Read the male mean fishing mortality rate deviations \n")
  # -------------------------------------------------------------------------

  # Extract the female F offset to male fishing mortality parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the female F offset to male fishing mortality parameters \n")

  Foff <- fill_para(dat, Loc, nrow = N_fleet)
  Foff <-
    as.data.frame(cbind(paste("log_foff_", 1:N_fleet, sep = ""), Foff))
  colnames(Foff) <- nam_para
  Param[["Foff"]] <- Foff # female F offset parameters

  if (verbose)
    cat("\t-> Read the female F offset to male fishing mortality parameters \n")
  # -------------------------------------------------------------------------

  # Extract the female F deviation offset parameters
  # -------------------------------------------------------------------------
  if (any(CtlFile$Penalty_fdevs$Fdov_total) > 0) {
    if (verbose)
      cat("-- Reading the female F deviation offset parameters \n")

    Fdov <- list()

    # Find the number of parameter for each fleet
    findnPara <- NULL
    for (i in seq(DatFile$Catch))
      assign("findnPara", rbind(findnPara, DatFile$Catch[[i]]))
    findnPara <-
      unique(findnPara[, c("year", "seas", "fleet", "sex")])

    if (nsex == 1) {
      nYparams <- rep(1, N_fleet)
      for (f in 1:N_fleet) {
        Fdov[[f]] <- fill_para(dat, Loc, nrow = nYparams[f])
        Fdov[[f]] <-
          as.data.frame(cbind(paste("log_Fdov_", f, sep = ""), Fdov[[f]]))
        colnames(Fdov[[f]]) <- nam_para
      }
    } else {
      findnPara <- findnPara[findnPara$sex == 2, ]
      findnPara$nam <-
        paste("_Yr",
              findnPara$year,
              "_Seas",
              findnPara$seas,
              "_Fl",
              findnPara$fleet,
              sep = "")
      nYparams <- findnPara %>%
        dplyr::group_by(fleet) %>%
        dplyr::summarise(n = dplyr::n())

      # Extract the data

      for (f in 1:N_fleet) {
        nrow_F <- ifelse(
          test = f %in% nYparams$fleet,
          yes = nYparams[which(nYparams$fleet %in% f), "n", drop = TRUE],
          no = 0
        )
        if (nrow_F != 0) {
          Fdov[[f]] <- fill_para(dat, Loc, nrow = nrow_F)
          ID <- findnPara[findnPara$fleet == f, "nam"]
          Fdov[[f]] <-
            as.data.frame(cbind(paste("log_Fdov", ID, sep = ""), Fdov[[f]]))
          colnames(Fdov[[f]]) <- nam_para
        }
      }
    }
    Fdov <- vctrs::list_drop_empty(Fdov)
    names(Fdov) <- paste0("log_Fdov_Fl_", nYparams$fleet)
    Param[["Fdov"]] <- Fdov # female F deviation offset

    if (verbose)
      cat("\t-> Read the female F deviation offset parameters \n")
  }
  # -------------------------------------------------------------------------

  # Extract the initial values of recruitment
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the initial values for recruitment \n")

  rec_ini <-
    fill_para(dat, Loc, nrow = DatFile$N_sizeC, n_split = 2) # Initial values for recruitment

  rec_ini <-
    as.data.frame(cbind(paste("SizeC_", 1:DatFile$N_sizeC, sep = ""), rec_ini))
  colnames(rec_ini) <- nam_para
  Param[["rec_ini"]] <- rec_ini # female F offset parameters

  if (verbose)
    cat("\t-> Read the initial values for recruitment \n")
  # -------------------------------------------------------------------------

  # Extract the recruitment deviation estimates
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the recruitment deviation estimates \n")

  rec_dev_est <- fill_para(dat, Loc, nrow = DatFile$N_year)
  rec_dev_est <-
    as.data.frame(cbind(
      paste("RecDev_est_Yr", DatFile$Start_Y:DatFile$End_Y, sep = "_"),
      rec_dev_est
    ))
  colnames(rec_dev_est) <- nam_para
  Param[["rec_dev_est"]] <- rec_dev_est # recruitment deviation

  if (verbose)
    cat("\t-> Read the recruitment deviation estimates \n")
  # -------------------------------------------------------------------------

  # Extract the sex-ratio recruitment deviation estimates
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the sex-ratio recruitment deviation estimates \n")

  logit_rec_prop_est <- fill_para(dat, Loc, nrow = DatFile$N_year)
  logit_rec_prop_est <-
    as.data.frame(cbind(
      paste(
        "Logit_RecProp_est_Yr",
        DatFile$Start_Y:DatFile$End_Y,
        sep = "_"
      ),
      logit_rec_prop_est
    ))
  colnames(logit_rec_prop_est) <- nam_para
  Param[["logit_rec_prop_est"]] <-
    logit_rec_prop_est # sex-ratio recruitment deviation

  if (verbose)
    cat("\t-> Read the sex-ratio recruitment deviation estimates \n")
  # -------------------------------------------------------------------------

  # Extract the natural mortality deviation parameters
  # -------------------------------------------------------------------------
  if (CtlFile$nMdev > 0) {
    if (verbose)
      cat("-- Reading the natural mortality deviation parameters \n")

    Mdev <- fill_para(dat, Loc, nrow = CtlFile$nMdev)
    Mdev <-
      as.data.frame(cbind(paste("M_dev_est_", 1:CtlFile$nMdev, sep = ""), Mdev))
    colnames(Mdev) <- nam_para
    Param[["Mdev"]] <- Mdev # Natural mortality deviations

    if (verbose)
      cat("\t-> Read the natural mortality deviation parameters \n")
  }
  # -------------------------------------------------------------------------

  # Extract the effective sample size parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the effective sample size parameters \n")

  nVn <- max(CtlFile$iCompAggregator)
  EffSamp_size <- fill_para(dat, Loc, nrow = nVn)
  EffSamp_size <-
    as.data.frame(cbind(paste("log_vn_", 1:nVn, sep = ""), EffSamp_size))
  colnames(EffSamp_size) <- nam_para
  Param[["EffSamp_size"]] <-
    EffSamp_size # effective sample size parameters

  if (verbose)
    cat("\t-> Read the effective sample size parameters \n")
  # -------------------------------------------------------------------------

  # Extract the catchability coefficient parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the catchability coefficient parameters \n")

  survey_Q <- fill_para(dat, Loc, nrow = DatFile$N_SurveyDF)
  survey_Q <-
    as.data.frame(cbind(paste("survey_q", 1:DatFile$N_SurveyDF, sep = ""), survey_Q))
  colnames(survey_Q) <- nam_para
  Param[["survey_Q"]] <-
    survey_Q # catchability coefficient parameters

  if (verbose)
    cat("\t-> Read the catchability coefficient parameters \n")
  # -------------------------------------------------------------------------

  # Extract the addtional CV for surveys/indices parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the addtional CV for surveys/indices parameters \n")

  add_cv <- fill_para(dat, Loc, nrow = DatFile$N_SurveyDF)
  add_cv <-
    as.data.frame(cbind(paste(
      "log_add_cv_", 1:DatFile$N_SurveyDF, sep = ""
    ), add_cv))
  colnames(add_cv) <- nam_para
  Param[["log_add_cvt"]] <- add_cv # addtional CV parameters

  if (verbose)
    cat("\t-> Read the addtional CV for surveys/indices parameters \n")
  # -------------------------------------------------------------------------

  # Fill in DatOut with the parameters
  DatOut[["Param"]] <- Param

  # Management quantities
  # -------------------------------------------------------------------------

  if (verbose)
    cat("-- Reading Management (derived) quantities \n")

  nam_para <-
    c("Parameter",
      "Estimate",
      "Standard_error",
      "Estimated_quantity_count")
  nam <- NULL
  DQ <- NULL
  out_recr <- NULL

  if (GmacsFile$OutVar_BRPs == 1)
    nam <-
    c(
      "Male_Spr_Rbar",
      "Female_Spr_Rbar",
      "SSB_R_ratio_F0",
      "BMSY",
      "Bcurr_BMSY_ratio",
      "OFL_tot",
      paste0("Fmsy_Fl", 1:N_fleet),
      paste0("FOFL_Fl", 1:N_fleet),
      paste0("OFL_Fl", 1:N_fleet)
    )

  # if(GmacsFile$OutVar_Recr == 1)
  #   nam <- c(nam, paste0(rep(paste0("Log_rec_", nam_sex[1:nsex]), each=DatFile$N_year), "_", rep(Start_Y:End_Y, times=nsex)))
  #
  # if(GmacsFile$OutVar_SSB == 1)
  #   nam <- c(nam, paste0("Log_ssb_", Start_Y:End_Y))
  #
  # if(GmacsFile$OutVar_Fbar == 1)
  #   nam <- c(nam, paste0("Mean_F_", Start_Y:End_Y))
  #
  # if(GmacsFile$OutVar_DynB0 == 1)
  #   nam <- c(nam, paste0("Log_DynB0_", Start_Y:End_Y))
  # Loc<- 604

  if (!is.null(nam)) {
    nrow_DQ <- length(nam)
    DQ <-
      fill_para(dat,
                Loc,
                nrow = nrow_DQ,
                n_split = 2,
                par_nam = FALSE)
    DQ <- as.data.frame(cbind(nam, DQ[, 1:3]))
    colnames(DQ) <- nam_para
  }

  # Recruitment
  Recr <- dat[Loc]
  Loc <- Loc + 1
  LocDatOut <- LocDatOut + 1
  Recr <- unlist(stringr::str_split(string = Recr, pattern = ";"))
  for (n in 1:length(Recr)) {
    tmp <- unlist(stringr::str_split(string = Recr[n], pattern = ":"))
    tmp <- unlist(stringr::str_remove_all(string = tmp, pattern = " "))
    out_recr <- rbind(out_recr, rbind(tmp))

    if (n == length(Recr))
      rownames(out_recr) <- rep("", dim(out_recr)[1])
  }
  out_recr <-
    data.frame(Parameter = out_recr[, 1], Estimate = .an(out_recr[, 2]))

  if (is.null(DQ)) {
    MgtQ <- out_recr
  } else {
    MgtQ <- list("Derive_Quantity" = DQ, "Recruitment" = out_recr)
  }
  DatOut[["Management_Quantities"]] <- MgtQ

  if (verbose)
    cat("\t-> Read Management (derived) quantities \n")
  # -------------------------------------------------------------------------

  # Useful to name colnames and rownames
  .sex <- rep(nam_sex, each = (End_Y + 1 - Start_Y + 1))
  .year <- rep(Start_Y:(End_Y + 1), nsex)

  # # Model characteristics ----
  # if (verbose)
  #   cat("-- Reading model characteristics \n")
  #
  # # Fleet names
  # if(dat[Loc] == "fleetname"){
  #   DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = N_fleet)
  # }
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
  # -------------------------------------------------------------------------

  # Overall summary ----
  if (verbose)
    cat("-- Reading overall summary \n")
  nam_Summ <-
    c(
      "Year",
      "SSB",
      "log_SSB",
      "SD_log_SSB",
      "SSA",
      "Dynamic_B0",
      "log_DynB0",
      "SD_log_DynB0"
    )
  tmp <- NULL
  for (s in 1:nsex) {
    tmp <-
      c(tmp, paste0(
        c("Recruit_", "log_Recruits_", "SD_log_Recruits_"),
        nam_sex[s]
      ))
  }
  nam_Summ <- c(nam_Summ,
                tmp,
                "log_F",
                "SD_log_F",
                "Retained_mortality",
                "Total_mortality")
  nam_Summ <- c(nam_Summ, DatOut$Stock_info$Fleets)
  nam_Summ <- c(nam_Summ, "Rec_dev", "logit_rec_prop", "res_recruit")

  Overall_Summ <-
    get.df(dat = dat,
           Loc = Loc,
           nrow = length(Start_Y:End_Y))
  colnames(Overall_Summ) <- nam_Summ
  DatOut[["Overall_Summ"]] <- Overall_Summ

  if (verbose)
    cat("\t-> Read overall summary \n")

  # Mean weight at size ----
  if (verbose)
    cat("-- Reading mean weight at size \n")

  mean_weight <- NULL
  mean_weight$mid_points <-
    get.vec(dat = dat, Loc = Loc, num = TRUE)
  # mean_weight$values <- get.df(dat = dat,Loc = Loc, nrow = nsex * nmature * length(Start_Y:End_Y))
  mean_weight$mean_wt <-
    get.df(dat,
           Loc = Loc,
           nrow = nsex * nmature * length(Start_Y:End_Y))

  .new <- paste0("SizeC_", 1:nclass)
  colnames(mean_weight$mean_wt) <-
    c("Sex", "Maturity", "Year", .new)
  DatOut[["mean_weight"]] <- mean_weight

  if (verbose)
    cat("\t-> Read mean weight at size \n")
  # -------------------------------------------------------------------------

  # Maturity ----
  if (verbose)
    cat("-- Reading maturity \n")

  # DatOut <-
  #   base::switch(
  #     .ac(nsex),
  #     "1" = do.vec(dat, DatOut, Loc, LocDatOut),
  #     "2" = do.df(dat, DatOut, Loc, LocDatOut, nrow = nclass, varname = 'maturity')
  #   )
  # colnames(DatOut$maturity) <- .new
  # rownames(DatOut$maturity) <- nam_sex

  DatOut <-
    do.df(dat,
          DatOut,
          Loc,
          LocDatOut,
          nrow = nclass * nsex,
          varname = 'maturity')
  rownames(DatOut$maturity) <-
    paste0(rep(nam_sex, each = nclass), "_", .new)
  # if(nsex != 1){
  #   DatOut$maturity <- DatOut$maturity %>%
  #     dplyr::mutate(sex = nam_sex) %>%
  #     dplyr::select(sex, dplyr::everything()) %>%
  #     dplyr::rename_with(~ .new, dplyr::all_of(.old))
  # }

  if (verbose)
    cat("\t-> Read maturity \n")
  # -------------------------------------------------------------------------

  # Catch data ----
  if (verbose)
    cat("-- Reading catch data and derivates \n")

  # dCatchData
  DatOut <-
    do.df(
      dat,
      DatOut,
      Loc,
      LocDatOut,
      nrow = sum(DatFile$Nrows_CatchDF),
      varname = 'dCatchData'
    )
  namCatch <-
    c(
      "Series",
      "Year",
      'Fleet',
      "Season",
      "Sex",
      "Catch",
      "CV",
      "Partition",
      "Units",
      "Multiplier",
      "Effort",
      "Discard_mortality",
      "Predicted",
      "Residual"
    )
  colnames(DatOut$dCatchData) <- namCatch

  # log_q_catch
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  if (verbose)
    cat("\t-> Read catch data and derivates \n")
  # -------------------------------------------------------------------------

  # Survey data ----
  if (verbose)
    cat("-- Reading survey data and derivates \n")
  # dSurveyData
  DatOut <-
    do.df(dat,
          DatOut,
          Loc,
          LocDatOut,
          nrow = DatFile$Nrows_SvDF,
          varname = 'dSurveyData')
  colnames(DatOut$dSurveyData) <-
    c(
      "Index",
      "Year",
      "Fleet",
      "Season",
      "Sex",
      "Maturity",
      "Obs",
      "Base_CV",
      "Actual_CV",
      "Units",
      "q",
      "CPUE_time",
      "Predicted"
    )

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

  if (verbose)
    cat("\t-> Read survey data and derivates \n")
  # -------------------------------------------------------------------------

  # Size composition data ----
  if (verbose)
    cat("-- Reading size composition \n")

  namSizeC <-
    c(
      'Original_series',
      'Modified_series',
      'Year',
      'Fleet',
      'Season',
      'Sex',
      'Type',
      'Shell',
      'Maturity',
      'Stage1_EffN'
    )


  # Loc <- 1145

  # First table combines size_comp_sample_size / d3_obs_size_comps / d3_pre_size_comps
  # Size_data_summary
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  spc_nam <- NULL
  oldk <- 0
  for (n in 1:DatFile$N_SizeFreq_df) {
    k <- CtlFile$iCompAggregator[n]

    if (oldk != k) {
      eval(parse(
        text = paste0(
          "DatOut$",
          tmp,
          "$",
          names(DatFile$SizeFreq)[n],
          " <- get.df(dat, Loc, nrow = ",
          DatFile$Nrows_SiseFreqDF[n],
          ")"
        )
      ))

      eval(parse(
        text = paste0(
          "colnames(DatOut$",
          tmp,
          "$",
          names(DatFile$SizeFreq)[n],
          ") <- c(namSizeC, rep('', dim(DatOut$",
          tmp,
          "$",
          names(DatFile$SizeFreq)[n],
          ")[2]-length(namSizeC)))"
        )
      ))
      tmp_nam <- names(DatFile$SizeFreq)[n]
    } else {
      # tmp_nam <- gsub(x = names(DatFile$SizeFreq)[k],pattern = paste(paste0(nam_sex, "s_"), collapse = "|"), replacement = "")
      # tmp_nam <- paste0("Aggreg_", tmp_nam)
      tmp_nam <- NULL
      spc_nam[oldk] <- gsub(
        x = spc_nam[oldk],
        pattern = paste(paste0(nam_sex, "s_"), collapse = "|"),
        replacement = "Aggr_"
      )
    }

    spc_nam <- c(spc_nam, tmp_nam)
    oldk <- k
  }
  # sdnr_MAR_lf
  # Size data: standard deviation and median
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(unique(CtlFile$iCompAggregator)))
  # eval(parse(text = paste0(
  #   "rownames(DatOut$sdnr_MAR_lf) <- na.omit(names(DatOut$",tmp,")[unique(CtlFile$iCompAggregator)])"
  # )))
  rownames(DatOut$sdnr_MAR_lf) <- spc_nam
  colnames(DatOut$sdnr_MAR_lf) <- c("sdnr", "MAR")

  # Francis weights
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  if (verbose)
    cat("\t-> Read size composition \n")
  # -------------------------------------------------------------------------

  # Selectivity ----
  if (verbose)
    cat("-- Reading selectivity \n")

  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))

  # slx_capture
  nRowSlx <- length(Start_Y:End_Y) * nsex * N_fleet
  eval(parse(
    text = paste0(
      "DatOut$",
      tmp,
      "$slx_capture <- get.df(dat, Loc, nrow = nRowSlx)"
    )
  ))

  # slx_retaind
  nRowRet <- length(Start_Y:End_Y) * nsex * N_fleet
  eval(parse(
    text = paste0(
      "DatOut$",
      tmp,
      "$slx_retained <- get.df(dat, Loc, nrow = nRowSlx)"
    )
  ))
  # slx_discard
  nRowDis <- length(Start_Y:End_Y) * nsex * N_fleet
  eval(parse(
    text = paste0(
      "DatOut$",
      tmp,
      "$slx_discard <- get.df(dat, Loc, nrow = nRowSlx)"
    )
  ))
  # Colnames
  colnames(DatOut$Selectivity$slx_capture) <-
    colnames(DatOut$Selectivity$slx_retained) <-
    colnames(DatOut$Selectivity$slx_discard) <-
    c("year", 'sex', 'fleet', .new)


  # slx_control
  # how many selectivities are we dealing with
  nslx <- 0
  for (f in 1:DatFile$N_fleet) {
    nslx <-
      nslx + CtlFile[["slx_nsel_period_in"]][f] * (1 + CtlFile[["slx_bsex_in"]][f])

    nslx <-
      nslx + CtlFile[["ret_nret_period_in"]][f] * (1 + CtlFile[["ret_bsex_in"]][f])
  }
  DatOut <-
    do.df(dat,
          DatOut,
          Loc,
          LocDatOut,
          nrow = nslx,
          varname = "Select_control")
  # Colnames
  colnames(DatOut$Select_control) <-
    c(
      "Fleet",
      "Parameter_no",
      "Phase",
      'Start_block',
      'End_block',
      'Env_Link',
      'Env_Link_Var',
      'Rand_Walk',
      'Start_year_RW',
      'End_year_RW',
      'Sigma_RW'
    )

  if (verbose)
    cat("\t-> Read selectivity \n")
  # -------------------------------------------------------------------------

  # Natural, Fishing and Total mortality ----

  F_function <-
    function(DatOut = NULL,
             vname = NULL,
             nrow = NULL,
             colnam = NULL) {
      eval(parse(text = paste0(
        "DatOut$", vname, " <- get.df(dat, Loc, nrow = nrow)"
      )))
      eval(parse(text = paste0(
        "colnames(DatOut$", vname, ") <- colnam"
      )))

      assign("Loc", Loc, parent.frame())
      assign("LocDatOut", LocDatOut, parent.frame())
      # assign("DatOut", DatOut, parent.frame())
      return(DatOut)
    }

  if (verbose)
    cat("-- Reading natural, fishing and total mortality \n")
  # m_prop
  DatOut <-
    do.df(
      dat,
      DatOut,
      Loc,
      LocDatOut,
      nrow = length(Start_Y:nyrRetro),
      varname = "m_prop"
    )
  # rownames(DatOut$m_prop) <- Start_Y:nyrRetro
  colnames(DatOut$m_prop) <-
    c("Year", paste0("Season_", 1:DatFile$N_seasons))

  # Natural mortality by size-class
  nRowM <- length(Start_Y:nyrRetro)
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  tmp <- "M_size_class"
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
  for (s in 1:nsex) {
    for (m in 1:nmature) {
      eval(parse(
        text = paste0(
          "DatOut$",
          tmp,
          "$",
          nam_sex[s],
          "$",
          nam_mat[m],
          " <- get.df(dat, Loc, nrow = nRowM)"
        )
      ))
      eval(parse(
        text = paste0(
          "colnames(DatOut$",
          tmp,
          "$",
          nam_sex[s],
          "$",
          nam_mat[m],
          ") <- c('Year', 'Sex', 'Maturity',.new)"
        )
      ))
      eval(parse(
        text = paste0(
          "rownames(DatOut$",
          tmp,
          "$",
          nam_sex[s],
          "$",
          nam_mat[m],
          ") <- .ac(Start_Y:nyrRetro)"
        )
      ))
    }
  }

  # Fully selected fishing mortality by fleet, year and sex
  # Fully_selected_fishing_F_Fl_Yr_Sex
  vname <- "Fully_selected_fishing_F_Fl_Yr_Sex"
  colnam <-
    c("Sex", "Fleet", "Year", paste0("Seas", 1:DatFile$N_seasons))
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_fleet
  DatOut <-
    F_function(DatOut,
               vname = vname,
               nrow = nF,
               colnam = colnam)

  # Fully selected fishing mortality by fleet, year, season and sex
  # Fully_F
  vname <- "Fully_F"
  colnam <-
    c("Sex",
      "Year",
      "Season",
      DatFile$F_Fleet_names,
      DatFile$Survey_names)
  colnam <- stringr::str_subset(string = colnam, pattern = ".+")
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons
  DatOut <-
    F_function(DatOut,
               vname = vname,
               nrow = nF,
               colnam = colnam)

  # F_SizeC_Continuous
  vname <- "F_SizeC_Continuous"
  colnam <- c("Sex", "Year", "Seas")
  colnam <- c(colnam, paste("SizeC_", 1:DatFile$N_sizeC, sep = ""))
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons
  DatOut <-
    F_function(
      DatOut = DatOut,
      vname = vname,
      nrow = nF,
      colnam = colnam
    )

  # F_SizeC_Discrete
  vname <- "F_SizeC_Discrete"
  colnam <- c("Sex", "Year", "Seas")
  colnam <- c(colnam, paste("SizeC_", 1:DatFile$N_sizeC, sep = ""))
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons
  DatOut <-
    F_function(
      DatOut = DatOut,
      vname = vname,
      nrow = nF,
      colnam = colnam
    )

  # TotMorta_SizeC_Continuous
  vname <- "TotMorta_SizeC_Continuous"
  colnam <- c("Sex", "Mature", "Year", "Seas")
  colnam <- c(colnam, paste("SizeC_", 1:DatFile$N_sizeC, sep = ""))
  nF <-
    length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons * DatFile$N_maturity
  DatOut <-
    F_function(
      DatOut = DatOut,
      vname = vname,
      nrow = nF,
      colnam = colnam
    )

  # TotMorta_SizeC_Discrete
  vname <- "TotMorta_SizeC_Discrete"
  colnam <- c("Sex", "Mature", "Year", "Seas")
  colnam <- c(colnam, paste("SizeC_", 1:DatFile$N_sizeC, sep = ""))
  nF <-
    length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons * DatFile$N_maturity
  DatOut <-
    F_function(
      DatOut = DatOut,
      vname = vname,
      nrow = nF,
      colnam = colnam
    )

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

  if (verbose)
    cat("\t-> Read Natural mortality, fishing and total mortality \n")
  # -------------------------------------------------------------------------

  # SSB and N----
  if (verbose)
    cat("-- Reading Numbers at size \n")

  DatOut[["N_at_size"]] <- NULL
  colnam <- c("Year", .new)
  # N_total
  DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_total",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_males
  DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_males",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_females
  if (nsex > 1)
    DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_females",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_males_new
  DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_males_new",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_females_new
  if (nsex > 1)
    DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_females_new",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_males_old
  DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_males_old",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_females_old
  if (nsex > 1)
    DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_females_old",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_males_mature
  DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_males_mature",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_females_mature
  if (nsex > 1)
    DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_females_mature",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_males_imature
  DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_males_imature",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )
  # N_females_imature
  if (nsex > 1)
    DatOut[["N_at_size"]] <-
    F_function(
      DatOut = DatOut[["N_at_size"]],
      vname = "N_females_imature",
      nrow = length(Start_Y:nyrRetro),
      colnam = colnam
    )

  # Group based on sex * mature * shell
  n_gp <- nsex * nshell * nmature
  ptn_sex <- ptn_shell <- ptn_mature <- NULL
  ptn_count <- 1
  for(s in 1:nsex){
    for(m in 1:nmature){
      for(c in 1:nshell){
        ptn_sex[ptn_count] <- s
        ptn_shell[ptn_count] <- c
        ptn_mature[ptn_count] <- m
        ptn_count <- ptn_count + 1
      }
    }
  }

  # N_males_imature_new
  for(g in 1:n_gp)
    if(ptn_sex[g] == 1 && ptn_shell[g] == 1 && ptn_mature[g] == 2){
      # print("N_males_imature_new")

      DatOut[["N_at_size"]] <-
        F_function(
          DatOut = DatOut[["N_at_size"]],
          vname = "N_males_imature_new",
          nrow = length(Start_Y:nyrRetro),
          colnam = colnam
        )
    }
  # N_males_imature_old
  for(g in 1:n_gp)
    if(ptn_sex[g] == 1 && ptn_shell[g] == 2 && ptn_mature[g] == 2){
      # print("N_males_imature_old")

      DatOut[["N_at_size"]] <-
        F_function(
          DatOut = DatOut[["N_at_size"]],
          vname = "N_males_imature_old",
          nrow = length(Start_Y:nyrRetro),
          colnam = colnam
        )
    }
  # N_males_mature_new
  for(g in 1:n_gp)
    if(ptn_sex[g] == 1 && ptn_shell[g] == 1 && ptn_mature[g] == 1){
      # print("N_males_mature_new")

      DatOut[["N_at_size"]] <-
        F_function(
          DatOut = DatOut[["N_at_size"]],
          vname = "N_males_mature_new",
          nrow = length(Start_Y:nyrRetro),
          colnam = colnam
        )
    }
  # N_males_mature_old
  for(g in 1:n_gp)
    if(ptn_sex[g] == 1 && ptn_shell[g] == 2 && ptn_mature[g] == 1){
      # print("N_males_mature_old")

      DatOut[["N_at_size"]] <-
        F_function(
          DatOut = DatOut[["N_at_size"]],
          vname = "N_males_mature_old",
          nrow = length(Start_Y:nyrRetro),
          colnam = colnam
        )
    }

  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # # We don't use this tmp object - Just a way to skip the "SSB and N" line
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


  if (verbose)
    cat("\t-> Read Numbers at size \n")
  # -------------------------------------------------------------------------

  # Molting and growth ----
  if (verbose)
    cat("-- Reading molting and growth \n")

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
  # if(DatFile$NGrowthObs>0){
  #   DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # } else {
  #   Loc <- Loc + 1
  #   LocDatOut <- LocDatOut + 1
  # }
  # # iMoltIncSex
  # if(DatFile$NGrowthObs>0){
  #   DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # } else {
  #   Loc <- Loc + 1
  #   LocDatOut <- LocDatOut + 1
  # }
  # # dMoltInc
  # if(DatFile$NGrowthObs>0){
  #   DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # } else {
  #   Loc <- Loc + 1
  #   LocDatOut <- LocDatOut + 1
  # }

  # molt_probability
  # DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro)*nsex)
  tmp <- "molt_probability"
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
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
    eval(parse(
      text = paste0(
        "colnames(DatOut$",
        tmp,
        "$",
        nam_sex[s],
        ") <- c('Sex', 'Year',.new)"
      )
    ))
  }

  # Growth matrix
  # tmp <- get.namVar(dat, Loc, LocDatOut)
  # tmp <- unlist(stringr::str_split(string = tmp, pattern = "_"))
  # tmp <- paste0(tmp[1], "_", tmp[2])
  tmp <- "Growth_transition_Matrix"
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
          ") <- .new"
        )
      ))
    }
  }

  # Size transition matrix
  tmp <- "Size_transition_Matrix"
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
          ") <- .new"
        )
      ))
    }
  }

  if (verbose)
    cat("\t-> Read molting and growth \n")
  # -------------------------------------------------------------------------

  # Mature Probability ----
  if(nmature == 2){
    if (verbose)
      cat("-- Reading mature probability \n")
    tmp <- "mature_probability"
    eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
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
      eval(parse(
        text = paste0(
          "colnames(DatOut$",
          tmp,
          "$",
          nam_sex[s],
          ") <- c('Sex', 'Year',.new)"
        )
      ))
    }
    if (verbose)
      cat("\t-> Read mature probability \n")
  }
  # -------------------------------------------------------------------------

  # Reference points and OFL ----
  if (verbose)
    cat("-- Reading reference points and OFL \n")

  # fhitfut
  # Which combinations of season (rows) and fleet (column) have F>0 in the forecast
  DatOut[["fhitfut"]] <-
    get.df(dat = dat,
           Loc = Loc,
           nrow = DatFile$N_seasons)
  colnames(DatOut[["fhitfut"]]) <-
    c("Season", paste0("Fleet_", 1:N_fleet))

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
  # # ssbF0
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # spr_bmsy
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # spr_depl
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # SR_alpha_prj
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # SR_beta_prj
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_fofl
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # # sd_fmsy
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # sd_fofl
  # DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  # # spr_cofl
  # DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # spr_cofl_ret
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)

  if (verbose)
    cat("\t-> Read reference points and OFL \n")
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

  nam_rowsdf <- c("CatchDF", "SurveyDF", "SizeFreq_df")
  nam_rowsR <- c("FirstYearDevs", "InitDevs", "SexRatioDevs")
  nam_rowsG <-
    if (CtlFile$bUseGrowthIncrementModel == 1 &
        DatFile$GrowthObsType == 1) {
      paste(nam_sex, "Growth_IncDat", sep = "_")
    } else if (DatFile$GrowthObsType == 2 ||
               DatFile$GrowthObsType == 3) {
      paste(nam_sex, "SizeClass_change", sep = "_")
    }

  nlikes <- length(nam_likes)
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))

  for (l in 1:nlikes) {
    namRows <-
      if (nam_likes[l] %in% c("Catch_data", "Index_data", "Size_data")) {
        eval(parse(
          text = paste0(
            "paste0('",
            nam_rowsdf[l],
            "', 1:DatFile$N_",
            nam_rowsdf[l],
            ")"
          )
        ))
      } else if (nam_likes[l] == "Stock_Recruit") {
        nam_rowsR
      } else {
        nam_rowsG
      }
    if (nam_likes[l] == "Size_data")
      namRows <- namRows[unique(CtlFile$iCompAggregator)]
    if (nam_likes[l] == "Tagging_data")
      namRows <- paste0("Growth_like", "_", nam_sex)


    eval(parse(
      text = paste0("DatOut$", tmp, "$", nam_likes[l], " <- get.vec(dat, Loc)")
    ))

    eval(parse(
      text = paste0(
        "DatOut$",
        tmp,
        "$",
        nam_likes[l],
        " <- as.data.frame(DatOut$",
        tmp,
        "$",
        nam_likes[l],
        ", row.names = namRows)"
      )
    ))
    eval(parse(
      text = paste0("colnames(DatOut$", tmp, "$", nam_likes[l], ") <- 'nloglike'")
    ))
  }

  # nlogPenalty
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  namPenal <- c(
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
    "Fvecs",
    "Fdovs",
    "Vul_devs"
  )
  DatOut$nlogPenalty <-
    as.data.frame(DatOut$nlogPenalty, row.names = namPenal)
  colnames(DatOut$nlogPenalty) <- "nlogPenalty"

  # priorDensity
  # priorDensity for each estimated parameters
  # NVarpar values - Need to implement with the names
  DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)

  if (verbose)
    cat("\t-> Read Loglikelihoods / Penalties / Prior density \n")
  # -------------------------------------------------------------------------

  # End of data file
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("====================================================\n")
    cat("Read of gmacsAll.out file complete.")
    cat("\n")
  }

  return(DatOut)
}
