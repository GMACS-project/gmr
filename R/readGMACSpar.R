#' @title readGMACSpar
#'
#' @description Read the gmacs.par file. This is an output of GMACS that contains
#' estimates of all parameters
#'
#' @param Dir (character string)- path to the folder where the parameter file is stored.
#' @param FileName (character string)- name of the parameter file - By default, "gmacs.par"
#' @param verbose (logical)- flag to print processing information
#' @param DatFile (list)- Object containing the .dat file - This is the output
#' of the [readGMACSdat()] function.
#' @param CtlFile (list)- Object containing the .ctl file - This is the output
#' of the [readGMACSctl()] function.
#' @param GMACSdat (list)- Object containing the gmacs.dat file - This is the output
#' of the [readGMACS.dat()] function.
#'
#' @return the gmacs.par file as a named list. Where data frame are used, the
#' columns are parameter_ID/value.
#'
#' \itemize{
#'   \item \code{Nparams} - The number of parameters.
#'   \item \code{ObjFvalue} - The objective function value.
#'   \item \code{MaxGradComp} - The maximum gradient component.
#'   \item \code{theta} - The key parameter controls (core parameters - theta parameters).
#'   \item \code{Grwth} - The growth parameters.
#'   \item \code{Vul} - The vulnerability (selectivity and retention) parameters.
#'   \item \code{Envpar_Slx}
#'   \item \code{Slx_Devs} - The selectivity deviations.
#'   \item \code{Fbar} - The mean fishing mortality parameters.
#'   \item \code{Fdev} - The fishing fleet-specific weights for male.
#'   \item \code{Foff} - The female fishing mortality offset to male F.
#'   \item \code{Fdov} - The fishing fleet-specific weights for female.
#'   \item \code{rec_ini} - The initial recruitment by size-class.
#'   \item \code{rec_dev_est} - The year-specific recruitment deviations.
#'   \item \code{logit_rec_prop_est} - The year-specific sex-ratio recruitment.
#'   \item \code{Mdev} - The natural mortality deviation.
#'   \item \code{M_mat} - The sex-specific natural mortality multiplier for each
#'   maturity state.
#'   \item \code{EffSamp_size} - The effective sample size.
#'   \item \code{survey_Q} - The survey-specific catchability.
#'   \item \code{add_cv} - The survey-specific additional CV.
#' }
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
#' GMACSparfile <- readGMACSpar(Dir =file.path(Dir_Dvpt_Vers, "build", stock, fsep = fsep),
#' FileName = "gmacs.par",
#' verbose = TRUE,
#' DatFile = datFile,
#' CtlFile = ctlFile,
#' GMACSdat = GMACSdat
#' )
#' }
#'
#' @seealso \code{\link{readGMACS.dat}},\code{\link{readGMACSdat}},\code{\link{readGMACSctl}},
#' \code{\link{readGMACSprj}}.
#'
#' @export
#' @md
#
readGMACSpar <- function(Dir = NULL,
                         FileName = "gmacs.par",
                         verbose = NULL,
                         DatFile = NULL,
                         CtlFile = NULL,
                         GMACSdat = NULL) {
  parnam <- get_Param_name(
    CtlFile = CtlFile,
    DatFile = DatFile,
    nyrRetro = GMACSdat$N_Year_Retro
  )

  N_fleet <- DatFile$N_fleet
  Nyears <- DatFile$End_Y - DatFile$Start_Y + 1
  nsex <- DatFile$N_sexes
  fleetname <-
    stringi::stri_remove_empty(c(DatFile$F_Fleet_names, DatFile$Survey_names))

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

  # 2- Read the parameters file and find the first line containing numeric data
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("-- Reading parameters file \n")
    cat("====================================================")
    cat("\n")
  }

  FileName2 <- file.path(Dir, FileName)
  dat <- readLines(FileName2, warn = FALSE)

  DatOut <- list()

  # Get the number of parameters, the value of the objective function and the
  # Maximum gradient component
  Specs <- dat[1]
  if(stringr::str_detect(string = Specs, pattern = "# Number of parameters")){
    getSpecs <-
      stringr::str_extract_all(Specs, "[-+.e0-9]*\\d", simplify = T)
    DatOut[["Nparams"]] <- getSpecs[1]
    DatOut[["ObjFvalue"]] <- getSpecs[2]
    DatOut[["MaxGradComp"]] <- getSpecs[3]
  } else {
    which1 <- dat[stringr::str_detect(string = dat, pattern = "# Number of parameters")]
    DatOut[["Nparams"]] <-
      .an(stringr::str_extract_all(which1, "[-+.e0-9]*\\d", simplify = T))
    which2 <- dat[stringr::str_detect(string = dat, pattern = "# Objective function value")]
    DatOut[["ObjFvalue"]] <-
      .an(stringr::str_extract_all(which2, "[-+.e0-9]*\\d", simplify = T))
    which3 <- dat[stringr::str_detect(string = dat, pattern = "# Maximum gradient component")]
    DatOut[["MaxGradComp"]] <-
      .an(stringr::str_extract_all(which3, "[-+.e0-9]*\\d", simplify = T))
  }
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

  # Initialize the location index
  Loc <- 1
  # -------------------------------------------------------------------------

  # Extract theta parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading theta parameters \n")

  theta <- get.df(dat, Loc, nrow = CtlFile$ntheta)
  theta <-
    as.data.frame(cbind(
      paste("theta[", 1:CtlFile$ntheta, "]", sep = ""),
      parnam$nameTheta,
      theta
    ))
  colnames(theta) <- c("Param_ID", "Param_name", "value")
  DatOut[["theta"]] <- theta # theta parameters

  if (verbose)
    cat("\t-> Read theta parameters \n")
  # -------------------------------------------------------------------------

  # Extract growth parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading growth parameters \n")

  Grwth <-
    get.df(dat, Loc, nrow = (CtlFile$nGrwth + CtlFile$nSizeIncPar))
  Grwth <-
    as.data.frame(cbind(paste(
      "Grwth[", 1:(CtlFile$nGrwth + CtlFile$nSizeIncPar), "]", sep = ""
    ),
    parnam$nameGrwth,
    Grwth))
  colnames(Grwth) <- c("Param_ID", "Param_name", "value")
  DatOut[["Grwth"]] <- Grwth # growth parameters

  if (verbose)
    cat("\t-> Read growth parameters \n")
  # -------------------------------------------------------------------------

  # Extract vulnerability parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading vulnerability parameters \n")

  Vul <- get.df(dat, Loc, nrow = CtlFile$nslx_pars)
  Vul <-
    as.data.frame(cbind(
      paste("log_slx_pars[", 1:CtlFile$nslx_pars, "]", sep = ""),
      parnam$selname1,
      Vul
    ))
  colnames(Vul) <- c("Param_ID", "Param_name", "value")
  DatOut[["Vul"]] <- Vul # vulnerability parameters

  if (verbose)
    cat("\t-> Read vulnerability parameters \n")
  # -------------------------------------------------------------------------

  # Extract the asymptotic retention parameters
  # -------------------------------------------------------------------------
  if (CtlFile$NumAsympRet > 0) {
    if (verbose)
      cat("-- Reading the number of asymptotic retention parameters \n")

    Asympt <- get.df(dat, Loc, nrow = CtlFile$NumAsympRet)
    Asympt <-
      as.data.frame(cbind(
        paste("Asymret[", 1:CtlFile$NumAsympRet, "]", sep = ""),
        parnam$Asymptname,
        Asympt
      ))
    colnames(Asympt) <- c("Param_ID", "Param_name", "value")
    DatOut[["Asympt"]] <- Asympt # asymptotic retention parameters

    if (verbose)
      cat("\t-> Read the asymptotic retention parameters \n")
  } else {
    DatOut[["Asympt"]] <- NULL
  }
  # -------------------------------------------------------------------------

  # Extract the time-varying parameters for the vulnerability
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading environmental parameters for vulnerability  \n")
  if (CtlFile$nslx_envpars > 0) {
    Envpar_Slx <- get.df(dat, Loc, nrow = CtlFile$nslx_envpars)
    Envpar_Slx <-
      as.data.frame(cbind(
        paste("Envpar_Slx", 1:CtlFile$nslx_envpars, sep = "_"),
        parnam$selenvnames1,
        Envpar_Slx
      ))
  } else {
    Envpar_Slx <- get.df(dat, Loc, nrow = 1)
    Envpar_Slx <- as.data.frame(cbind(
      paste("Envpar_Slx", 0, sep = "_"),
      parnam$selenvnames1,
      Envpar_Slx
    ))
  }
  colnames(Envpar_Slx) <- c("Param_ID", "Param_name", "value")
  DatOut[["Envpar_Slx"]] <-
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
    Slx_Devs <- get.df(dat, Loc, nrow = CtlFile$NSlx_devs_param)
    Slx_Devs <-
      as.data.frame(cbind(
        paste("Slx_Devs", 1:CtlFile$NSlx_devs_param, sep = "_"),
        parnam$seldevnames1,
        Slx_Devs
      ))
  } else {
    Slx_Devs <- get.df(dat, Loc, nrow = 1)
    Slx_Devs <- as.data.frame(cbind(
      paste("Slx_Devs", 0, sep = "_"),
      parnam$seldevnames1,
      Slx_Devs
    ))
  }
  colnames(Slx_Devs) <- c("Param_ID", "Param_name", "value")
  DatOut[["Slx_Devs"]] <- Slx_Devs # Estimated selectivty deviations

  if (verbose)
    cat("\t-> Read the deviations for vulnerability \n")
  # -------------------------------------------------------------------------

  # Extract the mean fishing mortality rate parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the mean fishing mortality rate parameters \n")

  Fbar <- get.df(dat, Loc, nrow = N_fleet)
  Fbar <-
    as.data.frame(cbind(
      paste("log_fbar[", 1:N_fleet, "]", sep = ""),
      parnam$nameFbar,
      Fbar
    ))
  colnames(Fbar) <- c("Param_ID", "Param_name", "value")
  DatOut[["Fbar"]] <- Fbar # mean F parameters

  if (verbose)
    cat("\t-> Read the mean fishing mortality rate parameters \n")
  # -------------------------------------------------------------------------

  # Extract the male mean fishing mortality rate deviations
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the male mean fishing mortality rate deviations \n")
  Fdev <- list()
  for (d in 1:N_fleet) {
    Fdev[[d]] <-
      as.data.frame(cbind(parnam$nameFdev[[d]], get.vec(dat, Loc)))
    colnames(Fdev[[d]]) <- c("Param_name", "value")
  }
  names(Fdev) <- fleetname
  DatOut[["Fdev"]] <-
    Fdev # deviations of the male mean F parameters

  if (verbose)
    cat("\t-> Read the male mean fishing mortality rate deviations \n")
  # -------------------------------------------------------------------------

  # Extract the female F offset to male fishing mortality parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the female F offset to male fishing mortality parameters \n")

  Foff <- get.df(dat, Loc, nrow = N_fleet)
  Foff <-
    as.data.frame(cbind(
      paste("log_foff[", 1:N_fleet, "]", sep = ""),
      parnam$nameFoff,
      Foff
    ))
  colnames(Foff) <- c("Param_ID", "Param_name", "value")
  DatOut[["Foff"]] <- Foff # female F offset parameters

  if (verbose)
    cat("\t-> Read the female F offset to male fishing mortality parameters \n")
  # -------------------------------------------------------------------------

  # Extract the female F deviation offset parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the female F deviation offset parameters \n")
  Fdov <- list()
  for (d in 1:N_fleet) {
    Fdov[[d]] <- data.frame(Param_name = parnam$nameFdov[[d]],
                            value = get.vec(dat, Loc))
    # colnames(Fdov[[d]]) <- c("Param_name","value")
  }
  names(Fdov) <- fleetname
  DatOut[["Fdov"]] <- Fdov # female F deviation offset

  if (verbose)
    cat("\t-> Read the female F deviation offset parameters \n")
  # -------------------------------------------------------------------------

  # Extract the initial values of recruitment
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the initial values for recruitment \n")

  # rec_ini <- get.vec(dat, Loc)
  DatOut[["rec_ini"]] <-
    data.frame(Param_name = parnam$nameRecrInit,
               value = get.vec(dat, Loc)) # Initial values for recruitment

  if (verbose)
    cat("\t-> Read the initial values for recruitment \n")
  # -------------------------------------------------------------------------

  # Extract the recruitment deviation estimates
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the recruitment deviation estimates \n")

  DatOut[["rec_dev_est"]] <-
    data.frame(Param_name = parnam$nameRecrDev,
               value = get.vec(dat, Loc)) # recruitment deviation

  if (verbose)
    cat("\t-> Read the recruitment deviation estimates \n")
  # -------------------------------------------------------------------------

  # Extract the sex-ratio recruitment deviation estimates
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the sex-ratio recruitment deviation estimates \n")

  DatOut[["logit_rec_prop_est"]] <-
    data.frame(Param_name = parnam$nameRecrLogitProp,
               value = get.vec(dat, Loc))  # sex-ratio recruitment deviation

  if (verbose)
    cat("\t-> Read the sex-ratio recruitment deviation estimates \n")
  # -------------------------------------------------------------------------

  # Extract the natural mortality deviation parameters
  # -------------------------------------------------------------------------
  if (CtlFile$nMdev > 0) {
    if (verbose)
      cat("-- Reading the natural mortality deviation parameters \n")
    Mdev <- get.df(dat, Loc, nrow = CtlFile$nMdev)
    Mdev <- as.data.frame(cbind(
      paste("m_dev_est[", 1:CtlFile$nMdev,
            "]", sep = ""),
      parnam$nameMortality,
      Mdev
    ))
    colnames(Mdev) <- c("Param_ID", "Param_name", "value")
    DatOut[["Mdev"]] <- Mdev # Natural mortality deviations
    if (verbose)
      cat("\t-> Read the natural mortality deviation parameters \n")
  } else {
    DatOut[["Mdev"]] <- NULL
  }
  # -------------------------------------------------------------------------

  # Extract the maturity specific natural mortality parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the maturity specific natural mortality parameters \n")
  nM_mat <- dim(CtlFile$m_mat_controls)[1]
  M_mat <- get.df(dat, Loc, nrow = nM_mat)
  M_mat <-
    as.data.frame(cbind(
      paste("m_mat_mult[", 1:nM_mat, "]", sep = ""),
      parnam$nameMat_NatMort,
      M_mat
    ))
  colnames(M_mat) <- c("Param_ID", "Param_name", "value")
  DatOut[["M_mat"]] <- M_mat # maturity specific natural mortality

  if (verbose)
    cat("\t-> Read the maturity specific natural mortality parameters \n")
  # -------------------------------------------------------------------------

  # Extract the effective sample size parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the effective sample size parameters \n")

  nVn <- max(CtlFile$iCompAggregator)
  EffSamp_size <- get.df(dat, Loc, nrow = nVn)
  EffSamp_size <-
    as.data.frame(cbind(
      paste("log_vn[", 1:nVn, "]", sep = ""),
      parnam$nameEffSamp,
      EffSamp_size
    ))
  colnames(EffSamp_size) <- c("Param_ID", "Param_name", "value")
  DatOut[["EffSamp_size"]] <-
    EffSamp_size # effective sample size parameters

  if (verbose)
    cat("\t-> Read the effective sample size parameters \n")
  # -------------------------------------------------------------------------

  # Extract the catchability coefficient parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the catchability coefficient parameters \n")

  survey_Q <- get.df(dat, Loc, nrow = DatFile$N_SurveyDF)
  survey_Q <-
    as.data.frame(cbind(
      paste("survey_q[", 1:DatFile$N_SurveyDF, "]", sep = ""),
      parnam$nameCatchability,
      survey_Q
    ))
  colnames(survey_Q) <- c("Param_ID", "Param_name", "value")
  DatOut[["survey_Q"]] <-
    survey_Q # catchability coefficient parameters

  if (verbose)
    cat("\t-> Read the catchability coefficient parameters \n")
  # -------------------------------------------------------------------------

  # Extract the addtional CV for surveys/indices parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the addtional CV for surveys/indices parameters \n")

  add_cv <- get.df(dat, Loc, nrow = DatFile$N_SurveyDF)
  add_cv <-
    as.data.frame(cbind(
      paste("log_add_cv[", 1:DatFile$N_SurveyDF, "]", sep = ""),
      parnam$nameAddCV,
      add_cv
    ))
  colnames(add_cv) <- c("Param_ID", "Param_name", "value")
  DatOut[["add_cv"]] <- add_cv # addtional CV parameters

  if (verbose)
    cat("\t-> Read the addtional CV for surveys/indices parameters \n")
  # -------------------------------------------------------------------------

  # End of data file
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("====================================================\n")
    cat("Read of parameter file complete.")
    cat("\n")
  }
  return(DatOut)
}
