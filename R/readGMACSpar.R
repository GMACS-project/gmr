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
#' @param CtlFile (list)- Object (list) containing the .ctl file - This is the output
#' of the [readGMACSctl()] function.
#'
#' @return the gmacs.par file as a named list. Where data frame are used, the
#' columns are parameter_ID/value.
#'
#' @seealso \code{\link{readGMACS.dat}},\code{\link{readGMACSdat}},\code{\link{readGMACSctl}},
#' \code{\link{readGMACSprj}}
#'
#' @export
#' @md
#
readGMACSpar <- function(Dir = NULL,
                         FileName = "gmacs.par",
                         verbose = NULL,
                         DatFile = NULL,
                         CtlFile = NULL){
  DatOut <- list()

  N_fleet <- DatFile$N_fleet
  Nyears <- DatFile$End_Y - DatFile$Start_Y + 1

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

  FileName <- file.path(Dir, FileName)
  dat <- readLines(FileName, warn = FALSE)
  # Get the number of parameters, the value of the objective function and the
  # Maximum gradient component
  Specs <- dat[1]
  getSpecs <- stringr::str_extract_all(Specs, "[-+.e0-9]*\\d",simplify = T)
  DatOut[["Nparams"]] <- getSpecs[1]
  DatOut[["ObjFvalue"]] <- getSpecs[2]
  DatOut[["MaxGradComp"]] <- getSpecs[3]
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
  theta <- as.data.frame(cbind(paste("theta[",1:CtlFile$ntheta,"]",sep=""),theta))
  colnames(theta) <- c("Param_ID","value")
  DatOut[["theta"]] <- theta # theta parameters

  if (verbose)
    cat("-> Read theta parameters \n")
  # -------------------------------------------------------------------------

  # Extract growth parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading growth parameters \n")

  Grwth <- get.df(dat, Loc, nrow = (CtlFile$nGrwth + CtlFile$nSizeIncPar))
  Grwth <- as.data.frame(cbind(paste("Grwth[",1:(CtlFile$nGrwth + CtlFile$nSizeIncPar),"]",sep=""),Grwth))
  colnames(Grwth) <- c("Param_ID","value")
  DatOut[["Grwth"]] <- Grwth # growth parameters

  if (verbose)
    cat("-> Read growth parameters \n")
  # -------------------------------------------------------------------------

  # Extract vulnerability parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading vulnerability parameters \n")

  Vul <- get.df(dat, Loc, nrow = CtlFile$nslx_pars)
  Vul <- as.data.frame(cbind(paste("log_slx_pars[",1:CtlFile$nslx_pars,"]",sep=""),Vul))
  colnames(Vul) <- c("Param_ID","value")
  DatOut[["Vul"]] <- Vul # vulnerability parameters

  if (verbose)
    cat("-> Read vulnerability parameters \n")
  # -------------------------------------------------------------------------

  # Extract the asymptotic retention parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the number of asymptotic retention parameters \n")

  Asympt <- get.df(dat, Loc, nrow = CtlFile$NumAsympRet)
  Asympt <- as.data.frame(cbind(paste("Asymret[",1:CtlFile$NumAsympRet,"]",sep=""),Asympt))
  colnames(Asympt) <- c("Param_ID","value")
  DatOut[["Asympt"]] <- Asympt # asymptotic retention parameters

  if (verbose)
    cat("-> Read the asymptotic retention parameters \n")
  # -------------------------------------------------------------------------

  # Extract the mean fishing mortality rate parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the mean fishing mortality rate parameters \n")

  Fbar <- get.df(dat, Loc, nrow = N_fleet)
  Fbar <- as.data.frame(cbind(paste("log_fbar[",1:N_fleet,"]",sep=""),Fbar))
  colnames(Fbar) <- c("Param_ID","value")
  DatOut[["Fbar"]] <- Fbar # mean F parameters

  if (verbose)
    cat("-> Read the mean fishing mortality rate parameters \n")
  # -------------------------------------------------------------------------

  # Extract the male mean fishing mortality rate deviations
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the male mean fishing mortality rate deviations \n")
  Fdev <- list()
  for(d in 1:N_fleet){
    Fdev[[d]] <- get.vec(dat, Loc)
  }
  names(Fdev) <- paste0("log_Fdev[",1:N_fleet,"]")
  DatOut[["Fdev"]] <- Fdev # deviations of the male mean F parameters

  if (verbose)
    cat("-> Read the male mean fishing mortality rate deviations \n")
  # -------------------------------------------------------------------------

  # Extract the female F offset to male fishing mortality parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the female F offset to male fishing mortality parameters \n")

  Foff <- get.df(dat, Loc, nrow = N_fleet)
  Foff <- as.data.frame(cbind(paste("log_foff[",1:N_fleet,"]",sep=""),Foff))
  colnames(Foff) <- c("Param_ID","value")
  DatOut[["Foff"]] <- Foff # female F offset parameters

  if (verbose)
    cat("-> Read the female F offset to male fishing mortality parameters \n")
  # -------------------------------------------------------------------------

  # Extract the female F deviation offset parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the female F deviation offset parameters \n")
  Fdov <- list()
  for(d in 1:N_fleet){
    Fdov[[d]] <- get.vec(dat, Loc)
  }
  names(Fdov) <- paste0("log_Fdov[",1:N_fleet,"]")
  DatOut[["Fdov"]] <- Fdov # female F deviation offset

  if (verbose)
    cat("-> Read the female F deviation offset parameters \n")
  # -------------------------------------------------------------------------

  # Extract the initial values of recruitment
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the initial values for recruitment \n")

  # rec_ini <- get.vec(dat, Loc)
  DatOut[["rec_ini"]] <- get.vec(dat, Loc) # Initial values for recruitment

  if (verbose)
    cat("-> Read the initial values for recruitment \n")
  # -------------------------------------------------------------------------

  # Extract the recruitment deviation estimates
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the recruitment deviation estimates \n")

  DatOut[["rec_dev_est"]] <- get.vec(dat, Loc) # recruitment deviation

  if (verbose)
    cat("-> Read the recruitment deviation estimates \n")
  # -------------------------------------------------------------------------

  # Extract the sex-ratio recruitment deviation estimates
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the sex-ratio recruitment deviation estimates \n")

  DatOut[["logit_rec_prop_est"]] <- get.vec(dat, Loc) # sex-ratio recruitment deviation

  if (verbose)
    cat("-> Read the sex-ratio recruitment deviation estimates \n")
  # -------------------------------------------------------------------------

  # Extract the natural mortality deviation parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the natural mortality deviation parameters \n")

  Mdev <- get.df(dat, Loc, nrow = CtlFile$nMdev)
  Mdev <- as.data.frame(cbind(paste("m_dev_est[",1:CtlFile$nMdev,"]",sep=""),Mdev))
  colnames(Mdev) <- c("Param_ID","value")
  DatOut[["Mdev"]] <- Mdev # Natural mortality deviations

  if (verbose)
    cat("-> Read the natural mortality deviation parameters \n")
  # -------------------------------------------------------------------------

  # Extract the maturity specific natural mortality parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the maturity specific natural mortality parameters \n")
  nM_mat <- dim(CtlFile$m_mat_controls)[1]
  M_mat <- get.df(dat, Loc, nrow = nM_mat)
  M_mat <- as.data.frame(cbind(paste("m_mat_mult[",1:nM_mat,"]",sep=""),M_mat))
  colnames(M_mat) <- c("Param_ID","value")
  DatOut[["M_mat"]] <- M_mat # maturity specific natural mortality

  if (verbose)
    cat("-> Read the maturity specific natural mortality parameters \n")
  # -------------------------------------------------------------------------

  # Extract the effective sample size parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the effective sample size parameters \n")

  nVn <- max(CtlFile$iCompAggregator)
  EffSamp_size <- get.df(dat, Loc, nrow = nVn)
  EffSamp_size <- as.data.frame(cbind(paste("log_vn[",1:nVn,"]",sep=""),EffSamp_size))
  colnames(EffSamp_size) <- c("Param_ID","value")
  DatOut[["EffSamp_size"]] <- EffSamp_size # effective sample size parameters

  if (verbose)
    cat("-> Read the effective sample size parameters \n")
  # -------------------------------------------------------------------------

  # Extract the catchability coefficient parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the catchability coefficient parameters \n")

  survey_Q <- get.df(dat, Loc, nrow = DatFile$N_SurveyDF)
  survey_Q <- as.data.frame(cbind(paste("survey_q[",1:DatFile$N_SurveyDF,"]",sep=""),survey_Q))
  colnames(survey_Q) <- c("Param_ID","value")
  DatOut[["survey_Q"]] <- survey_Q # catchability coefficient parameters

  if (verbose)
    cat("-> Read the catchability coefficient parameters \n")
  # -------------------------------------------------------------------------

  # Extract the addtional CV for surveys/indices parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the addtional CV for surveys/indices parameters \n")

  add_cv <- get.df(dat, Loc, nrow = DatFile$N_SurveyDF)
  add_cv <- as.data.frame(cbind(paste("log_add_cv[",1:DatFile$N_SurveyDF,"]",sep=""),add_cv))
  colnames(add_cv) <- c("Param_ID","value")
  DatOut[["add_cv"]] <- add_cv # addtional CV parameters

  if (verbose)
    cat("-> Read the addtional CV for surveys/indices parameters \n")
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
