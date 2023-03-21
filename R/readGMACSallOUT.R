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
#' @seealso \code{\link{readGMACSdat}}, \code{\link{readGMACSctl}}, \code{\link{readGMACSrep}}
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
  do.df <- function(dat, DatOut, Loc, LocDatOut, nrow = NULL) {
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
  DoLine <- function(dat, DatOut, Loc, LocDatOut, fill = FALSE){

    assign("Loc", Loc + 1, parent.frame())
    assign("LocDatOut", LocDatOut + 1, parent.frame())

    tmp <- getLine(dat, Loc, LocDatOut)
    if(fill){
      eval(parse(text = paste("DatOut[[tmp[['nam_var']]]] <- tmp[['vals']]", sep="")))
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
    tmp <- unlist(stringr::str_split(string = tmp,
                                     pattern = ":"))

    tmp1 <- unlist(stringr::str_split(string = tmp[1],pattern = " "))
    tmp1 <- tmp1[!tmp1==""]
    out$nam_var <- ifelse(test = length(tmp1>1),
                          yes = paste0(tmp1, collapse = "_"),
                          no = tmp1)
    tmp <- unlist(stringr::str_split(string = tmp[2],pattern = " "))
    out$vals <- tmp[!tmp==""]
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
  DoMultLine <- function(dat, DatOut, Loc, LocDatOut,
                         row_nam = NULL, var_DatOut = NULL){

    tmp <- get.namVar(dat, Loc, LocDatOut)
    tmp <- stringr::str_replace_all(string = tmp, pattern = "-",replacement = "_")
    tmp <- stringr::str_replace_all(string = tmp, pattern = " ",replacement = "_")

    line_out <- NULL

    if(grepl("Recruitment", tmp, fixed = TRUE) ||  grepl("Growth", tmp, fixed = TRUE)){
      nbLine <- 1
      tmp_line <- DoLine(dat, DatOut, Loc, LocDatOut, fill = FALSE)
      eval(parse(text =
                   paste("line_out <- data.frame(row.names = row_nam,",
                         tmp_line$nam_var, "= tmp_line$vals)",
                         sep="")
      ))
    } else {
      nbLine <- 3
      for(l in 1:nbLine){
        eval(parse(text=paste(
          "tmp_line_",l,"<-getLine(dat, Loc, LocDatOut)", sep=""
        )))
      }
      eval(parse(text =
                   paste("line_out <- data.frame(row.names = row_nam,",
                         tmp_line_1$nam_var, "= tmp_line_1$vals,",
                         tmp_line_2$nam_var, "= tmp_line_2$vals,",
                         tmp_line_3$nam_var, "= tmp_line_3$vals)",
                         sep="")
      ))
    }
    eval(parse(text = paste0("DatOut$",var_DatOut,"$",tmp, " <- line_out")))
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
  fill_para <- function(dat, Loc, nrow, n_split = 3){

    df <- dat[Loc:(Loc + nrow - 1)]
    assign("Loc", Loc + nrow, parent.frame())

    param <- stringr::str_split_fixed(string = df, pattern = ":", n = n_split)[,n_split]
    param <- stringr::str_split_fixed(string = param, pattern = " ", n = 2)[,2]
    param <- stringr::str_split(string = param, pattern = " ")


    outbound <- NULL
    outbound <- suppressWarnings(which(stringr::str_detect(string = param, pattern = "[*]")))
    if(length(outbound)>0){
      for(o in 1:length(outbound)){
        param[[outbound[o]]] <- c(param[[outbound[o]]][1:4], param[[outbound[o]]][6:8])
      }
    }
    for(i in 1:length(param)){
      if(length(param[[i]]) != 7)
        param[[i]] <- c(param[[i]], rep(NA, (7-length(param[[i]]))))
    }
    param <- do.call("rbind", param) ## Make it into a dataframe
    param <- as.data.frame(param, stringsAsFactors = FALSE)
    param <- utils::type.convert(param, as.is = TRUE)

    param$V8 <- ifelse(test = row.names(param)%in%outbound,
                       yes = "***",
                       no = "")
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

  # Read Report file
  dat <- readLines(FileName, warn = FALSE)

  start <- which(stringr::str_detect(dat, "#Likelihoods_by_type") == T)[1]
  Com <-
    grep(x = dat[seq_len(start - 1)],
         pattern = "^#",
         value = TRUE)
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
  nam_type <- c("raw","weighted")
  Lik_type <- NULL

  for (l in 1:nlikes) {
    tmp <- getLine(dat, Loc, LocDatOut)

    eval(parse(text = paste(
      "Lik_type <- rbind(Lik_type,data.frame(row.names = tmp$nam_var,",nam_type[1]," = tmp$vals[1],",nam_type[2]," = tmp$vals[2]))",sep="")
    ))
  }
  DatOut[["Lik_type"]] <- Lik_type
  # nlogPenalty
  DatOut <- DoLine(dat, DatOut, Loc, LocDatOut, fill = TRUE)
  # priorDensity
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
  row_nam <- stringr::str_replace_na(string = row_nam, replacement = "")
  if(length((unique(row_nam)))==1 && unique(row_nam) == ""){
    row_nam <- paste0("Cpue_",1:DatFile$N_SurveyDF)
  } else {
    row_nam[which(row_nam=="")] <- paste0("Cpue_",which(row_nam==""))
  }
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Size-Composition
  row_nam <- NULL
  oldk <- 0

  for (n in 1:DatFile$N_SizeFreq_df) {
    k <- CtlFile$iCompAggregator[n]
    if (oldk != k) {
      row_nam <- c(row_nam, names(DatFile$SizeFreq)[k])
    }
    oldk <- k
  }
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Recruitment penalties
  row_nam <- c("Post_First_YrDevs", "Init_Devs", "SexRatio_Devs")
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Tagging
  row_nam <- paste("Sex_", 1:DatFile$N_sexes, sep="")
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)

  # Growth Likelihood
  row_nam <- paste("Sex_", 1:DatFile$N_sexes, sep="")
  DoMultLine(dat, DatOut, Loc, LocDatOut, row_nam, var_DatOut)
  if (verbose)
    cat("\t-> Read likelihoods by type and fleet \n")
  # -------------------------------------------------------------------------

  # Penalties ----
  if (verbose)
    cat("-- Reading penalties \n")

  nam_Penal <- c("MeanF", "MeanF_dev", "M_devs", "Rec_ini", "Rec_dev","Sex_ratio",
                 "Molt_Prob","Free_Selectivity","Init_N_at_len", "Fdevs", "Fdovs",
                 "SelDevs")
  nPenal <- length(nam_Penal)
  pen_out <- NULL
  for(p in 1:nPenal){
    tmp <- getLine(dat, Loc, LocDatOut)
    pen_out <- cbind(pen_out, tmp$vals)
  }
  pen_out <- data.frame(pen_out, row.names = c("logPenalty", "Penalty_emphasis","Mult"))
  colnames(pen_out) <- nam_Penal
  DatOut[["Penalties_type"]] <- as.data.frame(t(pen_out))

  if (verbose)
    cat("\t-> Read penalties \n")
  # -------------------------------------------------------------------------

  # Extract theta parameters
  # -------------------------------------------------------------------------

  nam_para <- c("Parameter","Estimate", "Phase", "Lower_Bd", "Upper_Bd", "Penalty",
                "Std_error",  "Param_count","CheckBound")
  Param <- NULL

  if (verbose)
    cat("-- Reading theta parameters \n")

  theta <- fill_para(dat, Loc, nrow = CtlFile$ntheta)
  theta <- as.data.frame(cbind(paste("theta",1:CtlFile$ntheta,sep="_"),theta))
  colnames(theta) <- nam_para
  Param[["theta"]] <- theta # theta parameters

  if (verbose)
    cat("\t-> Read theta parameters \n")
  # -------------------------------------------------------------------------

  # Extract growth parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading growth parameters \n")

  Grwth <- fill_para(dat, Loc, nrow = (CtlFile$nGrwth + CtlFile$nSizeIncPar))
  Grwth <- as.data.frame(cbind(paste("Grwth",1:(CtlFile$nGrwth + CtlFile$nSizeIncPar),sep="_"),Grwth))
  colnames(Grwth) <- nam_para
  Param[["Grwth"]] <- Grwth # growth parameters

  if (verbose)
    cat("\t-> Read growth parameters \n")
  # -------------------------------------------------------------------------

  # Extract vulnerability parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading vulnerability parameters \n")

  Vul <- fill_para(dat, Loc, nrow = CtlFile$nslx_pars)
  Vul <- as.data.frame(cbind(paste("log_slx_pars",1:CtlFile$nslx_pars,sep="_"),Vul))
  colnames(Vul) <- nam_para
  Param[["Vul"]] <- Vul # vulnerability parameters

  if (verbose)
    cat("\t-> Read vulnerability parameters \n")
  # -------------------------------------------------------------------------

  # Extract the asymptotic retention parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the number of asymptotic retention parameters \n")

  Asympt <- fill_para(dat, Loc, nrow = CtlFile$NumAsympRet)
  Asympt <- as.data.frame(cbind(paste("Asymret",1:CtlFile$NumAsympRet,sep="_"),Asympt))
  colnames(Asympt) <- nam_para
  Param[["Asympt"]] <- Asympt # asymptotic retention parameters

  if (verbose)
    cat("\t-> Read the asymptotic retention parameters \n")
  # -------------------------------------------------------------------------

  # Extract the time-varying parameters for the vulnerability
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading environmental parameters for vulnerability  \n")
  if(CtlFile$nslx_envpars > 0){
    Envpar_Slx <- fill_para(dat, Loc, nrow = CtlFile$nslx_envpars)
    Envpar_Slx <- as.data.frame(cbind(paste("Envpar_Slx",1:CtlFile$nslx_envpars,sep="_"),Envpar_Slx))
  } else {
    Envpar_Slx <- fill_para(dat, Loc, nrow = 1)
    Envpar_Slx <- as.data.frame(cbind(paste("Envpar_Slx",0,sep="_"),Envpar_Slx))
  }
  colnames(Envpar_Slx) <- nam_para
  Param[["Envpar_Slx"]] <- Envpar_Slx # Estimated environmental parameters

  if (verbose)
    cat("\t-> Read the environmental parameters for vulnerability \n")
  # -------------------------------------------------------------------------

  # Extract vulnerability deviations
  # -------------------------------------------------------------------------
  # These deviations include the environmental impacts if applicable
  if (verbose)
    cat("-- Reading vulnerabity deviations  \n")

  if(CtlFile$NSlx_devs_param > 0){
    Slx_Devs <- fill_para(dat, Loc, nrow = CtlFile$NSlx_devs_param)
    Slx_Devs <- as.data.frame(cbind(paste("Slx_Devs",1:CtlFile$NSlx_devs_param,sep="_"),Slx_Devs))
  } else {
    Slx_Devs <- fill_para(dat, Loc, nrow = 1)
    Slx_Devs <- as.data.frame(cbind(paste("Slx_Devs",0,sep="_"),Slx_Devs))
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
  Fbar <- as.data.frame(cbind(paste("log_fbar",1:N_fleet,sep="_"),Fbar))
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
    assign("findnPara", rbind(findnPara,DatFile$Catch[[i]]))
  findnPara <- unique(findnPara[,c("year", "seas", "fleet")])
  findnPara$nam <- paste("_Yr",findnPara$year, "_Seas", findnPara$seas, "_Fl", findnPara$fleet, sep = "")
  nFparams <- findnPara %>%
    dplyr::group_by(fleet) %>%
    dplyr::summarise(n = dplyr::n())
  # Extract data
  Fdev <- list()
  for(f in 1:N_fleet){
    nrow_F <- ifelse(test = f%in%nFparams$fleet,
                     yes = nFparams[which(nFparams$fleet%in%f),"n", drop = TRUE],
                     no = 1)

    Fdev[[f]] <- fill_para(dat, Loc, nrow = nrow_F)
    ID <- findnPara[findnPara$fleet==f,"nam"]
    Fdev[[f]] <- as.data.frame(cbind(paste("log_Fdev",ID,sep=""),Fdev[[f]]))
    colnames(Fdev[[f]]) <- nam_para
  }
  names(Fdev) <- paste0("log_Fdev_Fl_",1:N_fleet)
  Param[["Fdev"]] <- Fdev # deviations of the male mean F parameters

  if (verbose)
    cat("\t-> Read the male mean fishing mortality rate deviations \n")
  # -------------------------------------------------------------------------

  # Extract the female F offset to male fishing mortality parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the female F offset to male fishing mortality parameters \n")

  Foff <- fill_para(dat, Loc, nrow = N_fleet)
  Foff <- as.data.frame(cbind(paste("log_foff_",1:N_fleet,sep=""),Foff))
  colnames(Foff) <- nam_para
  Param[["Foff"]] <- Foff # female F offset parameters

  if (verbose)
    cat("\t-> Read the female F offset to male fishing mortality parameters \n")
  # -------------------------------------------------------------------------

  # Extract the female F deviation offset parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the female F deviation offset parameters \n")

  Fdov <- list()

  # Find the number of parameter for each fleet
  findnPara <- NULL
  for (i in seq(DatFile$Catch))
    assign("findnPara", rbind(findnPara,DatFile$Catch[[i]]))
  findnPara <- unique(findnPara[,c("year", "seas", "fleet", "sex")])

  if(nsex == 1){
    nYparams <- rep(1, N_fleet)
    for(f in 1:N_fleet){
      Fdov[[f]] <- fill_para(dat, Loc, nrow = nYparams[f])
      Fdov[[f]] <- as.data.frame(cbind(paste("log_Fdov_",f,sep=""),Fdov[[f]]))
      colnames(Fdov[[f]]) <- nam_para
    }
  } else {
    findnPara <- findnPara[findnPara$sex==2,]
    findnPara$nam <- paste("_Yr",findnPara$year, "_Seas", findnPara$seas, "_Fl", findnPara$fleet, sep = "")
    nYparams <- findnPara %>%
      dplyr::group_by(fleet) %>%
      dplyr::summarise(n = dplyr::n())

    # Extract the data

    for(f in 1:N_fleet){
      nrow_F <- ifelse(test = f%in%nYparams$fleet,
                       yes = nYparams[which(nYparams$fleet%in%f),"n", drop = TRUE],
                       no = 1)

      Fdov[[f]] <- fill_para(dat, Loc, nrow = nrow_F)
      ID <- findnPara[findnPara$fleet==f,"nam"]
      Fdov[[f]] <- as.data.frame(cbind(paste("log_Fdov",ID,sep=""),Fdov[[f]]))
      colnames(Fdov[[f]]) <- nam_para
    }
  }
  names(Fdov) <- paste0("log_Fdov_Fl_",1:N_fleet)
  Param[["Fdov"]] <- Fdov # female F deviation offset

  if (verbose)
    cat("\t-> Read the female F deviation offset parameters \n")
  # -------------------------------------------------------------------------

  # Extract the initial values of recruitment
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the initial values for recruitment \n")

  rec_ini <- fill_para(dat, Loc, nrow = DatFile$N_sizeC) # Initial values for recruitment

  rec_ini <- as.data.frame(cbind(paste("SizeC_",1:DatFile$N_sizeC,sep=""),rec_ini))
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
  rec_dev_est <- as.data.frame(cbind(paste("RecDev_est_Yr", DatFile$Start_Y:DatFile$End_Y,sep="_"),rec_dev_est))
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
  logit_rec_prop_est <- as.data.frame(cbind(paste("RecDev_est_Yr", DatFile$Start_Y:DatFile$End_Y,sep="_"),logit_rec_prop_est))
  colnames(logit_rec_prop_est) <- nam_para
  Param[["logit_rec_prop_est"]] <- logit_rec_prop_est # sex-ratio recruitment deviation

  if (verbose)
    cat("\t-> Read the sex-ratio recruitment deviation estimates \n")
  # -------------------------------------------------------------------------

  # Extract the natural mortality deviation parameters
  # -------------------------------------------------------------------------
  if(CtlFile$nMdev>0){
    if (verbose)
      cat("-- Reading the natural mortality deviation parameters \n")

    Mdev <- fill_para(dat, Loc, nrow = CtlFile$nMdev)
    Mdev <- as.data.frame(cbind(paste("m_dev_est_",1:CtlFile$nMdev,sep=""),Mdev))
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
  EffSamp_size <- as.data.frame(cbind(paste("log_vn_",1:nVn,sep=""),EffSamp_size))
  colnames(EffSamp_size) <- nam_para
  Param[["EffSamp_size"]] <- EffSamp_size # effective sample size parameters

  if (verbose)
    cat("\t-> Read the effective sample size parameters \n")
  # -------------------------------------------------------------------------

  # Extract the catchability coefficient parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the catchability coefficient parameters \n")

  survey_Q <- fill_para(dat, Loc, nrow = DatFile$N_SurveyDF)
  survey_Q <- as.data.frame(cbind(paste("survey_q",1:DatFile$N_SurveyDF,sep=""),survey_Q))
  colnames(survey_Q) <- nam_para
  Param[["survey_Q"]] <- survey_Q # catchability coefficient parameters

  if (verbose)
    cat("\t-> Read the catchability coefficient parameters \n")
  # -------------------------------------------------------------------------

  # Extract the addtional CV for surveys/indices parameters
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the addtional CV for surveys/indices parameters \n")

  add_cv <- fill_para(dat, Loc, nrow = DatFile$N_SurveyDF)
  add_cv <- as.data.frame(cbind(paste("log_add_cv_",1:DatFile$N_SurveyDF,sep=""),add_cv))
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

  nam_para <- c("Parameter", "Estimate", "Standard_error", "Estimated_quantity_count")
  nam <- NULL
  DQ <- NULL
  out_recr <- NULL
  nam_sexes <- c("Males", "Females")

  if(GmacsFile$OutVar_BRPs == 1)
    nam <- c("Male_Spr_Rbar", "Female_Spr_Rbar", "SSB_R_ratio_F0", "BMY",
             "Bcurr_BMSY_ratio", "OFL_tot", paste0("Fmsy_Fl",1:N_fleet),
             paste0("FOFL_Fl",1:N_fleet))

  if(GmacsFile$OutVar_Recr == 1)
    nam <- c(nam, paste0(rep(paste0("Log_rec_", nam_sexes[1:nsex]), each=DatFile$N_year), "_", rep(Start_Y:End_Y, times=nsex)))

  if(GmacsFile$OutVar_SSB == 1)
    nam <- c(nam, paste0("Log_ssb_", Start_Y:End_Y))

  if(GmacsFile$OutVar_Fbar == 1)
    nam <- c(nam, paste0("Mean_F_", Start_Y:End_Y))

  if(GmacsFile$OutVar_DynB0 == 1)
    nam <- c(nam, paste0("Log_DynB0_", Start_Y:End_Y))

  if(!is.null(nam)){
    nrow_DQ <- length(nam)
    DQ <- fill_para(dat, Loc, nrow = nrow_DQ,n_split = 2)
    DQ <- as.data.frame(cbind(nam,DQ[,1:3]))
    colnames(DQ) <- nam_para
  }

  # Recruitment
  Recr <- dat[Loc]
  Loc <- Loc + 1
  LocDatOut <- LocDatOut + 1
  Recr <- unlist(stringr::str_split(string = Recr,
                                    pattern = ";"))
  for(n in 1:length(Recr)){
    tmp <- unlist(stringr::str_split(string = Recr[n],
                                     pattern = ":"))
    tmp <- unlist(stringr::str_remove_all(string = tmp,
                                          pattern = " "))
    out_recr <- rbind(out_recr, rbind(tmp))

    if(n ==length(Recr))
      rownames(out_recr) <- rep("", dim(out_recr)[1])
  }

  if(is.null(DQ)){
    MgtQ <- out_recr
  } else {
    MgtQ <- list("Derive_Quantity" = DQ,
                 "Recruitment" = out_recr)
  }
  DatOut[["Management_Quantities"]] <- MgtQ

  if (verbose)
    cat("\t-> Read Management (derived) quantities \n")
  # -------------------------------------------------------------------------


  # Model characteristics ----
  if (verbose)
    cat("-- Reading model characteristics \n")

  # Fleet names
  if(dat[Loc] == "fleetname"){
    DatOut <- do.df(dat, DatOut, Loc, LocDatOut, nrow = N_fleet)
  }
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
    eval(parse(
      text = paste0("DatOut$", tmp, "$", nam_likes[l], " <- get.vec(dat, Loc)")
    ))
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

  colnames(DatOut$dCatchData) <- c("year","seas",'fleet','sex','obs','CV','Type','units','mult','effort','discard_mortality')


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
  colnames(DatOut$dCatchData_out) <- colnames(DatOut$dCatchData)

  if (verbose)
    cat("\t-> Read catch data and derivates \n")
  # -------------------------------------------------------------------------


  # Survey data ----
  if (verbose)
    cat("-- Reading survey data and derivates \n")
  # dSurveyData
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = DatFile$Nrows_SvDF)
  colnames(DatOut$dSurveyData) <-
    c('Index','year','seas','fleet','sex','Mature','Abundance','CV','units','Timing')
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

  # First table combines size_comp_sample_size / d3_obs_size_comps / d3_pre_size_comps
  # Size_data_summary
  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))
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
    }
    oldk <- k
  }

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

  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))

  # slx_capture
  nRowSlx <- length(Start_Y:End_Y) * nsex * N_fleet
  eval(parse(text = paste0(
    "DatOut$", tmp, "$slx_capture <- get.df(dat, Loc, nrow = nRowSlx)"
  )))
  # slx_retaind
  nRowRet <- length(Start_Y:End_Y) * nsex * N_fleet
  eval(parse(text = paste0(
    "DatOut$", tmp, "$slx_retaind <- get.df(dat, Loc, nrow = nRowSlx)"
  )))
  # slx_discard
  nRowDis <- length(Start_Y:End_Y) * nsex * N_fleet
  eval(parse(text = paste0(
    "DatOut$", tmp, "$slx_discard <- get.df(dat, Loc, nrow = nRowSlx)"
  )))
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

  F_function <- function(DatOut = NULL, vname=NULL, nrow = NULL, colnam = NULL){

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
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro))

  # M
  # DatOut <-
  #   do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) * nsex * DatFile$N_maturity)
  # nRowM <- length(Start_Y:nyrRetro) * nsex * DatFile$N_maturity
  nRowM <- length(Start_Y:nyrRetro)
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

  # Fully_selected_fishing_F_Fl
  vname <- "Fully_selected_fishing_F_Fl"
  colnam <- c("Sex", "Year","Seas")
  colnam <- c(colnam, paste( DatOut$fleetname[,1], sep = ""))
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons
  DatOut <- F_function(DatOut, vname = vname, nrow = nF, colnam = colnam)

  # F_SizeC_Continuous
  vname <- "F_SizeC_Continuous"
  colnam <- c("Sex", "Year","Seas")
  colnam <- c(colnam, paste("SizeC_",1:DatFile$N_sizeC, sep = ""))
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons
  DatOut <- F_function(DatOut = DatOut, vname = vname, nrow = nF, colnam = colnam)

  # F_SizeC_Discrete
  vname <- "F_SizeC_Discrete"
  colnam <- c("Sex", "Year","Seas")
  colnam <- c(colnam, paste("SizeC_",1:DatFile$N_sizeC, sep = ""))
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons
  DatOut <- F_function(DatOut = DatOut, vname = vname, nrow = nF, colnam = colnam)

  # TotMorta_SizeC_Continuous
  vname <- "TotMorta_SizeC_Continuous"
  colnam <- c("Sex", "Mature", "Year","Seas")
  colnam <- c(colnam, paste("SizeC_",1:DatFile$N_sizeC, sep = ""))
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons * DatFile$N_maturity
  DatOut <- F_function(DatOut = DatOut, vname = vname, nrow = nF, colnam = colnam)

  # TotMorta_SizeC_Discrete
  vname <- "TotMorta_SizeC_Discrete"
  colnam <- c("Sex", "Mature", "Year","Seas")
  colnam <- c(colnam, paste("SizeC_",1:DatFile$N_sizeC, sep = ""))
  nF <- length(Start_Y:nyrRetro) * nsex * DatFile$N_seasons * DatFile$N_maturity
  DatOut <- F_function(DatOut = DatOut, vname = vname, nrow = nF, colnam = colnam)

  if (verbose)
    cat("\t-> Read Natural mortality, fishing and total mortality \n")
  # -------------------------------------------------------------------------


  # Recuitment ----
  if (verbose)
    cat("-- Reading Recuitment \n")

  tmp <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(text = paste0("DatOut$", tmp, " <- list()")))

  # rec_sdd
  tmpnam <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(
    text = paste0("DatOut$", tmp, "[['", tmpnam, "']] <- get.df(dat, Loc, nrow = nsex)")
  ))
  # rec_ini
  tmpnam <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(
    text = paste0("DatOut$",tmp,"[['",tmpnam,"']] <- get.vec(dat, Loc)")
  ))
  # rec_dev
  tmpnam <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(
    text = paste0("DatOut$",tmp,"[['",tmpnam,"']] <- get.vec(dat, Loc)")
  ))
  # logit_rec_prop
  tmpnam <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(
    text = paste0("DatOut$",tmp,"[['",tmpnam,"']] <- get.vec(dat, Loc)")
  ))
  # recruits
  tmpnam <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(
    text = paste0("DatOut$", tmp, "[['", tmpnam, "']] <- get.df(dat, Loc, nrow = nsex)")
  ))
  # res_recruit
  tmpnam <- get.namVar(dat, Loc, LocDatOut)
  eval(parse(
    text = paste0("DatOut$",tmp,"[['",tmpnam,"']] <- get.vec(dat, Loc)")
  ))

  if (verbose)
    cat("\t-> Read Recruitment \n")
  # -------------------------------------------------------------------------


  # SSB and N----
  if (verbose)
    cat("-- Reading SSB and N \n")
  tmp <- get.namVar(dat, Loc, LocDatOut)
  # We don't use this tmp object - Just a way to skip the "SSB and N" line
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
  if(nsex>1)
    DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_males_new
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_females_new
  if(nsex>1)
    DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_males_old
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_females_old
  if(nsex>1)
    DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_males_mature
  DatOut <-
    do.df(dat, DatOut, Loc, LocDatOut, nrow = length(Start_Y:nyrRetro) + 1)
  # N_females_mature
  if(nsex>1)
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
  if(DatFile$NGrowthObs>0){
    DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  } else {
    Loc <- Loc + 1
    LocDatOut <- LocDatOut + 1
  }
  # iMoltIncSex
  if(DatFile$NGrowthObs>0){
    DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  } else {
    Loc <- Loc + 1
    LocDatOut <- LocDatOut + 1
  }
  # dMoltInc
  if(DatFile$NGrowthObs>0){
    DatOut <- do.vec(dat, DatOut, Loc, LocDatOut)
  } else {
    Loc <- Loc + 1
    LocDatOut <- LocDatOut + 1
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
    }
  }

  # Size transition matrix
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
    }
  }

  if (verbose)
    cat("\t-> Read molting and growth \n")
  # -------------------------------------------------------------------------

  # Reference points and OFL ----
  if (verbose)
    cat("-- Reading reference points and OFL \n")

  # fhitfut
  # Which combinations of season (rows) and fleet (column) have F>0 in the forecast
  DatOut[["fhitfut"]] <- get.df(dat = dat, Loc = Loc, nrow = DatFile$N_seasons)
  colnames(DatOut[["fhitfut"]]) <- c("Season",paste0("Fleet_", 1:N_fleet))

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
  # SR_alpha_prj
  DatOut <- do.num(dat, DatOut, Loc, LocDatOut)
  # SR_beta_prj
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

  if (verbose)
    cat("\t-> Read reference points and OFL \n")
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
