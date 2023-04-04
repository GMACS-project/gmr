#' @title Read the GMACS data file
#'
#' @description Read the GMACS data file.
#
#' @param FileName (character string)- path (and name) of the data file (e.g. snow.data)
#' @param verbose (logical)- (TRUE/FALSE); flag to print processing information
#'
#' @return the .dat file as a named list.
#'
#' @seealso \code{\link{readGMACS.dat}},\code{\link{readGMACSctl}},
#' \code{\link{readGMACSprj}}
#'
#' @export
#' @md
#
readGMACSdat <- function(FileName = NULL,
                         verbose = TRUE) {
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

  # @title namCatch
  #
  # @description Name the catch data frame.
  #
  # @param df one catch data frame
  #
  # @return the name of the catch data frame as "Sex_Type_Fleet_Seas".
  #
  namCatch <- function(df) {
    sex <- base::switch(.ac(unique(df[, "sex"])),
                        "0" = "Both",
                        "1" = "Males",
                        "2" = "Females")
    type <- base::switch(.ac(unique(df[, "Type"])),
                         "0" = "Total",
                         "1" = "Retained",
                         "2" = "Discard")
    fleet <- DatOut$F_Fleet_names[unique(df[, "fleet"])]
    Seas <- paste0("Seas", unique(df[, "seas"]))
    nam <- paste(sex, type, fleet, Seas, sep = "_")
    return(nam)
  }
  # @title namSizeFq
  #
  # @description Name the Size Frequency data frame.
  #
  # @param df one Size frequency data frame
  #
  # @return the name of the size frequency data frame as "Sex_Type_Fleet_Seas".
  #
  namSizeFq <- function(df) {
    if (length(unique(df[, "sex"])) > 1) {
      sex <- "Males_Females_"
    } else {
      sex <- base::switch(.ac(unique(df[, "sex"])),
                          "0" = "Both",
                          "1" = "Males",
                          "2" = "Females")
    }
    type <- base::switch(.ac(unique(df[, "Type"])),
                         "0" = "Total",
                         "1" = "Retained",
                         "2" = "Discard")
    mat <- base::switch(.ac(unique(df[, "Maturity"])),
                        "0" = "BothMat",
                        "1" = "Mature",
                        "2" = "Immat")
    fleet <-
      c(DatOut$F_Fleet_names, DatOut$Survey_names)[unique(df[, "fleet"])]
    Seas <- paste0("Seas", unique(df[, "seas"]))
    nam <- paste(sex, mat, type, fleet, Seas, sep = "_")
    return(nam)
  }
  # -------------------------------------------------------------------------

  # 2- Read the data file and find the first line containing numeric data
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("-- Reading data file \n")
    cat("====================================================\n")
  }

  dat <- readLines(FileName, warn = FALSE)

  Startdat <- which(stringr::str_detect(dat, "^\\#.*") == F)[1]
  Com <-
    grep(x = dat[seq_len(Startdat - 1)],
         pattern = "^#",
         value = TRUE)
  if (verbose)
    cat(
      "-> The start year of the assessment that has been detected is:",
      sub("\\D*(\\d+).*", "\\1", dat[Startdat])[1],
      "\n\n"
    )
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

  # Model dimensions
  # ======================================================================= #
  # -------------------------------------------------------------------------
  DatOut[["Start_Y"]] <- get.num(dat, Loc) # Start year
  DatOut[["End_Y"]] <- get.num(dat, Loc) # End year
  DatOut[["N_year"]] <- DatOut$End_Y - DatOut$Start_Y + 1
  DatOut[["N_seasons"]] <- get.num(dat, Loc) # Number of season
  DatOut[["N_fleet"]] <-
    get.num(dat, Loc) # Number of fleets (fishing fleets and surveys)
  DatOut[["N_sexes"]] <- get.num(dat, Loc) # Number of sexes
  DatOut[["N_shell_cdt"]] <-
    get.num(dat, Loc) # Number of shell condition types
  DatOut[["N_maturity"]] <-
    get.num(dat, Loc) # Number of maturity types
  DatOut[["N_sizeC"]] <-
    get.num(dat, Loc) # Number of size class in the model
  DatOut[["Recr_Season"]] <-
    get.num(dat, Loc) # Season recruitment occurs
  DatOut[["Grwth_Season"]] <-
    get.num(dat, Loc) # Season molting & growth occur
  DatOut[["SSB_Season"]] <-
    get.num(dat, Loc) # Season to calculate SSB
  DatOut[["N_Season"]] <- get.num(dat, Loc) # Season for N output

  if (verbose)
    cat("-> Read model dimensions. \n")
  # ======================================================================= #
  # -------------------------------------------------------------------------

  # Size classes definition
  # -------------------------------------------------------------------------
  if (DatOut[["N_sexes"]] == 1) {
    # Maximum size class
    DatOut[["Max_sizeC"]] <- get.num(dat, Loc) # for males only
  } else {
    DatOut[["Max_sizeC"]] <-
      get.vec(dat, Loc) # for males and then females
  }
  DatOut[["Size_breaks"]] <- get.vec(dat, Loc) # Size breaks
  # vector giving the break points between size intervals

  if (verbose)
    cat("-> Read size class definition. \n")
  # -------------------------------------------------------------------------

  # Natural mortality
  # -------------------------------------------------------------------------
  DatOut[["M_in_Type"]] <-
    get.num(dat, Loc) # Natural mortality per season input type
  # If M_in_Type == 1 vector by season
  # If M_in_Type == 2 matrix by year/season => matrix[N_year, N_seasons]
  DatOut[["M_Seas_prop"]] <- base::switch(
    DatOut$M_in_Type,
    "1" = get.vec(dat, Loc),
    "2" = get.df(dat, Loc, nrow = DatOut$N_year),
    "3" = stop(
      "Input type '3' does not
                                                         exist for the natural mortality
                                                         rate per season"
    )
  )

  if (verbose)
    cat("-> Read natural mortality settings. \n")
  # -------------------------------------------------------------------------

  # Fishery and survey definition
  # -------------------------------------------------------------------------
  DatOut[["F_Fleet_names"]] <- ifelse(DatOut$N_fleet == 1,
                                      get.num(dat, Loc, num = FALSE),
                                      get.vec(dat, Loc, num = FALSE))[[1]] # Fishing fleet names

  if (length(DatOut[["F_Fleet_names"]]) == DatOut$N_fleet) {
    DatOut[["Survey_names"]] <- ""
  } else {
    DatOut[["Survey_names"]] <-
      ifelse((DatOut$N_fleet - length(DatOut$F_Fleet_names)) == 1,
             get.num(dat, Loc, num = FALSE),
             get.vec(dat, Loc, num = FALSE)
      )[[1]] # Survey names
  }

  if (DatOut$N_seasons == 1) {
    # Type of fishing mortality per season (0: instantaneous; 1: continuous)
    DatOut[["F_Season_Type"]] <- get.num(dat, Loc)
  } else {
    DatOut[["F_Season_Type"]] <- get.vec(dat, Loc)
  }

  if (verbose)
    cat("-> Read fishery and survey definition. \n")
  # -------------------------------------------------------------------------

  # Catch data
  # -------------------------------------------------------------------------
  DatOut[["N_CatchDF"]] <-
    get.num(dat, Loc) # Number of catch data frame

  if (DatOut$N_CatchDF == 0) {
    DatOut[["Nrows_CatchDF"]] <- ""
  } else if (DatOut$N_CatchDF == 1) {
    DatOut[["Nrows_CatchDF"]] <- get.num(dat, Loc)
  } else {
    DatOut[["Nrows_CatchDF"]] <- get.vec(dat, Loc)
  }

  if (DatOut$N_CatchDF == 0) {
    DatOut[["Catch"]] <- ""
  } else {
    DatOut[["Catch"]] <- list()
    namcatch <- NULL

    for (n in 1:DatOut$N_CatchDF) {
      DatOut[["Catch"]][[n]] <-
        get.df(dat, Loc, nrow = DatOut$Nrows_CatchDF[n])
      colnames(DatOut[["Catch"]][[n]]) <- c(
        "year",
        "seas",
        "fleet",
        "sex",
        "obs",
        "CV",
        "Type",
        "units",
        "mult",
        "effort",
        "discard_mortality"
      )
      tmp <- namCatch(DatOut[["Catch"]][[n]])
      namcatch <- c(namcatch, tmp)
    }
    names(DatOut[["Catch"]]) <- namcatch
  }

  if (verbose)
    cat("-> Read catch data. \n")
  # -------------------------------------------------------------------------

  # Relative abundance index
  # -------------------------------------------------------------------------
  DatOut[["N_SurveyDF"]] <-
    get.num(dat, Loc) # Number of relative abundance indices

  if (verbose) {
    if (DatOut$N_SurveyDF != length(DatOut$Survey_names)) {
      cat(
        "The number of relative abundance indices does not correspond to the\nnumber of survey names. GMACS will consider CPUE index"
      )
    }
  }

  if (DatOut$N_SurveyDF == 0) {
    DatOut[["Sv_type"]] <- ""
  } else if (DatOut$N_SurveyDF == 1) {
    # Data type for each abundance index (1: total selectivity; 2:retention*selectivity)
    DatOut[["Sv_type"]] <- get.num(dat, Loc)
  } else {
    DatOut[["Sv_type"]] <- get.vec(dat, Loc)
  }
  if (DatOut$N_SurveyDF == 0) {
    DatOut[["Nrows_SvDF"]] <- ""
  } else {
    DatOut[["Nrows_SvDF"]] <-
      get.num(dat, Loc) # Number of rows of index data
  }

  if (DatOut$N_SurveyDF == 0) {
    DatOut[["Surveys"]] <-  ""
  } else {
    DatOut[["Surveys"]] <- list()
    tmp <- get.df(dat, Loc, nrow = DatOut$Nrows_SvDF)
    colnames(tmp) <- c(
      "Index",
      "year",
      "seas",
      "fleet",
      "sex",
      "Mature",
      "Abundance",
      "CV",
      "units",
      "Timing"
    )
    for (n in 1:DatOut$N_SurveyDF) {
      # n <- 1
      DatOut[["Surveys"]][[n]] <-
        tmp[which(tmp[, 'fleet'] == unique(tmp[, 'fleet'])[n]), ]
      names(DatOut[["Surveys"]])[n] <- DatOut$Survey_names[n]
    }
  }
  if (verbose)
    cat("-> Read survey data. \n")
  # -------------------------------------------------------------------------


  # SIZE COMPOSITION DATA FOR ALL FLEETS
  # -------------------------------------------------------------------------
  DatOut[["N_SizeFreq_df"]] <-
    get.num(dat, Loc) #Number of length frequency matrix

  if (DatOut$N_SizeFreq_df == 0) {
    DatOut[["Nrows_SiseFreqDF"]] <-  ""
  } else if (DatOut$N_SizeFreq_df == 1) {
    # Number of rows in each length frequency matrix
    DatOut[["Nrows_SiseFreqDF"]] <-  get.num(dat, Loc)
  } else {
    DatOut[["Nrows_SiseFreqDF"]] <-  get.vec(dat, Loc)
  }

  if (DatOut$N_SizeFreq_df == 0) {
    DatOut[["Nbins_SiseFreq"]] <-  ""
  } else if (DatOut$N_SizeFreq_df == 1) {
    # Number of bins in each length frequency matrix
    DatOut[["Nbins_SiseFreq"]] <-  get.num(dat, Loc)
  } else {
    DatOut[["Nbins_SiseFreq"]] <-  get.vec(dat, Loc)
  }

  if (DatOut$N_SizeFreq_df == 0) {
    DatOut[["SizeFreq"]] <- ""
  } else {
    DatOut[["SizeFreq"]] <- list() # Length frequency matrices
    namSF <- NULL

    for (n in 1:DatOut$N_SizeFreq_df) {
      DatOut[["SizeFreq"]][[n]] <-
        get.df(dat, Loc, nrow = DatOut$Nrows_SiseFreqDF[n])
      colnames(DatOut[["SizeFreq"]][[n]]) <- c(
        "year",
        "seas",
        "fleet",
        "sex",
        "Type",
        "Shell",
        "Maturity",
        "Nsamp",
        rep("", DatOut[["Nbins_SiseFreq"]][n])
      )
      tmp <- namSizeFq(DatOut[["SizeFreq"]][[n]])
      namSF <- c(namSF, tmp)
    }
    names(DatOut[["SizeFreq"]]) <- namSF
  }
  if (verbose)
    cat("-> Read size composition data. \n")
  # -------------------------------------------------------------------------

  # GROWTH DATA
  # -------------------------------------------------------------------------
  DatOut[["GrowthObsType"]] <-
    get.num(dat, Loc) # Type of observation (increment or change in size-class)
  if (DatOut[["GrowthObsType"]] > 3)
    stop("GrowthObsType can only be 0,1, 2 or 3")
  DatOut[["NGrowthObs"]] <-
    get.num(dat, Loc) # Number of observation (lines of the df)

  if (DatOut[["GrowthObsType"]] > 0 & DatOut[["NGrowthObs"]] > 0) {
    namGrowthObs <- base::switch(
      .ac(DatOut$GrowthObsType),
      "1" = c("Premolt", "Sex", "Molt_Inc", "CV"),
      "2" = c("Size_rel", "Size_Recap", "T_at_sea"),
      "3" = c(
        "Size_rel",
        "sex",
        "Size_Recap",
        "T_at_sea",
        "fleet",
        "Recap_Year",
        "Number"
      )
    )
    DatOut[["GrowthData"]] <-
      get.df(dat, Loc, nrow = DatOut[["NGrowthObs"]])
    colnames(DatOut[["GrowthData"]]) <- namGrowthObs
  } else {
    DatOut[["GrowthData"]] <- ""
  }
  if (verbose)
    cat("-> Read growth data. \n")
  # -------------------------------------------------------------------------

  # ENVIRONMENTAL DATA
  # -------------------------------------------------------------------------
  DatOut[["NenvIndics"]] <-
    get.num(dat, Loc) # Number of environmental indices
  if (DatOut$NenvIndics > 0) {
    # Years cover for each index
    DatOut[["EnvYrs"]] <-
      get.df(dat, Loc, nrow = DatOut$NenvIndics)
    # Environmental data
    NenvData = 0
    for (e in 1:DatOut$NenvIndics)
      NenvData = NenvData + DatOut$EnvYrs[e, 2] - DatOut$EnvYrs[e, 1] + 1
    DatOut[["NenvData"]] <- NenvData
    DatOut[["EnvData"]] <-
      get.df(dat, Loc, nrow = NenvData)
    names(DatOut[["EnvData"]]) <- c("Index", "Year", "Value")
  } else {
    DatOut[["EnvYrs"]] <- ""
    DatOut[["EnvData"]] <- ""
  }
  if (verbose)
    cat("-> Read environmental data. \n")
  # -------------------------------------------------------------------------

  # End of data file
  # -------------------------------------------------------------------------
  eof <- get.num(dat, Loc)

  if (eof != 9999) {
    cat("\n\nSomething went wrong while reading the data file !!\n")
    stop()
  }

  if (verbose) {
    cat("====================================================\n")
    cat("Read of data file complete. Final value = ", eof, "\n")
    cat("\n")
  }

  DatOut[["eof"]] <- FALSE
  if (eof == 9999)
    DatOut[["eof"]] <- TRUE

  return(DatOut)
}
