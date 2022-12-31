#' @title readGMACSsimdat
#'
#' @description Read the simdata.out file. This is an output of GMACS that holds
#' the predicted catch and cpue from an assessment run as well as the observed
#' and predicted size compositions.
#'
#' @param Dir - path to the "simulated" data file (output from GMACS)
#' @param FileName - name of the "simulated" data file - By default, "simdata.out"
#' @param verbose - (TRUE/FALSE); flag to print processing information
#' @param DatFile - Object (list) containing the .dat file - This is the output
#' of the [readGMACSdat()] function.
#' @param CtlFile - Object (list) containing the .ctl file - This is the output
#' of the [readGMACSctl()] function.
#'
#' @return the simdata.out file as a named list.
#'
#' @seealso \code{\link{readGMACS.dat}},\code{\link{readGMACSdat}},\code{\link{readGMACSctl}},
#' \code{\link{readGMACSprj}}, \code{\link{readGMACSpar}}
#'
#' @export
#' @md
#
readGMACSsimdat <- function(Dir = NULL,
                            FileName = "simdata.out",
                            verbose = TRUE,
                            DatFile = NULL,
                            CtlFile = NULL) {
  DatOut <- list()

  nCatchDF <- DatFile$N_CatchDF
  N_SizeFreq_df <- DatFile$N_SizeFreq_df
  Nrows_SiseFreqDF <- DatFile$Nrows_SiseFreqDF
  Nbins_SiseFreq <- DatFile$Nbins_SiseFreq
  iCompAggregator <- CtlFile$iCompAggregator

  # 1- Internal functions
  # -------------------------------------------------------------------------
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

  # 2- Read the simdata.out file and find the first line containing numeric data
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("-- Reading simulated data file \n")
    cat("====================================================")
    cat("\n")
  }

  FileName <- file.path(Dir, FileName)
  dat <- readLines(FileName, warn = FALSE)
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

  # Extract the predicted catch
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the predicted catch \n")

  pre_catch <- list()
  for (n in 1:nCatchDF) {
    pre_catch[[n]] <- get.vec(dat, Loc)
  }
  DatOut[["pre_catch"]] <- pre_catch

  if (verbose)
    cat("-> Read the predicted catch \n")
  # -------------------------------------------------------------------------

  # Extract the predicted relative abundance index
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the predicted relative abundance index \n")

  DatOut[["pre_cpue"]] <- get.vec(dat, Loc)

  if (verbose)
    cat("-> Read the predicted relative abundance index \n")
  # -------------------------------------------------------------------------

  # Extract the size composition
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the size composition \n")

  SizeDataS1 <- list()
  oldk <- 0

  # Extract column names from the size-composition data
  Nam <- names(DatFile$SizeFreq[[1]])
  Nam <- Nam[which(Nam != "")]
  Nam <- c(Nam,  "sim_X", "sim_Nsamp")

  for (n in 1:N_SizeFreq_df) {
    tmp <- NULL
    k <- iCompAggregator[n]
    if (oldk != k) {
      for (rows in 1:Nrows_SiseFreqDF[n]) {
        tmp <- rbind(tmp, get.vec(dat, Loc))
      }
      supNam <- rep("", dim(tmp)[2]-length(Nam))
      colnames(tmp) <- c(Nam, supNam)
    }
    SizeDataS1[[n]] <- tmp
    oldk <- k
  }
  SizeDataS1[sapply(SizeDataS1, is.null)] <- NULL
  DatOut[["SizeDataS1"]] <- SizeDataS1

  if (verbose)
    cat("-> Read the size composition \n")
  # -------------------------------------------------------------------------



  # Extract the size composition residuals
  # -------------------------------------------------------------------------
  if (verbose)
    cat("-- Reading the size composition residuals \n")

  SizeDataS2 <- list()

  for (n in 1:N_SizeFreq_df) {
    tmp <- NULL
    for (rows in 1:Nrows_SiseFreqDF[n]) {
      tmp <- rbind(tmp, get.vec(dat, Loc))
    }
    Nam <- paste0("SizeC_res_S", 1:Nbins_SiseFreq[n])
    colnames(tmp) <- Nam
    SizeDataS2[[n]] <- tmp
  }
  SizeDataS2[sapply(SizeDataS2, is.null)] <- NULL
  DatOut[["SizeDataS2"]] <- SizeDataS2

  if (verbose)
    cat("-> Read the size composition residuals \n")
  # -------------------------------------------------------------------------


  # End of data file
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("====================================================\n")
    cat("Read of simulated data file complete.")
    cat("\n")
  }
  return(DatOut)
}
