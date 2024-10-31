#' @title Write all the stock-specific Gmacs input files
#'
#' @description Function to write all the input files for a specific/multiple
#' stock(s) for a given version of Gmacs.
#'
#' @param stock (character vector)- names of the stock to consider. This matches
#' the name of stock-specific input file folder.
#' @param Stock_NameFiles (list)- named list (the names corresponding to the
#' \code{stock}) with the names for the new input files to write. See details for
#' more information about the structure of that list.
#' @param dir_InputFiles (path)- path to the folder containing the input files
#' for each stock.
#' @param dir_WriteFiles (path)- path to the repertory(ies) where to write the new
#' input files. This(ese) folder(s) hold(s) the stock-specific folder named as
#' in the \code{stock} argument. See the details section.
#' @param CatchDF_format (integer)- the format of the Catch data frame in the
#' 'model.dat' file. By default, the function considers the one specified in the
#' data file but this can be overwritten using this argument. Values: 0;1.
#' @param SurveyDF_format (integer)- the format of the survey data frame in the
#' 'model.dat' file. By default, the function considers the one specified in the
#' data file but this can be overwritten using this argument. Values: 0;1.
#' @param SizeFreqDF_format (integer)- the format of the size frequency data frame
#' in the 'model.dat' file. By default, the function considers the one specified
#' in the data file but this can be overwritten using this argument. Values: 0;1.
#' @param cleanup - flag to delete old input files in the \code{dir_InputFiles}
#' folder after writing the new ones.
#' @param dir_TPL (path)- path to the folder holding the "gmacs.tpl" file which
#' will be used to run the stock assessments with these input files.
#' @param Gmacs_Version (character)- the version number of Gmacs.
#' This has to be in the form \code{X.YY.ZZ}. If \code{NULL}, the \code{dir_TPL}
#' path has to be provided.
#' @param verbose (logical)- if \code{TRUE}, report the writing process (useful
#' for debugging).
#'
#' @details
#' The Stock_NameFiles list is a named list of length \code{length(stock)} and
#' holds for each stock the following object:
#' \describe{
#'  \item{\code{$datfileName_New}}{the name used to save the new 'model.dat' file.}
#'  \item{\code{$ctlfileName_New}}{the name used to save the new 'model.ctl' file.}
#'  \item{\code{$prjfileName_New}}{the name used to save the new 'model.prj' file.}
#'  \item{\code{$model_name}}{the name of the stock assessment model for the stock
#'  (e.g., 'model_16_0').}
#'  \item{\code{$Ass_year}}{the year in which these input files (in terms of model
#'  configuration, data,...) were used to assess the stock.}
#' }
#'
#' When multiple repertories are provided for the \code{dir_WriteFiles} argument,
#' the function will write the input files in each of these folders.
#'
#' @examples
#' \dontrun{
#' #' fsep <- .Platform$file.sep
#' # Load packages ----
#' library(gmr)
#' library(magrittr)
#'
#' # Name of the folder in the Testing_Version folder of the version being developed
#' # Gmacs_Ver <-"Gmacs_2_10_01"
#' Gmacs_Ver <-"Gmacs_2_10_01"
#'
#'
#' # Set directories ----
#' Dir_Dvpt_Vers <- file.path(here::here(), "Dvpt_Version", "build", fsep = fsep)
#' dir_test <- file.path(here::here(), "Testing_Versions", Gmacs_Ver)
#' dir_InputFiles <- file.path(dirname(dir_test), "Stock_Input_files", Gmacs_Ver, fsep = fsep)
#'
#' # Stock of interest input files names ----
#' # This also includes the model name (\code{model_name}) and the year of
#' # assessment (\code{Ass_year})
#' EAG <- list(
#'   datfileName_New = "EAG_21_1.dat",
#'   ctlfileName_New = "EAG_21_1.ctl",
#'   prjfileName_New = "EAG_21_1.prj",
#'   model_name = "model_21_1e",
#'   Ass_year = 2021
#' )
#' WAG <- list(
#'   datfileName_New = "WAG_21_1.dat",
#'   ctlfileName_New = "WAG_21_1.ctl",
#'   prjfileName_New = "WAG_21_1.prj",
#'   model_name = "model_21_1e",
#'   Ass_year = 2021
#' )
#' SNOW_crab <- list(
#'   datfileName_New = "snow_21.dat",
#'   ctlfileName_New = "snow_21.ctl",
#'   prjfileName_New = "snow_21.prj",
#'   model_name = "model_21_g",
#'   Ass_year = 2021
#' )
#' BBRKC <- list(
#'   datfileName_New = "bbrkc_21_1.dat",
#'   ctlfileName_New = "bbrkc_21_1.ctl",
#'   prjfileName_New = "bbrkc_21_1.prj",
#'   model_name = "model_21_1",
#'   Ass_year = 2021
#' )
#' SMBKC <- list(
#'   datfileName_New = "sm_22.dat",
#'   ctlfileName_New = "sm_22.ctl",
#'   prjfileName_New = "sm_22.prj",
#'   model_name = "model_16_0",
#'   Ass_year = 2021
#' )
#' # Make a list of these stock-specific lists
#' Stock_models <- list(
#'   EAG = EAG,
#'   WAG = WAG,
#'   SNOW_crab = SNOW_crab,
#'   BBRKC = BBRKC,
#'   SMBKC = SMBKC
#' )
#' write_Gmacs_InputFiles(
#'  stock = c("EAG", "WAG", "SMBKC", "BBRKC"),
#'  Stock_NameFiles = Stock_models,
#'  verbose = FALSE,
#'  dir_WriteFiles = NULL,
#'  CatchDF_format = NULL,
#'  SurveyDF_format = NULL,
#'  SizeFreqDF_format = NULL,
#'  cleanup = TRUE,
#'  dir_TPL = NULL,
#'  Gmacs_Version = "2.01.M.10",
#'  dir_InputFiles = dir_InputFiles
#' )
#' }
#'
#' @export
#' @md
write_Gmacs_InputFiles <- function(stock = NULL,
                                   Stock_NameFiles = NULL,
                                   dir_InputFiles = NULL,
                                   dir_WriteFiles = NULL,
                                   CatchDF_format = NULL,
                                   SurveyDF_format = NULL,
                                   SizeFreqDF_format = NULL,
                                   cleanup = NULL,
                                   dir_TPL = NULL,
                                   Gmacs_Version = NULL,
                                   verbose = NULL) {
  # Internal functions ----
  # Function to print warning over a loop
  war <- function(text) {
    suppressWarnings(w <- warning(text))
    message(w)
    return(text)
  } # End war function
  # --------------------------------------------------------------------
  # --------------------------------------------------------------------

  # Function for one stock
  write_Files <- function(i_stock = NULL,
                          Stock_NameFiles_IN = NULL,
                          dir_InputFiles_IN = NULL,
                          dir_WriteFiles_IN = NULL,
                          CatchDF_format_IN = NULL,
                          SurveyDF_format_IN = NULL,
                          SizeFreqDF_format_IN = NULL,
                          cleanup_IN = NULL,
                          dir_TPL_IN = NULL,
                          Gmacs_Ver = NULL,
                          verbose_IN = NULL) {
    fsep <- .Platform$file.sep

    cat("*** Writing Gmacs input files for:", i_stock, "***\n")

    i_NameFiles <- Stock_NameFiles_IN[[i_stock]]
    dir_Base <- dir_InputFiles_IN

    # gmacs.dat file ----
    # read gmacs.dat
    fileName <- "gmacs.dat"
    fileName <- file.path(dir_Base, i_stock, fileName, fsep = fsep)
    if (!file.exists(fileName)) {
      test1 <- paste(
        "The 'gmacs.dat' file does not exist in:\n",
        file.path(dir_Base, i_stock, fsep = fsep),
        "\n\t-> Can't read input files.\n\n"
      )
      stop(test1)
    } else {
      if (verbose_IN)
        cat("\n")
      GMACSdat <- readGMACS.dat(path = fileName, verbose = verbose_IN)
      gmacs_old <- GMACSdat
    }

    # Data file ----
    # Read the data file
    datFile <- file.path(dir_Base, i_stock, GMACSdat[["DatFileName"]], fsep = fsep)
    if (!file.exists(datFile)) {
      test2 <- paste0(
        "The 'model.dat' file (",
        GMACSdat[["DatFileName"]],
        ") does not exist in:\n",
        file.path(dir_Base, i_stock, fsep = fsep),
        "\n\t-> Can't write 'model.dat'.\n\n"
      )
      war(text = test2)
    } else {
      datFile <- readGMACSdat(FileName = datFile, verbose = verbose_IN)

      # Write the data file
      writeGmacsdatfile(
        Dir = file.path(dir_WriteFiles_IN, i_stock, fsep = fsep),
        FileName = i_NameFiles[["datfileName_New"]],
        overwrite = TRUE,
        DatFile = datFile,
        stock = i_stock,
        model_name = i_NameFiles[["model_name"]],
        Ass_Year = i_NameFiles[["Ass_year"]],
        DirTPL = dir_TPL_IN,
        Ver_number = Gmacs_Ver,
        CatchDF_format = ifelse(
          is.null(CatchDF_format_IN),
          yes = datFile[["CatchDF_format"]],
          no = CatchDF_format_IN
        ),
        SurveyDF_format = ifelse(
          is.null(SurveyDF_format_IN),
          yes = datFile[["SurveyDF_format"]],
          no = SurveyDF_format_IN
        ),
        SizeFreqDF_format = ifelse(
          is.null(SizeFreqDF_format_IN),
          yes = datFile[["SizeFreqDF_format"]],
          no = SizeFreqDF_format_IN
        )
      )
    }

    # Control file ----
    # Read the control file
    ctlFile <- file.path(dir_Base, i_stock, GMACSdat[["CtlFileName"]], fsep = fsep)
    if (!file.exists(ctlFile)) {
      test3 <- paste0(
        "The 'model.dat' file (",
        GMACSdat[["CtlFileName"]],
        ") does not exist in:\n",
        file.path(dir_Base, i_stock, fsep = fsep),
        "\n\t-> Can't write 'model.ctl'.\n\n"
      )
      war(text = test3)
    } else {
      ctlFile <- readGMACSctl(
        FileName = ctlFile,
        verbose = verbose_IN,
        DatFile = datFile,
        nyrRetro = GMACSdat[["N_Year_Retro"]]
      )

      # Write the control file
      writeGmacsctlfile(
        Dir = file.path(dir_WriteFiles_IN, i_stock, fsep = fsep),
        FileName = i_NameFiles[["ctlfileName_New"]],
        CtlFile = ctlFile,
        DatFile = datFile,
        stock = i_stock,
        model_name = i_NameFiles[["model_name"]],
        Ass_Year = i_NameFiles[["Ass_year"]],
        DirTPL = dir_TPL_IN,
        Ver_number = Gmacs_Ver,
        nyrRetro = GMACSdat[["N_Year_Retro"]]
      )
    }

    # Projection file ----
    # Read the projection file
    prjfile <- file.path(dir_Base, i_stock, GMACSdat[["PrjFileName"]], fsep = fsep)
    if (!file.exists(prjfile)) {
      test3 <- paste0(
        "The 'model.dat' file (",
        GMACSdat[["PrjFileName"]],
        ") does not exist in:\n",
        file.path(dir_Base, i_stock, fsep = fsep),
        "\n\t-> Can't write 'model.prj'.\n\n"
      )
      war(text = test3)
    } else{
      prjfile <-  readGMACSprj(FileName = prjfile, verbose = verbose_IN)

      # Write the projection file
      writeGmacsprjfile(
        Dir = file.path(dir_WriteFiles_IN, i_stock, fsep = fsep),
        FileName = i_NameFiles[["prjfileName_New"]],
        PrjFile = prjfile,
        stock = i_stock,
        model_name = i_NameFiles[["model_name"]],
        Ass_Year = i_NameFiles[["Ass_year"]],
        DirTPL = dir_TPL_IN,
        Ver_number = Gmacs_Ver
      )
    }

    # Write the new gmacs.dat file
    if (file.exists(file.path(dir_WriteFiles_IN, i_stock, "gmacs_old.dat", fsep = fsep))) {
      # Remove the old gmacs_old.dat
      unlink(file.path(dir_WriteFiles_IN, i_stock, "gmacs_old.dat", fsep = fsep))
    }
    # Rename the current gmacs.dat as a old file
    file.rename(
      from = file.path(dir_WriteFiles_IN, i_stock, "gmacs.dat", fsep = fsep),
      to = file.path(dir_WriteFiles_IN, i_stock, "gmacs_old.dat", fsep = fsep)
    )
    # Update the name of the input files
    GMACSdat$DatFileName <- i_NameFiles[["datfileName_New"]]
    GMACSdat$CtlFileName <- i_NameFiles[["ctlfileName_New"]]
    GMACSdat$PrjFileName <- i_NameFiles[["prjfileName_New"]]
    # Write the new file
    writeGmacs.dat(
      Dir = file.path(dir_WriteFiles_IN, i_stock, fsep = fsep),
      FileName = "gmacs.dat",
      gmacsDat = GMACSdat,
      stock = i_stock,
      model_name = i_NameFiles[["model_name"]],
      Ass_Year = i_NameFiles[["Ass_year"]],
      DirTPL = dir_TPL_IN,
      Ver_number = Gmacs_Ver
    )

    if (cleanup_IN) {
      To_del <- c("gmacs_old.dat", gmacs_old[["DatFileName"]], gmacs_old[["CtlFileName"]], gmacs_old[["PrjFileName"]])
      # Check that we don't delete the new files if the function is ran multiple times
      To_Keep <- c(GMACSdat[["DatFileName"]], GMACSdat[["CtlFileName"]], GMACSdat[["PrjFileName"]])
      if (any(To_Keep %in% To_del)) {
        To_del <- To_del[!(To_del %in% To_Keep)]
      }
      cat(
        "\t--> In \n",
        file.path(dir_WriteFiles_IN, i_stock, fsep = fsep),
        "\n Deleting the following files:\n",
        paste("\t-", To_del, collapse = "\n")
      )

      res <- NULL
      for (f in To_del) {
        res[[f]] <- 1
        while (res[[f]]==1) {
          res[[f]] <- unlink(
            file.path(dir_WriteFiles_IN, i_stock, f, fsep = fsep),
            recursive = TRUE,
            force = TRUE,
          )
        } # End while condition
      } # End loop on To_del
    } # End cleanup_IN
    # return(paste0("\t--> ", stock, ": All the input files have been updated"))
    cat("\n\n\t--> ",
        i_stock,
        ":\n\t All the input files have been updated.\n\n")
  } # End write_Files function
  # --------------------------------------------------------------------
  # --------------------------------------------------------------------

  if (is.null(dir_WriteFiles))
    dir_WriteFiles <- dir_InputFiles

  # Run the write file funtion for each dir_WriteFiles
  for (d in 1:length(dir_WriteFiles)) {
    cat("\n-- In:", dir_WriteFiles[d], "\n\n")

    # Apply the write Files function on all stocks
    sapply(
      X = stock,
      FUN = write_Files,
      Stock_NameFiles_IN = Stock_NameFiles,
      dir_InputFiles_IN = dir_InputFiles,
      dir_WriteFiles_IN = dir_WriteFiles[d],
      CatchDF_format_IN = CatchDF_format,
      SurveyDF_format_IN = SurveyDF_format,
      SizeFreqDF_format_IN = SizeFreqDF_format,
      cleanup_IN = cleanup,
      dir_TPL_IN = dir_TPL,
      Gmacs_Ver = Gmacs_Version,
      verbose_IN = verbose
    )
  }
} # End write_Gmacs_InputFiles
