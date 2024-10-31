#' @title Set up structure folder for new code version of Gmacs
#'
#' @description Function to build the folder structure when updating/upgrading
#' Gmacs and prepare input files to run Gmacs. This function is to be used by
#' developers.
#'
#' @param stock (character; vector)- The stock(s) to be considered. This specifies
#' the input file folder names that will be build in the \code{dir_test} repertory.
#' If set as \code{"all"}, then the function will look in either the
#' \code{dir_LastAss_Files} or the \code{dir_Version_Files} repertories to identify
#' which stock(s) to consider.
#' @param ADMBpaths (filepath): absolute or relative to current working directory
#' path to file defining required ADMB paths. See the details section for more
#' information about this `ADMB file`.
#' @param dir_test (path)- path to the repertory where the codes and stocks folder
#' will be saved during the development of this new version. This folder will be
#' used to run tests when updating/upgrading Gmacs.
#' @param dir_old_version (path)- path to the repertory where the latest version
#' of Gmacs is saved. This repertory has to hold the last codes available for Gmacs
#' as well as the outptut of Gmacs for each \code{stock} obtained using this version.
#' The names of the \code{stock} folders in that repertory have to match the ones
#' in the \code{stock} argument.
#' @param use_LastAss_Files (logical)- If \code{TRUE}, the last available
#' assessment input files are used. See details for more information.
#' @param dir_LastAss_Files (path)- path to the repertory holding the last
#' available input files for each stock.
#' @param use_Version_Files (logical)- If \code{TRUE}, the input files for a given
#' version of Gmacs are used.
#' @param dir_Version_Files (path)- path to the repertory holding the input files
#' for each stock that have been used for an older version of Gmacs. See details
#' for more information.
#' @param new_Ver_ID (character)- the version number of Gmacs for this update/upgrade.
#' This has to be in the form \code{X.YY.ZZ}. If \code{NULL}, a window will pop
#' up and ask the user to specify this version number.
#' @param verbose (logical)- if \code{TRUE}, report the building process.
#'
#' @details
#' The \code{dir_test} repertory will hold the following folders:
#' \describe{
#'  \item{\code{Gmacs_XX.YY.ZZ}}{a Gmacs version specific folder that is used to
#'  store the outupt of Gmacs for the current version in development. The name
#'  gave to the version being developed (\code{new_Ver_ID}) has to match the one
#'  that will be given when updating/upgrading Gmacs.}
#'  \item{\code{Stock_Input_files}}{a folder subdived in multiple subfolders
#'  corresponding to each version of Gmacs that is developped. Each of these
#'  subfolders holds the stock-specific input files that are used for the
#'  development.}
#' }
#'
#' The functions allows to use the stock-specific input files available
#' in the \code{dir_LastAss_Files} folder so you are sure to work with the last
#' available input files for each stock. This can be avoid by i) using input files
#' from a previous developed version of Gmacs (see the \code{use_Version_Files}
#' argument) or ii) providing your own input files in the \code{Stock_Input_files}
#' folder.
#'
#' The \code{ADMBpaths} refers to the repertory that holds either the
#' "ADpaths_Windows.txt" file for the windows machine or the "ADpaths_MacOS.txt"
#' file for Mac. The function automatically detect which machine you are working
#' on.
#'
#' @return Set up the folder structure to develop a new version of Gmacs and provide
#' the stock specific input files that will be used to develop the new version of
#' Gmacs.
#'
#' @author M. Veron
#'
#' @examples
#' \dontrun{
#' rm(list = ls(all.names = TRUE))
#'
#' # 1. set up ----
#'
#' fsep <- .Platform$file.sep
#'
#' # Load packages ----
#' library(gmr)
#' library(magrittr)
#'
#' # Set directories ----
#' dir_test <- file.path(here::here(), "Testing_Versions")
#' dir_old_version <- file.path(here::here(), "Dvpt_Version", fsep = fsep)
#' dir_LastAss_Files <- file.path(dirname(here::here()), "Assessment_data", fsep = fsep)
#' dir_Version_Files <- NULL
#' dir_ADMBpath_Files <- here::here()
#'
#'
#' # 2. local functions ----
#'
#' # Specification for building the structure
#' stock <-  "all"
#' use_LastAss_Files <- TRUE
#' use_Version_Files <- FALSE
#' new_Ver_ID <- NULL
#' verbose <- TRUE
#'
#' # Run the function to set up the folder structure ----
#' set_DevStruct(stock = stock,
#'               ADMBpaths = dir_ADMBpath_Files,
#'               dir_test = dir_test,
#'               dir_old_version = dir_old_version,
#'               use_LastAss_Files = use_LastAss_Files,
#'               dir_LastAss_Files = dir_LastAss_Files,
#'               use_Version_Files = use_Version_Files,
#'               dir_Version_Files = dir_Version_Files,
#'               new_Ver_ID = new_Ver_ID,
#'               verbose = verbose
#' )
#' }
#'
#' @export
#' @md
set_DevStruct <- function(stock = NULL,
                          ADMBpaths = NULL,
                          dir_test = NULL,
                          dir_old_version = NULL,
                          use_LastAss_Files = NULL,
                          dir_LastAss_Files = NULL,
                          use_Version_Files = NULL,
                          dir_Version_Files = NULL,
                          new_Ver_ID = NULL,
                          verbose = NULL) {
  # Internal functions ----

  # Get the name of the new version to create the associated folder
  getVerGMACS <- function (GmacsVer = NULL) {
    New.ver <- NA
    while (is.na(New.ver)) {
      text = "You are implementing a new version of Gmcas (updating/Upgrading).
      The name of the new version has to be of the form to: 'X.YY.ZZ'.
      The last updated version number is indicated in the dialog box - Please update the version number."
      # New.ver <- svDialogs::dlgInput(message = text, Sys.info())$res
      New.ver <- svDialogs::dlgInput(message = text, default = print(GmacsVer))$res
      Sys.sleep(0.1)
    }
    return(New.ver)
  }
  # ----------------------------------------------------------------
  # ----------------------------------------------------------------

  fsep <- .Platform$file.sep

  # Get the GMACS version name for the new version ----
  tmp <- GMACSversion(Dir = dir_old_version)
  old_Ver <- tmp$ver
  old_Ver <- stringr::str_squish(stringr::str_remove(string = old_Ver, pattern = "GMACS Version"))
  if (!is.null(new_Ver_ID)) {
    new_Ver <- new_Ver_ID
  } else {
    new_Ver <- getVerGMACS(GmacsVer = old_Ver)
  }
  Folder_Ver <- stringr::str_replace_all(string = new_Ver,
                                         pattern = "[[.]]",
                                         replacement = "_")
  Folder_Ver <- paste0("Gmacs_", Folder_Ver)

  # Create Repertories if applicable ----
  # Repertory for Gmacs CODES and model runs
  dir_Model_test <- file.path(dir_test, Folder_Ver, fsep = fsep)
  if (!dir.exists(dir_Model_test)) {
    dir.create(dir_Model_test, recursive = TRUE)
    cat(paste0(
      "-- The following repertory has been created:\n\t",
      dir_Model_test,
      "\n"
    ))
  }
  subdir <- c("include", "lib")
  if (!any(dir.exists(file.path(dir_Model_test, subdir, fsep = fsep)))) {
    cat("-- The following subfolder(s) are created in the repertoy:\n",
        dir_Model_test,
        "\n")
    for (sbd in subdir) {
      if (!dir.exists(file.path(dir_Model_test, sbd, fsep = fsep))) {
        dir.create(file.path(dir_Model_test, sbd, fsep = fsep))
        cat(paste0("\t- ", sbd, "\n"))
      }
    }
  }

  # Repertory for Stock input files
  # Check if the test have to be ran over all stocks
  if (length(stock) == 1 && stock == "all") {
    if (use_LastAss_Files) {
      stock <- list.files(dir_LastAss_Files)
      stock <- stock[(tools::file_ext(list.files(dir_LastAss_Files)) == "")]
    } else if (use_Version_Files) {
      stock <- list.files(dir_Version_Files)
    }
  }
  # Create the version specific subfolder in the Stock_Input_files folder
  dir_InFiles_test <- file.path(dir_test, "Stock_Input_files", Folder_Ver, fsep = fsep)
  if (!dir.exists(dir_InFiles_test)) {
    dir.create(dir_InFiles_test, recursive = TRUE)
    cat(paste0(
      "-- The following repertory has been created:\n\t",
      dir_InFiles_test,
      "\n"
    ))
  }
  # Create the stock specific subfolder in the Gmacs version folder
  if (any(!dir.exists(file.path(dir_InFiles_test, stock, fsep = fsep)))) {
    cat(
      "-- The following subfolder(s) are created in the repertoy:\n",
      dir_InFiles_test,
      "\n"
    )
    for (nam in stock) {
      if (!dir.exists(file.path(dir_InFiles_test, nam, fsep = fsep))) {
        dir.create(file.path(dir_InFiles_test, nam, fsep = fsep))
        cat(paste0("\t- ", nam, "\n"))
      }
    }
  }

  # Check if input files are available ----
  # the function looks in either the dir_LastAss_Files or dir_Version_Files
  if (use_LastAss_Files) {
    # check if the folder for each stock exist in the Assessment_data
    if (!any(dir.exists(file.path(dir_LastAss_Files, stock, fsep = fsep)))) {
      No_dir <- which(!dir.exists(file.path(dir_LastAss_Files, stock, fsep = fsep)))
      No_dir <- file.path(dir_LastAss_Files, stock, fsep = fsep)[No_dir]
      test0 <- paste(
        "The following repertory/ies: \n",
        paste("\t-", No_dir, collapse = "\n"),
        "\nis/are missing in the \n",
        dir_LastAss_Files,
        "\n\n\t=> If input files are also missing in the Gmacs version specific folder, tests won't be run for this/ese stock(s)"
      )
      warning(test0)
      # Check if the files are available in the Gmacs version specific folser
      No_dir <- basename(No_dir)
      if (!any(dir.exists(file.path(dir_InFiles_test, No_dir, fsep = fsep)))) {
        No_dir <- No_dir[which(!dir.exists(file.path(dir_InFiles_test, No_dir, fsep = fsep)))]
        cat(
          "Tests won't be ran for the following stock(s):\n",
          paste("\t-", No_dir, collapse = "\n")
        )
        stock <- stock[!stock %in% No_dir]
      }
    }
  } else if (use_Version_Files) {
    # check if the folder for each stock exist in a old Gmacs version folder
    if (!any(dir.exists(file.path(dir_Version_Files, stock, fsep = fsep)))) {
      No_dir <- which(!dir.exists(file.path(dir_Version_Files, stock, fsep = fsep)))
      No_dir <- file.path(dir_Version_Files, stock, fsep = fsep)[No_dir]
      test0 <- paste(
        "The following repertory/ies: \n",
        paste("\t-", No_dir, collapse = "\n"),
        "\nis/are missing in the \n",
        dir_Version_Files,
        "\n\n\t=> If input files are also missing in the Gmacs version specific folder, tests won't be run for this/ese stock(s)"
      )
      warning(test0)
      # Check if the files are available in the Gmacs version specific folser
      No_dir <- basename(No_dir)
      if (!any(dir.exists(file.path(dir_InFiles_test, No_dir, fsep = fsep)))) {
        No_dir <- No_dir[which(!dir.exists(file.path(dir_InFiles_test, No_dir, fsep = fsep)))]
        cat(
          "Tests won't be ran for the following stock(s:\n",
          paste("\t-", No_dir, collapse = "\n")
        )
        stock <- stock[!stock %in% No_dir]
      }
    }
  }

  # Copy the Gmacs CODES from the last version ----
  # Copy the ADMB paths file
  ADMBfiles <- ifelse(isWindowsOS(), "ADpaths_Windows.txt", "ADpaths_MacOS.txt")
  cop0 <- file.copy(
    from = file.path(ADMBpaths, ADMBfiles, fsep = fsep),
    to = file.path(dir_Model_test, ADMBfiles, fsep = fsep),
    overwrite = TRUE,
    copy.date = TRUE
  )
  if (!cop0) {
    ADMB_files_stp <- paste("Error: the ADMB path file was not copied to:\n",
                            dir_Model_test,
                            "\n")
    stop(ADMB_files_stp)
  }
  # Copy gmacs code files to dir_Model_test
  gmacs_files <- c("gmacsbase.tpl", "personal.tpl")
  cop1 <- file.copy(
    from = file.path(dir_old_version, gmacs_files, fsep = fsep),
    to = file.path(dir_Model_test, gmacs_files, fsep = fsep),
    overwrite = TRUE,
    # recursive = TRUE,
    copy.date = TRUE
  )
  if (any(!cop1)) {
    gmacs_files_stp <- paste(
      "Error: the following files were not copied to:\n",
      dir_Model_test,
      "\n\n",
      paste(
        "\t-",
        file.path(dir_old_version, gmacs_files, fsep = fsep)[!cop1],
        collapse = "\n"
      )
    )
    stop(gmacs_files_stp)
  }
  # Set the header with a development flag in the gmacs.tpl file
  gmacsbase <- file.path(dir_Model_test, "gmacsbase.tpl", fsep = fsep)
  # Write gmacsbase.tpl
  add.text <- readLines(gmacsbase)
  unlink(gmacsbase, recursive = FALSE, force = FALSE)
  fs::file_create(gmacsbase)
  add.text <- insertTime(
    object = add.text,
    pattern = ' !! TheHeader',
    .update = FALSE,
    devel = TRUE,
    New_ver = new_Ver
  )
  add.text <- paste0(add.text, collapse = "\n")
  add.text <- c(add.text, "\n", "")
  fileConn <- file(gmacsbase)
  writeLines(text = add.text, fileConn)
  close(fileConn)

  # Copy the files from include
  dir_include <- file.path(dir_old_version, "include", fsep = fsep)
  include_files <- list.files(dir_include)
  cop2 <- file.copy(
    from = file.path(dir_include, include_files, fsep = fsep),
    to = file.path(dir_Model_test, "include", include_files, fsep = fsep),
    overwrite = TRUE,
    # recursive = TRUE,
    copy.date = TRUE
  )
  if (any(!cop2)) {
    include_files_stp <- paste(
      "Error: the following files were not copied to:\n",
      file.path(dir_Model_test, "include", fsep = fsep),
      "\n\n",
      paste(
        "\t-",
        file.path(dir_Model_test, "include", include_files, fsep = fsep)[!cop2],
        collapse = "\n"
      )
    )
    stop(include_files_stp)
  }
  # Copy files from lib
  dir_lib <- file.path(dir_old_version, "lib", fsep = fsep)
  libFiles <- dir(
    path = file.path(dir_old_version, "lib", fsep = fsep),
    pattern = "*.cpp",
    ignore.case = TRUE,
    all.files = TRUE,
    full.names = FALSE
  )
  cop3 <- file.copy(
    from = file.path(dir_lib, libFiles, fsep = fsep),
    to = file.path(dir_Model_test, "lib", libFiles, fsep = fsep),
    overwrite = TRUE,
    copy.date = TRUE
  )
  if (any(!cop3)) {
    include_files_stp <- paste(
      "Error: the following files were not copied to:\n",
      file.path(dir_Model_test, "lib", fsep = fsep),
      "\n\n",
      paste(
        "\t-",
        file.path(dir_Model_test, "lib", libFiles, fsep = fsep)[!cop3],
        collapse = "\n"
      )
    )
    stop(include_files_stp)
  }

  # Copy the input files ----
  if (use_LastAss_Files || use_Version_Files) {
    # Which input files are used as a basis?
    dir_base_file <- ifelse(use_LastAss_Files, yes = dir_LastAss_Files, no = dir_Version_Files)

    # Copy and paste the input files from the dir_LastAss_Files to the dir_InFiles_test
    for (nam in stock) {
      cat(
        "--> Currently coping the input files for",
        nam,
        "fom:\n\t",
        dir_base_file,
        "\n\t to:\n\t",
        file.path(dir_InFiles_test, nam, fsep = fsep),
        "\n\n"
      )

      # Read in the Gmacs input files
      Stock_files <- getInpOutFiles(Dir =
                                      file.path(dir_base_file, nam, fsep = fsep),
                                    verbose = FALSE)
      # Get info from files
      model_name <- Stock_files$datFile$Comments[which(
        stringr::str_detect(
          string = Stock_files$datFile$Comments,
          pattern = "#_Model name:"
        )
      )]
      model_name <-
        stringr::str_squish(stringr::str_remove(string = model_name, pattern = "#_Model name:"))
      Ass_Year <- Stock_files$datFile$Comments[which(
        stringr::str_detect(
          string = Stock_files$datFile$Comments,
          pattern = "#_Year of assessment:"
        )
      )]
      Ass_Year <-
        stringr::str_squish(stringr::str_remove(string = Ass_Year, pattern = "#_Year of assessment:"))

      # Write the input files modifying the info
      # Write the data file
      writeGmacsdatfile(
        Dir = file.path(dir_InFiles_test, nam, fsep = fsep),
        FileName = Stock_files$GMACSdat$DatFileName,
        overwrite = TRUE,
        DatFile = Stock_files$datFile,
        stock = nam,
        Ver_number = new_Ver,
        model_name = model_name,
        Ass_Year = Ass_Year,
        CatchDF_format = Stock_files$datFile$CatchDF_format,
        SurveyDF_format = Stock_files$datFile$SurveyDF_format,
        SizeFreqDF_format = Stock_files$datFile$SizeFreqDF_format
      )
      # Write the control file
      writeGmacsctlfile(
        Dir = file.path(dir_InFiles_test, nam, fsep = fsep),
        FileName = Stock_files$GMACSdat$CtlFileName,
        CtlFile = Stock_files$ctlFile,
        DatFile = Stock_files$datFile,
        stock = nam,
        model_name = model_name,
        Ass_Year = Ass_Year,
        Ver_number = new_Ver,
        nyrRetro = Stock_files$GMACSdat$N_Year_Retro
      )
      # Write the projection file
      writeGmacsprjfile(
        Dir = file.path(dir_InFiles_test, nam, fsep = fsep),
        FileName = Stock_files$GMACSdat$PrjFileName,
        PrjFile = Stock_files$prjfile,
        stock = nam,
        model_name = model_name,
        Ass_Year = Ass_Year,
        Ver_number = new_Ver
      )
      # Write the gmacs.dat file
      writeGmacs.dat(
        Dir = file.path(dir_InFiles_test, nam, fsep = fsep),
        FileName = "gmacs.dat",
        gmacsDat = Stock_files$GMACSdat,
        stock = nam,
        model_name = model_name,
        Ass_Year = Ass_Year,
        Ver_number = new_Ver
      )
    }
  } # End use_LastAss_Files || use_Version_Files
  cat(
    "!! The folder structure for the Gmacs development version",
    new_Ver,
    "has been created !!\n\n"
  )
} # End set_DevStruct
