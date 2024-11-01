#' @title UpdateGMACS
#'
#' @description Function used when updating and releasing a new version of GMACS.
#' It allows to copy and paste all the GMACS files (for one or several stocks)
#' resulting from the Development version from this directory to the Latest_Version
#' directory.
#'
#' @param dirSrc (path)- This is the path to the folder that has been
#' used to develop the new version of GMACS. Typically, this corresponds to the path
#' to the \code{Testing_Versions} folder, if following the basic workflow to
#' update/upgrade GMACS.
#' @param dirNew (path)- This is the path to the folder that will hold
#' the latest version of GMACS after the check steps. Typically, this corresponds
#' to the path to the \code{Latest_Version} folder, if following the basic workflow
#' to update/upgrade GMACS.
#' @param dir_Assdata (path)- This is the path to the folder holding the assessment
#' files for each stock. If following the basic workflow to update/upgrade GMACS,
#' this is the \code{Assessment_data} fodler, where the stock-specific input files
#' used to run the last version of Gmacs are saved.
#' @param dir_supp (path; vector)- Optional. This argument can be used to copy both
#' the Gmacs codes and the stock-specific input files in to one/multiple directories
#' (different from \code{dirNew} and \code{dir_Assdata}, if applicable).
#' @param ADMBpaths (filepath): path to the repertory holding the file defining
#' required ADMB paths. By default (\code{ADMBpaths = NULL}), the function will
#' look for that file in the relative folder of the \code{dirNew} repertory. See
#' the details section for more information about this `ADMB files`.
#' @param UpdateInputFiles (logical)- Flag to re-write all the input files in
#' order to update the Gmacs version name and the date of last compilation. By
#' default, \code{UpdateInputFiles = TRUE}. You need to fill in the
#' \code{Stock_models_names} list to provide the names of the files, the model
#' name and the year of assessment.
#' @param Stock_models_names (list)- named list providing the stock-specific input
#' files names, the name of the model and the year of the assessment for which these
#' are used for. See the [write_Gmacs_InputFiles()] documentation for more details.
#' @param Update_LastAss_file (logical)- If true, the stock-specific input files
#' will be copy to the \code{dir_Assdata}. Since this folder is intended to hold
#' the last available input file for each stock, this argument is set as
#' \code{Update_LastAss_file = TRUE} by default.
#' @param UpdateLast_GmacsVer (logical)- Because the development version is ran
#' using a .pin file and no estimation, this flag allows to update the last
#' version folder (\code{dirNew}) by runing Gmacs with estimation. Default,
#' \code{UpdateLast_GmacsVer = TRUE}
#' @param stock (character string; vector)- Stock input file folder names to be
#' copied. If set as \code{"all"} (default), then the function will consider the names of
#' the stock in the \code{dirNew} repertory (under the folder \code{build}).
#' @param verbose (logical)- flag to print processing information. Default,
#' \code{verbose = FALSE}.
#' @param cleanStockFolder (logical)- flag to delete stock-specific subfolders in
#' the \code{dirSrc} folder.
#' @param cleanInputFiles (logical)- flag to delete the Gmacs version specific
#' input files folder in the relative repertory of the \code{dirSrc} folder.
#' @param cleanTest (logical)- floag to delete the \code{dirSrc} folder.
#'
#' @details
#' All in the stock-specific input files are updated with the Gmacs version numder.
#' The .TPL file is also updated with the specification of the implementations
#' made during the developement of this new version of Gmacs.
#'
#' if a \code{build} folder exists in the directory(ies) where the
#' stock assessment files have to be copied, then it will be used as the folder
#' to save the output of the stock-specific runs.
#'
#' The \code{ADMBpaths} refers to the repertory that holds either the
#' "ADpaths_Windows.txt" file for the windows machine or the "ADpaths_MacOS.txt"
#' file for Mac. The function automatically detect which machine you are working
#' on.
#'
#' @return Copy both the Gmacs codes and the stock-specific input files from the
#' development folder to:
#' \describe{
#'  \item{\code{dirNew}}{the folder holding the last available/stable version of
#'  Gmacs;}
#'  \item{\code{dir_Assdata}}{the folder holding the last available stock-specific
#'  input files in terms of Gmacs version (Gmacs code are not copied in to that
#'  folder);}
#' }
#' The function creates the \code{GMACS_Version_details.txt} file that gives
#' details about the version of GMACS that is updating, including the last
#' implementations.
#'
#' @export
#' @md
#
UpdateGMACS <- function(dirSrc = NULL,
                        dirNew = NULL,
                        dir_Assdata = NULL,
                        dir_Supp = NULL,
                        ADMBpaths = NULL,
                        UpdateInputFiles = TRUE,
                        Stock_models_names = NULL,
                        Update_LastAss_file = TRUE,
                        UpdateLast_GmacsVer = TRUE,
                        stock = "all",
                        verbose = FALSE,
                        cleanStockFolder = FALSE,
                        cleanInputFiles = FALSE,
                        cleanTest = FALSE
) {
  # Internal function ----

  # Change the header to update/upgrade Gmacs
  changeHeader <- function(inFile) {
    text <- readLines(inFile)
    # Get the number version ----
    header <-
      which(stringr::str_detect(text, pattern = " !! TheHeader"))
    Vers <- text[header]

    # Change the header in inFile if applicable
    if (stringr::str_detect(string = Vers, pattern = "DEVELOPMENT VERSION")) {
      unlink(inFile, recursive = FALSE, force = FALSE)
      fs::file_create(inFile)

      Vers <- gsub(pattern = "## GMACS DEVELOPMENT VERSION ",
                   replacement = "## GMACS Version ",
                   x = Vers)
      Vers <- gsub(pattern = "Initialized",
                   replacement = "Compiled",
                   x = Vers)
      text[header] <- Vers

      add.text <- paste0(text, collapse = "\n")
      add.text <- c(add.text, "\n", "")
      fileConn <- file(inFile)
      writeLines(text = add.text, fileConn)
      close(fileConn)
    }
  }
  # --------------------------------------------------------------------

  # Function to print warning over a loop
  war <- function(text) {
    suppressWarnings(w <- warning(text))
    message(w)
    # return(text)
  } # End war function
  # --------------------------------------------------------------------
  # --------------------------------------------------------------------

  fsep <- .Platform$file.sep

  # 1. Specify the new implementation in the gmacsbase.tpl file ----
  # Check if the changes constitute an update or an upgrade of the version
  # Read the detaile from the latest version
  GMACSdetails <- file.path(
    ifelse(basename(dirNew) == "build", yes = dirname(dirNew), dirNew),
    "GMACS_Version_details.txt",
    fsep = fsep
  )
  text <- readLines(GMACSdetails)
  oldVer <- text[stringr::str_detect(string = text, pattern = "GMACS version")]
  oldVer <- stringr::str_squish(stringr::str_remove(string = stringr::str_squish(oldVer), pattern = "- GMACS version:"))[1]

  # Update the header in gmasbase.tpl and
  # Add the specifications of the new implementations
  gmacsbase <- file.path(dirSrc, "gmacsbase.tpl", fsep = fsep)
  changeHeader(gmacsbase)
  # Get the version of this new version
  text <- readLines(gmacsbase)
  # Get the number version ----
  header <-
    which(stringr::str_detect(text, pattern = " !! TheHeader"))
  Vers <- text[header]
  Vers <-
    sub(
      pattern = stringr::str_squish('!! TheHeader = adstring(\"## GMACS Version'),
      replacement = "",
      x = Vers,
      fixed = TRUE
    )
  Vers <- stringr::str_split(string = Vers, pattern = ';')[[1]][1]
  Vers <- stringr::str_squish(Vers)
  if (oldVer == Vers) {
    updateVer <- TRUE
  } else {
    updateVer <- FALSE
  }
  NewGMACSFeat(dirSrc, updateGMACS = updateVer)

  # Re write the gmacs.tpl to incoporate these specifications
  write_TPL(
    vv = 1,
    Dir = dirSrc,
    .update = FALSE,
    End_Devel = TRUE
  )

  # 2. Copy all files from the dirSrc to the Latest_Version ----
  # Copy Gmacs code files
  cop.files <- dir(
    path = dirSrc,
    pattern = c("*.tpl"),
    ignore.case = TRUE,
    all.files = TRUE,
    full.names = FALSE
  )
  gmacs_exe <- ifelse(isWindowsOS(), "gmacs.exe", "gmacs")
  cop.files <- c(cop.files, gmacs_exe, "lib", "include")
  test1 <- file.copy(
    from = file.path(dirSrc, cop.files, fsep = fsep),
    to = ifelse(basename(dirNew) == "build", yes = dirname(dirNew), dirNew),
    overwrite = TRUE,
    recursive = TRUE,
    copy.date = TRUE
  )
  if (any(!test1)) {
    gmacs_stp = paste(
      "Error: the following files/folder were not copied to",
      ifelse(basename(dirNew) == "build", yes = dirname(dirNew), dirNew),
      "\n",
      paste(file.path(dirSrc, c(cop.files), fsep = fsep)[!test1], collapse =
              "\n\t-")
    )
    stop(gmacs_stp)
  }

  # Build the description file of this new version in both the dirSrc folder and
  # the latest_Version folder
  GetVerSpec(Dir = ifelse(basename(dirNew) == "build", yes = dirname(dirNew), dirNew))
  GetVerSpec(Dir = dirSrc)

  # Get the names of the stocks in the in the latest_version folder
  if (stock == "all") {
    StockNames <- list.files(ifelse(
      basename(dirNew) == "build",
      yes = dirNew,
      no = file.path(dirNew, "build", fsep = fsep)
    ))
    StockNames <- StockNames[!StockNames %in% c("debug", "release")]
  } else {
    StockNames <- stock
  }

  # Re write the input file to update the version and date of compilation of Gmacs
  if(UpdateInputFiles){
    if(is.null(Stock_models_names)){
      stp1 <- paste0("The named list giving the information about the model input file names is missing. The input files won't be updated")
      war(text = stp1)
    } else {
      if(any(!StockNames %in% names(Stock_models_names))){
        res1 <- StockNames[which(!StockNames %in% names(Stock_models_names))]
        StockNamesUpdate <- StockNames[!StockNames %in% res1]
        stp2 <- paste(
          "The information for :\n", paste("\t-",res1,collapse = "\n"),
          "\nare missing in the 'Stock_models_names' list.\n\t--> Their input files will not be updated."
        )
        war(text = stp2)
      } else {
        StockNamesUpdate <- StockNames
      }
      dir_InputFiles <- file.path(dirname(dirSrc), "Stock_Input_files", basename(dirSrc), fsep = fsep)
      write_Gmacs_InputFiles(
        stock = StockNamesUpdate,
        Stock_NameFiles = Stock_models_names,
        verbose = FALSE,
        dir_InputFiles = dirSrc,
        dir_WriteFiles = c(dir_InputFiles,dirSrc),
        CatchDF_format = NULL,
        SurveyDF_format = NULL,
        SizeFreqDF_format = NULL,
        cleanup = TRUE,
        dir_TPL = dirSrc,
        Gmacs_Version = NULL
      )
    }
  } # End UpdateInputFiles

  # Clean repertories and Copy files ----
  dir_new <- ifelse(
    basename(dirNew) == "build",
    yes = dirNew,
    no = file.path(dirNew, "build", fsep = fsep)
  )
  for (nm in 1:length(StockNames)) {
    # Clean the Latest_Version directory
    tmpDirStock <- file.path(dir_new, StockNames[nm], fsep = fsep)
    #   # Clean gmacs output files
    # clean_bat(path = tmpDirStock, verbose = FALSE)
    #   # Clean the old Gmacs input files
    # clean_Inputfiles(path = tmpDirStock, verbose = FALSE)
    # Clean all files
    file_del <- list.files(tmpDirStock)[!stringr::str_detect(string = list.files(tmpDirStock),
                                                             pattern = paste(c('plots', 'clean'), collapse =
                                                                               "|"))]
    clean_files(path = tmpDirStock,
                names = file_del,
                verbose = verbose)
    if (dir.exists(file.path(tmpDirStock, "plots", fsep = fsep))) {
      file_del <- list.files(file.path(tmpDirStock, "plots", fsep = fsep))
      clean_files(
        path = file.path(tmpDirStock, "plots", fsep = fsep),
        names = file_del,
        verbose = verbose
      )
    }

    # Clean the dirSrc directory
    dirSrcStock <- file.path(dirSrc, StockNames[nm], fsep = fsep)
    clean_bat(path = dirSrcStock, verbose = verbose)

    # Get the names of the available files in the dirSrcStock repertory
    InputFiles <- list.files(path = dirSrcStock, full.names = TRUE)
    # Copy the stock-specific outputs to the latest_version folder
    res1 <- suppressWarnings(
      file.copy(
        from = InputFiles,
        to = file.path(dir_new, StockNames[nm], fsep = fsep),
        overwrite = TRUE,
        recursive = TRUE,
        copy.date = TRUE
      )
    )
    if (any(!res1)) {
      test1 <- paste0(
        "The following files were not copied to:\n",
        file.path(dir_new, StockNames[nm], fsep = fsep),
        "\n\n",
        paste("\t-", basename(InputFiles[!res1]), collapse = "\n")
      )
      war(text = test1)
    }

    # Copy the stock-specific input files to the Assessment_data folder if
    # applicable
    if (Update_LastAss_file) {
      # Delete the old input files
      clean_Inputfiles(path = file.path(dir_Assdata, StockNames[nm], fsep = fsep),
                       verbose = verbose)
      # res2 <- suppressWarnings(file.copy(
      #   from = InputFiles,
      #   to = file.path(dir_Assdata, StockNames[nm], fsep = fsep),
      #   overwrite = TRUE,
      #   recursive = TRUE,
      #   copy.date = TRUE
      # ))
      # if(any(!res2)){
      #   test2 <- paste0("The following files were not copied to:\n",
      #                   file.path(dir_Assdata, StockNames[nm], fsep = fsep),"\n\n",
      #                   paste("\t-", basename(InputFiles[!res2]), collapse = "\n"))
      #   war(text = test2)
      # }
      resCop <- copy_GMACSinputs(
        fromDir = dirSrcStock,
        toDir = file.path(dir_Assdata, StockNames[nm], fsep = fsep),
        GMACS_files = "all",
        overwrite = TRUE,
        verbose = verbose,
        do_pinFile = FALSE
      )
    } # End Update_LastAss_file


    # Copy the stock-specific input files to the dir_Supp folder(s) if
    # applicable
    if (!is.null(dir_Supp)) {
      for (f in 1:length(dir_Supp)) {
        if (!dir.exists(file.path(dir_Supp[f], StockNames[nm], fsep = fsep)))
          dir.create(file.path(dir_Supp[f], StockNames[nm], fsep = fsep))
        resCop <- copy_GMACSinputs(
          fromDir = dirSrcStock,
          toDir = file.path(dir_Supp[f], StockNames[nm], fsep = fsep),
          GMACS_files = "all",
          overwrite = TRUE,
          verbose = verbose,
          do_pinFile = FALSE
        )
      } # End loop on dir_Supp
    } # End if(!is.null(dir_Supp))

    # Clean the stock specific test folder
    if(cleanStockFolder){
      if(verbose){
        cat(paste0("In:\n", dirSrcStock, "\n"))
      }
      To_del <- list.files(path = dirSrcStock, all.files = TRUE, full.names = FALSE,no.. = TRUE)
      clean_files(path = dirSrcStock, filename = To_del, verbose = verbose)
      if(verbose){
        cat(paste0("Deleting folder:\n", dirSrcStock, "\n"))
      }
      unlink(dirSrcStock, recursive=TRUE)
    }
  } # End for (nm in 1:length(StockNames))


  # Update the run of the latest_version folder
  # Re run Gmacs with the estimation on
  if (UpdateLast_GmacsVer) {
    dir_new_run <- ifelse(basename(dirNew) == "build",
                          yes = dirname(dirNew),
                          no = dirNew)
    # if(is.null(ADMBpaths)){
    #   ADMBpaths <- dir_new_run
    # }
    res <- GMACS(
      Spc = StockNames,
      GMACS_version = "Latest_Version",
      Dir = dir_new_run,
      ASS = FALSE,
      compile = FALSE,
      run = TRUE,
      LastAssDat = FALSE,
      ADMBpaths = ifelse(
        isWindowsOS(),
        "ADpaths_Windows.txt",
        "ADpaths_MacOS.txt"
      ),
      make.comp = FALSE,
      cleanOut = TRUE,
      verbose = FALSE
    )
  } # end UpdateLast_Gmacs

  # Remove the Gmacs version specific folder used to run the test
  if(cleanTest){
    if(verbose){
      cat(paste0("Deleting folder:\n", dirSrc, "\n"))
    }
    unlink(dirSrc, recursive=TRUE)
  }
  # Remove the Gmacs version specific stock input files folder used for the test
  if(cleanInputFiles){
    dir_InputFiles <- file.path(dirname(dirSrc), "Stock_Input_files", basename(dirSrc), fsep = fsep)
    if(dir.exists(dir_InputFiles)){
      if(verbose){
        cat(paste0("Deleting folder:\n", dir_InputFiles, "\n"))
      }
      unlink(dir_InputFiles, recursive=TRUE)
    }
  }
} # End UpdateGMACS function
