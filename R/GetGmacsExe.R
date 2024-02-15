#' @title .GetGmacsExe
#'
#' @description Function used when updating and releasing a new version of GMACS.
#' It uses the `write_TPL` function to create a new
#' gmacs.tpl file from the gmacsbase.tpl and personal.TPL files,
#' compiles the model and uses the `.buildGMACS()`
#' function to provide a new executable.
#'
#' @param .nameFold (character string); name of the subfolder holding the development
#' version you want to work on. The default is `Dvpt_Version` but you can
#' have renamed this folder.
#' @param .nameVer (character string); name of the development version of GMACS you
#' are going to work on. Default: `Dvpt_Version`.
#' @param ADMBpaths (filepath): absolute or relative to current working directory
#' path to file defining required ADMB paths. The default is `NULL`.
#' @param verbose (TRUE/FALSE); flag to print processing information.
#' @param logFiles (TRUE/FALSE); flag to create PBSadmb log files.
#'
#' @details This function assumes you are calling it from the parent folder to
#' that identified by \code{.nameFold}. In the course of creating the executable
#' of a development version of GMACS, the working directory is switched to
#' \code{.nameFold}, but switched back to the parent folder when the function exits.
#'
#'
#' @return the new GMACS executable for the development version you are working on.
#'
#' @export
#'
#'
.GetGmacsExe <- function(.nameFold = "Dvpt_Version",
                         .nameVer = NULL,
                         ADMBpaths = NULL,
                         verbose = FALSE,
                         logFiles = FALSE) {
  # Define directory

  if(.nameFold != "Dvpt_Version"){

    check <- NA
    while (is.na(check)) {
      text = paste("========================================\nYou have specified a folder name other than 'Dvpt_Version' to work on GMACS and/or develop a new version.\n========================================\n
\nPlease confirm that the following directory is the one you want to work in (0:No, 1: Yes):\n",
                   file.path(getwd(), .nameFold),sep="")
      # check <- svDialogs::dlgInput(message = text, Sys.info())$res
      check <- svDialogs::dlgInput(message = text, default = "(0:No, 1: Yes)")$res
      Sys.sleep(0.1)
    }
    if(.an(check)==0){
      stop("Please redefine the directory you want to work in.")
    } else if(.an(check) !=1 || !is.numeric(.an(check))){
        stop("The only possibilities are 0 or 1.")
      }
    GMACS_version <- .nameVer
  } else {GMACS_version <- "Dvpt_Version"}


  Dir <-  file.path(getwd(), .nameFold);

  # Need to compile the model?
  # vector of length(.GMACS_version)
  # 0: GMACS is not compiled. This assumes that an executable exists in the directory of the concerned version.
  # 1: GMACS is compiles
  # compile <- 1

  # Names of the GMACS version to consider
  vv <- 1

  # Check directories for ADMB
  # Define the name of the file containing the different pathways needed to build
  # the GMACS executable
   suppressWarnings(PBSadmb::readADpaths(ADMBpaths));
  cat("\n Verifying the paths for ADMB, the C/C++ compiler and the editor ....\n")
  if (!PBSadmb::checkADopts())
    stop(
      "The definition of the pathways to locate ADMB,the C/C++ compiler and/or the editer are wrong.\nPlease check the ADMBpaths file."
    )

  cat("# ------------------------------------------------------------------- #\n")
  cat("        Now building GMACS for the ", GMACS_version[vv], " \n")
  cat("# ------------------------------------------------------------------- #\n")

  # 1.Get an executable for GMACS ----
  oldWD = getwd();
  on.exit(setwd(oldWD));
  cat("--Setting working directory to '", Dir[vv], "' \n",sep="")
  setwd(Dir[vv]);

  # Clean directory from previous version
  clean_root(path=Dir[vv]);

  #  Create gmacs.tpl from gmacsbase.tpl and personal.tpl
  cat("Now writing gmacs.tpl\n")
  write_TPL(vv = vv,
            Dir = Dir[vv],
            .update = TRUE)
  # cat("\n")

  # Copy files from ./lib IF on windows
  libFiles <-
    dir(path=file.path(Dir[vv], "lib"),
        pattern="*.cpp",
        ignore.case = TRUE,
        all.files = TRUE,
        full.names=TRUE)
  if (isWindowsOS()){
    file.copy(libFiles, Dir[vv], overwrite = TRUE)
    args <- get_nam(basename(libFiles));#--drop extensions
  } else {
    args <-get_nam(libFiles);#--drop extensions
  }

  # .tpl to .cpp
  cat("\nNow converting gmacs.tpl to gmacs.cpp ...\n")
  PBSadmb::convAD(
    prefix = "gmacs",
    pathfile = ADMBpaths,
    debug = TRUE,
    safe = TRUE,
    logfile = logFiles,
    verbose = verbose
  )
  cat("OK after conversion from .tpl to .cpp ...\n")
  cat("\n")

  # Compile files
#  compFiles <- c("gmacs", libFiles)
  compFiles <- c("gmacs", args)
  for (nm in 1:length(compFiles)) {
    cat("Now compiling file ",nm,": '",compFiles[nm],"'\n",sep="")
    PBSadmb::compAD(
      prefix = compFiles[nm],
      pathfile = ADMBpaths,
      safe = TRUE,
      debug = TRUE,
      logfile = logFiles,
      verbose = verbose
    )
  }
  cat("OK after compilation ...\n")

  # Build GMACS
  cat("\nNow building gmacs executable ...\n")
  .buildGMACS(
    prefix = "gmacs",
    raneff = FALSE,
    safe = TRUE,
    dll = FALSE,
    debug = TRUE,
    logfile = logFiles,
    add = FALSE,
    verbose = verbose,
    pathfile = NULL,
    args = args
  )
  cat("OK after building gmacs executable ...\n")

  cat("--Re-setting working directory to '", oldWD, "' \n",sep="")
  #--setwd(oldWD) <-does this on exit
}
