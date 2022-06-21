#' @title .GetGmacsExe
#'
#' @description Function used when updating and releasing a new version of GMACS.
#' It uses the `write_TPL` function to editate the new
#' gmacs.tpl, compiles the model and uses the `.buildGMACS()`
#' function to provide a new executable.
#'
#' @param .nameFold (character string); name of the folder holding the development
#' version you want to work on. The default is `Dvpt_Version` but you can
#' have renamed this folder.
#' @param .nameVer (character string); Name of the development version of GMACS you
#' are going to work on. Default: `Dvpt_Version`.
#'
#'
#' @return the new GMACS executable
#'
#' @export
#'
#'
.GetGmacsExe <- function(.nameFold = "Dvpt_Version", .nameVer = NULL) {


  # Define directory

  if(.nameFold != "Dvpt_Version"){

    check <- NA
    while (is.na(check)) {
      text = paste("You have specified a file name other than 'Dvpt_Version' to work on GMACS and/or develop a new version.
      Please confirm that the following directory is the one you want to work in (0:No, 1: Yes):\n",
                   print(paste0(getwd(), "/", .nameFold, "/")),sep="")
      check <- svDialogs::dlgInput(text, Sys.info())$res
      Sys.sleep(0.1)
    }
    if(.an(check)==0) stop("Please redefine the directory you want to work in.")
    GMACS_version <- .nameVer
  } else {GMACS_version <- "Dvpt_Version"}


  Dir <-  paste0(getwd(), "/", .nameFold, "/")

  # Need to conpile the model?
  # vector of length(.GMACS_version)
  # 0: GMACS is not compiled. This assumes that an executable exists in the directory of the concerned version.
  # 1: GMACS is compiles
  # compile <- 1

  # Names of the GMACS version to consider
  vv <- 1

  # Check directories for ADMB
  # Define the name of the file containing the different pathways needed to build
  # the GMACS executable
  ADMBpaths = .ADMBpaths
  suppressWarnings(PBSadmb::readADpaths(paste(dirname(Dir[vv]), ADMBpaths, sep =
                                                "/")))
  cat("\n Verifying the paths for ADMB, the C/C++ compiler and the editor ....\n")
  if (!PBSadmb::checkADopts())
    stop(
      "The definition of the pathways to locate ADMB,the C/C++ compiler and/or the editer are wrong.\nPlease check the ADMBpaths file."
    )

  cat("\n# ------------------------------------------------------------------- #\n")
  cat("# ------------------------------------------------------------------- #\n")
  cat("        Now building GMACS for the ", GMACS_version[vv], " \n")
  cat("# ------------------------------------------------------------------- #\n")
  cat("# ------------------------------------------------------------------- #\n")

  # 1.Get an executable for GMACS ----
  setwd(Dir[vv])
  # Clean directory from previous version
  gmr::.CallTerm(command = "clean_root.bat",
                 .Dir = Dir[vv],
                 verbose = FALSE)

  #  Create gmacs.tpl from gmacsbase.tpl and personal.tpl
  cat("Now writing gmacs.tpl\n")
  write_TPL(vv = vv,
                 Dir = Dir[vv],
                 .update = TRUE)
  # cat("\n")

  # Copy files from lib\
  libFiles <-
    dir(paste0(Dir[vv], "/lib/"),
        "*.cpp",
        ignore.case = TRUE,
        all.files = TRUE)
  file.copy(file.path(paste0(Dir[vv], "/lib/"), libFiles), Dir[vv], overwrite = TRUE)
  args <- get_nam(libFiles)

  # .tpl to .cpp
  cat("\nNow converting gmacs.tpl to gmacs.cpp ...\n")
  PBSadmb::convAD(
    prefix = "gmacs",
    pathfile = ADMBpaths,
    debug = TRUE,
    safe = TRUE,
    logfile = FALSE,
    verbose = FALSE
  )
  cat("OK after convertion from .tpl to .cpp ...\n")
  cat("\n")

  # Compile files
  compFiles <- c("gmacs", libFiles)
  for (nm in 1:length(compFiles)) {
    cat("Now compiling ", compFiles[nm], "...\n")
    PBSadmb::compAD(
      prefix = compFiles[nm],
      pathfile = ADMBpaths,
      safe = TRUE,
      debug = TRUE,
      logfile = FALSE,
      verbose = FALSE
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
    logfile = FALSE,
    add = FALSE,
    verbose = FALSE,
    pathfile = NULL,
    args = args
  )
  cat("OK after building gmacs executable ...\n")
  setwd(dirname(Dir[vv]))
}
