#' @title GMACS
#'
#' @description For each stock and version considered in the analysis, this function
#' realizes  the analysis based on the choice of the user, i.e: compiles the model,
#' runs the model, and/or establishes comparison between versions. This is the core
#' version to work with GMACS in `R`.
#'
#' @param Spc vector of strings specifying the name(s) of the stock
#' considered in the analysis.
#' @param GMACS_version vector of strings holding the name(s) of the
#' GMACS versions that is/are used in the analysis.
#' @param ASS Logical. If TRUE, the outputs of the last assessment will be compared
#' to the GMACS version(s) currently used in this analysis.
#' @param AssMod_names vector of strings specifying the names of the models used
#' in the last assessment (e.g., model_16_0)
#' @param Dir vector of strings containing the directories for all
#' \code{GMACS_version} used in this analysis
#' @param compile (0/1). If 0, GMACS is not compiled. This assumes that an
#' executable already exists in the directory of the version(s) used in the analysis.
#' If 1, the code will be compiled before a new run.
#' @param run Logical. If TRUE the model will be executed.
#' @param LastAssDat Logical. If TRUE, the latest available data will be used for
#' the analysis i.e. the model will be executed using the data stored in the
#' the "Assessment_data" folder.
#' @param ADMBpaths string name of 2-column text file that details the relevant
#' paths for the R variables admbpath, gccpath, and editor.
#' @param make.comp Logical. If TRUE, comparisons will be made between the various
#' \code{GMACS_version} considered in the analysis.
#' @param cleanOut (logical) - Specify if the `Dir` has to be cleaned after
#' the run. See the \code{\link{clean_bat()}} function. Default \code{cleanRun = FALSE}.
#' @inheritParams PBSadmb::convAD
#'
#' @seealso \code{\link{Do_GMACS}}, \code{.buildGMACS} for
#' building the executable.
#'
#'
#' @export
#'
#'
GMACS <- function(Spc = NULL,
                  GMACS_version = NULL,
                  Dir = NULL,
                  ASS = NULL,
                  AssMod_names  = NULL,
                  compile = NULL,
                  run = NULL,
                  LastAssDat = NULL,
                  ADMBpaths = NULL,
                  make.comp = NULL,
                  verbose = NULL,
                  cleanOut = NULL) {
  fsep <- .Platform$file.sep

  # 1. Set specific directories----
  if (is.null(Spc)) {
    cat("\nPlease provide the name of the stock considered in the analysis.\n")
    stop("The 'Spc' argument in the GMACS() function is empty.\n")
  }
  if (length(Spc) == 1 && Spc == "all") {
    # Spc <-
    #   grep(
    #     list.files(file.path(
    #       dirname(getwd()), "Assessment_data", fsep = fsep
    #     )
    #     ),
    #     pattern = '.bat',
    #     invert = TRUE,
    #     value = TRUE
    #   )
    Spc <-
      grep(
        list.files(file.path(Dir, "build", fsep = fsep)),
        pattern = 'debug|release',
        invert = TRUE,
        value = TRUE
      )

    nam.Spc <- sort(Spc)
    Spc <- Spc[order(match(Spc, nam.Spc))]
  }
  cat("\nThis analysis includes the following species:\n",
      paste0("- ", Spc, collapse = "\n"),
      "\n")
  cat("\n")

  # 2. Check for consistency between options----

  if (is.null(GMACS_version)) {
    cat("\nPlease provide the name of the GMACS version(s) you want to use in the analysis.\n")
    stop("The 'GMACS_version' argument in the GMACS() function is empty.\n")
  }
  if (is.null(Dir)) {
    cat(
      "\nPlease provide the Directories for the GMACS version(s) you want to use in the analysis.\n"
    )
    stop("The 'Dir' argument in the GMACS() function is empty.\n")
  }
  if (length(GMACS_version) != length(Dir)) {
    cat("The number of directory does not match the number of version you specified\n")
    stop("Please give a directory for each version of GMACS you defined.")
  }

  if (is.null(make.comp)) {
    cat("\nNo comparison will be establish in this analysis.\n")
    make.comp <- FALSE
    # LastAssDat <- rep(0, length(Spc))
  }
  if (is.null(ASS)) {
    ASS <- FALSE
    if(make.comp==TRUE) cat("\nThe last assessment is not considered in the comparison analysis.\n")
  }
  if(is.null(AssMod_names))
    if (!is.null(ASS) && ASS==TRUE) {
      cat(
        "\nPlease provide the name of the last assessment model you want to consider in the comparison analysis.\n"
      )
      stop("The 'AssMod_names' argument in the GMACS() function is empty\n.")
    }

  if (length(compile) != length(Dir)) {
    cat("\nPlease precise if you want to compile or not each GMACS version.\n")
    stop(
      "The length of the 'compile' argument in the GMACS() function does not fit the number of GMACS version considered\n."
    )
  }
  if (is.null(compile)) {
    cat("\nGMACS will not be compiled in this analysis.\n")
    compile <- rep(o, length(Dir))
  }

  if (is.null(run)) {
    cat("\nGMACS will not be executed in this analysis.\n")
    run <- FALSE
    # run <- rep(o, length(Dir))
  }
  if (run && ASS) {
    cat(
      "You're attempting to run the last assessment and then make comparison. We suppose here that you don't need to run the\n
        last assessment."
    )
    stop(
      "Please turn off the consideration of the last assessment if you want to run either old, current or new version of GMACS.\n
         You can use this script to make comparisons afterwords with the last evaluation but without running GMACS for the latter."
    )
  }

  if (is.null(LastAssDat)) {
    cat("\nThe latest assessment data will not be used in this analysis.\n")
    LastAssDat <- FALSE
    # LastAssDat <- rep(0, length(Spc))
  }


  if (make.comp && length(GMACS_version) == 1) {
    cat("\nYou have specified only one version of GMACS - no comparison can be made.\n")
    stop("Provide several versions of GMACS that you wish to compare\n")
  }

  # Turn off cleaning after run if not precised
  if(is.null(cleanOut))
    cleanOut <- FALSE


  # 3. Realize the analysis----

  if (run && !ASS) {
    Do_GMACS(
      Spc = Spc,
      GMACS_version = GMACS_version,
      ASS = FALSE,
      AssMod_names  = NULL,
      Dir = Dir,
      compile = compile,
      run = run,
      LastAssDat = LastAssDat,
      ADMBpaths = ADMBpaths,
      make.comp = make.comp,
      verbose = verbose,
      cleanRun = cleanOut
    )
  } else {
    if (make.comp && !run)
      tables <-
        Do_Comp(
          Spc = Spc,
          GMACS_version = GMACS_version,
          ASS = ASS,
          AssMod_names  = AssMod_names,
          Dir = Dir,
          compile = compile,
          run = run,
          LastAssDat = LastAssDat,
          ADMBpaths = ADMBpaths,
          make.comp = make.comp,
          verbose = verbose,
          cleanRun = cleanOut
        )
    # print(tables)
  }
}
