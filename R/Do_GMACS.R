
#' @title Do_GMACS
#'
#' @description This function allows one to build the GMACS executable, run GMACS
#' and make comparisons if several versions of GMACS are considered in the analysis.
#' For each stock considered in the analysis, this function will copy
#' all the input files required by GMACS from a specific directory (which can be
#' the directory where the input files used for the last assessment are stored,
#' or some other).
#'
#' @param Spc vector of strings specifying the name(s) of the stock
#' considered in the analysis.
#' @param GMACS_version vector of strings holding the name(s) of the
#' GMACS versions that is/are used in the analysis.
#' @param ASS Logical. If TRUE, the outputs of the last assessment will be compared
#' to the GMACS version(s) currently used in this analysis.
#' @param AssMod_names vector of strings specifying the names of the models used
#' in the last assessment (e.g., model_16_0)
#' @param Dir vector of strings containing the directories for all elements of
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
#' \code{GMACS_version}s considered in the analysis.
#' @inheritParams PBSadmb::convAD
#'
#' @seealso \code{\link{Do_Comp}} for comparisons, \code{\link{createGmacsExe}} for
#' building the executable.
#'
#' @return Depending on the option chose, it can return all the output files from
#' GMACS, an error if something happened during the run or a list of comparison
#' between the various GMACS version used for each stock considered in the analysis.
#'
#' @export
#'
#'
Do_GMACS <- function(Spc = NULL,
                     GMACS_version = NULL,
                     ASS = NULL,
                     AssMod_names  = NULL,
                     Dir = NULL,
                     compile = NULL,
                     run = NULL,
                     LastAssDat = NULL,
                     ADMBpaths = NULL,
                     make.comp = NULL,
                     verbose = NULL) {

  for (vv in 1:length(Dir)) {


    # Check directories for ADMB
    # Define the name of the file containing the different pathways needed to build
    # the GMACS executable
    suppressWarnings(PBSadmb::readADpaths(file.path(dirname(Dir[vv]), ADMBpaths)));
    cat("\n Verifying the paths for ADMB, the C/C++ compiler and the editor ....\n")
    if (!PBSadmb::checkADopts())
      stop(
        "The definition of the pathways to locate ADMB,the C/C++ compiler and/or the editer are wrong.\nPlease check the ADMBpaths file."
      )

    cat(
      "\n# ------------------------------------------------------------------- #\n"
    )
    cat("# ------------------------------------------------------------------- #\n")
    cat("        Now building GMACS for the ", GMACS_version[vv], " \n")
    cat("# ------------------------------------------------------------------- #\n")
    cat("# ------------------------------------------------------------------- #\n")

    # vv <- 1

    # 1.Get an executable for GMACS ----
    gmacs_exe <- ifelse(isWindowsOS(),"gmacs.exe","gmacs")
    if (compile[vv] == 1) {
      createGmacsExe(vv,Dir,verbose=ifelse(is.logical(verbose),verbose,FALSE))
    } else {
      if (!file.exists(file.path(Dir[vv], gmacs_exe))) {
        stop(
          paste(
            "no gmacs executable exists in the source directory:",
            Dir[vv],
            sep = " "
          ),
          cat(
            "\nPlease provide this folder with an executable or allow for compilation
                                                                    (i.e. turn on 'compile')\n"
          )
        )
      } else {
        warning("!!! GMACS has not been recompiled. !!! ")
      }

    }

    # 2. Check directory existence, data availability and run GMACS----

    cat(
      "\n# ------------------------------------------------------------------- #\n"
    )
    cat("# ------------------------------------------------------------------- #\n")
    cat("#   Now realizing assessments for the ",
        GMACS_version[vv],
        "of GMACS \n")
    cat("# ------------------------------------------------------------------- #\n")
    cat("# ------------------------------------------------------------------- #\n")

    # build directory
    dirbuild <- file.path(Dir[vv], "build")
    if (!dir.exists(dirbuild))
      dir.create(file.path(dirbuild), recursive = TRUE)
    if (!dir.exists(file.path(dirbuild, 'debug')))
      dir.create(file.path(dirbuild, 'debug'), recursive = TRUE)
    if (!dir.exists(file.path(dirbuild, 'release')))
      dir.create(file.path(dirbuild, 'release'), recursive = TRUE)

    id_term <- matrix(NA, nrow = length(Spc), ncol = 5)

    for (nm in 1:length(Spc)) {
      # nm=1
      srcdir <- file.path(dirname(getwd()), "Assessment_data", Spc[nm])
      todir  <- file.path(Dir[vv], "build", Spc[nm])

      # Check if directory already exist
      if (!dir.exists(todir)) {
        dir.create(file.path(todir), recursive = TRUE)
        cat(
          "\nBuilding the following directory :\n",
          todir,
          "\nIt will hold data and run outputs for the assessment of: ",
          Spc[nm],
          ".\n",
          sep = ""
        )
      }

      if (LastAssDat) {
        Filescop <- list.files(srcdir, full.names = TRUE)
        for (i in 1:length(Filescop))
          file.copy(Filescop[i], to = todir, overwrite = TRUE)
        cat("Data for ", Spc[nm], " have been copied.\n")
      } else {
        if (file.exists(file.path(todir, "gmacs.dat"))) {
          nam <- read_GMACS.dat(path = file.path(todir, "gmacs.dat"))
          tmp <- NULL
          for (f in 1:length(nam)) {
            if (!file.exists(file.path(todir, nam[f])))
              tmp <- c(tmp, f)
          }
          if (!is.null(tmp))
            stop(
              "The following file(s): ",
              paste0(unlist(nam[tmp]), sep = " "),
              "is/are missing in the ",
              todir,
              " directory.",
              sep = ""
            )
        } else {
          stop(
            "There is no 'gmacs.dat' in the following directory :\n",
            todir,
            "\nPlease provide this file and check for other files (.ctl, .dat, .prj) or allow data copy from the following directory:\n",
            srcdir
          )
        }
      }

      # Copy clean.bat and gmacs.exe to the repertories to run GMACS
      Excop <- file.path(Dir[vv], gmacs_exe)
      file.copy(
        Excop,
        to = file.path(Dir[vv], "build", Spc[nm]),
        copy.date = TRUE,
        overwrite = TRUE,
        recursive = TRUE
      )

      if (nm == length(Spc))
        cat("\nOK after copying all files ...\n")
    }

    # Run GMACS
    cat("\nNow entering assesments:\n")
    cat("\n")
    for (nm in 1:length(Spc)) {
      cat("Starting assessment for:", Spc[nm], "\n")
      # id <- GMACS_term(.Dir = paste(Dir[vv], "build/", Spc[nm], sep=""), verbose = verbose)
      id <-
        gmr::.CallTerm(
          command = ifelse(.Platform$OS.type=="windows",gmacs_exe,paste0("./",gmacs_exe)),
          .Dir = file.path(Dir[vv], "build", Spc[nm]),
          verbose = verbose
        )
      id_term[nm, c(1:3)] <-
        c(Spc[nm], paste("Terminal", nm, sep = "_"), id)
    } # end loop on Spc

    end_term <- "run"
    ct <- rep(NA, length(Spc))
    GMACS_OUT <- NULL

    while (end_term == "run") {
      Sys.sleep(0.1)

      for (nm in 1:length(Spc)) {
        if (!is.null(rstudioapi::terminalExitCode(id_term[nm, 3]))) {
          id_term[nm, 4] <- rstudioapi::terminalExitCode(id_term[nm, 3])
          id_term[nm, 5] <- "Done"
          if (id_term[nm, 5] == "Done" && is.na(ct[nm])) {
            ct[nm] <- nm
            cat("\nAssessment completed for :", Spc[nm], "\n")
            if (length(which(is.na(id_term[, 4]))) > 0)
              cat("\n", length(which(is.na(
                id_term[, 4]
              ))), " runs still in progress\n")
          }
        }
      }

      if (length(which(!is.na(id_term[, 5]))) > 0 &&
          length(which(id_term[, 5] == "Done")) == length(Spc)) {
        if (unique(id_term[, 4] == 0)) {
          cat("\nAll assessment have been carried out.\n")
        } else {
          for (nm in 1:length(Spc)) {
            #eval(parse(text = paste(Spc[nm], 'list()', sep = "")))
            eval(parse(
              text = paste(
                Spc[nm],
                '_out<-terminalBuffer(id_term[',
                nm,
                ',3], stripAnsi = FALSE)',
                sep = ""
              )
            ))
            cat(
              "\nSomething was wrong for : ",
              Spc[nm],
              ". Please Check the output of the terminal in : GMACS_OUT$",
              Dir[vv],
              "$",
              Spc[nm],
              '_out',
              sep = ""
            )
            # rstudioapi::terminalKill(id_term[nm,3])
          }
        }
        end_term <- "Done"
      }
    }

    if (!verbose) {
      for (nm in 1:length(Spc))
        rstudioapi::terminalKill(id_term[nm, 3])
    } else {
      cat("\nTerminals are still open. Please take a look at them if necessary or close them.\n")
    }

    cat("\n# ----------------------------------------------------------------- #\n")
    cat("# ----------------------------------------------------------------- #\n")
    cat("              Version ", GMACS_version[vv], "done.\n")
    cat("# ----------------------------------------------------------------- #\n")
    cat("# ----------------------------------------------------------------- #\n")


  }# end loop on GMACS version

  if (!is.null(GMACS_OUT))
    return(GMACS_OUT)

  if (make.comp)
    gmr::Do_Comp(
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
      verbose = verbose
    )

}
