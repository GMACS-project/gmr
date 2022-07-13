
#' @title Do_GMACS
#'
#' @description This function allows to build the GMACS executable, run GMACS
#' and make comparison if several version of GMACS are considered in the analysis.
#' For each stock considered in the analysis, this function will copy the
#' all the input files required by GMACS from a specific directory which can be
#' the directory where are stored the input files used for the last assessment.
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
#' @inheritParams PBSadmb::convAD
#'
#' @seealso \code{\link{Do_Comp}} for comparisons, \code{.buildGMACS} for
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
    cat(
      "\n# ------------------------------------------------------------------- #\n"
    )
    cat("# ------------------------------------------------------------------- #\n")
    cat("        Now building GMACS for the ", GMACS_version[vv], " \n")
    cat("# ------------------------------------------------------------------- #\n")
    cat("# ------------------------------------------------------------------- #\n")

    # vv <- 1

    # 1.Get an executable for GMACS ----

    # Check directories for ADMB
    suppressWarnings(PBSadmb::readADpaths(paste(dirname(Dir[vv]), ADMBpaths, sep =
                                                  "/")))
    if (!PBSadmb::checkADopts())
      stop("The definition for the ADMB directories are wrong. Please check.")

    if (compile[vv] == 1) {
      # Clean directory from previous version
      setwd(Dir[vv])
      gmr::.CallTerm(command = "clean_root.bat",
                     .Dir = Dir[vv],
                     verbose = verbose)

      #  Create gmacs.tpl from gmacsbase.tpl and personal.tpl
      cat("Now writing gmacs.tpl\n")
      write_TPL(vv = vv,
                Dir = Dir,
                .update = FALSE)
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
        verbose = verbose
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
        logfile = FALSE,
        add = FALSE,
        verbose = verbose,
        pathfile = NULL,
        args = args
      )
      cat("OK after building gmacs executable ...\n")
      setwd(dirname(Dir[vv]))

    } else {
      if (!file.exists(paste0(Dir[vv], "gmacs.exe"))) {
        stop(
          paste(
            "no gmacs executable exists in the source directory:",
            Dir[vv],
            sep = " "
          ),
          cat(
            "\nPlease provide this repertory with an executable or allow for compilation
                                                                    (i.e. turn on 'compile')\n"
          )
        )
      } else {
        cat("!!! GMACS has not been recompiled. !!! ")
        cat("\n")
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
    dirbuild <- paste(Dir[vv], "build/", sep = "")
    if (!dir.exists(dirbuild))
      dir.create(file.path(dirbuild), recursive = TRUE)
    if (!dir.exists(paste0(dirbuild, 'debug/')))
      dir.create(file.path(paste0(dirbuild, 'debug/')), recursive = TRUE)
    if (!dir.exists(paste0(dirbuild, 'release/')))
      dir.create(file.path(paste0(dirbuild, 'release/')), recursive = TRUE)

    id_term <- matrix(NA, nrow = length(Spc), ncol = 5)

    for (nm in 1:length(Spc)) {
      # nm=1
      srcdir <-
        paste(dirname(getwd()), "/Assessment_data/", Spc[nm], sep = "")
      todir <-
        paste(Dir[vv], "build/", paste0(Spc[nm], "/"), sep = "")

      # Check if directory already exist
      if (!dir.exists(todir)) {
        dir.create(file.path(todir), recursive = TRUE)
        cat(
          "\nBuilding the following directory :\n",
          todir,
          "\nIt will hold data and run outputs for the assessment of :",
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
        if (file.exists(paste(todir, "gmacs.dat", sep = ""))) {
          nam <- read_GMACS.dat(path = paste(todir, "gmacs.dat", sep = ""))
          tmp <- NULL
          for (f in 1:length(nam)) {
            if (!file.exists(paste(todir, nam[f], sep = "")))
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
      Excop <- paste0(Dir[vv], c("clean.bat", 'gmacs.exe'))
      file.copy(
        Excop,
        to = paste(Dir[vv], "build/", Spc[nm], sep = ""),
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
          command = "gmacs.exe",
          .Dir = paste(Dir[vv], "build/", Spc[nm], sep = ""),
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
            eval(parse(text = paste(Spc[nm], 'list()', sep = "")))
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
