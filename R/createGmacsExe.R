#' @title Create the Gmacs executable from the tpl file
#'
#' @description Function to create the GMACS executable from the gmacsbase.tpl
#' and personal.tpl files.
#'
#' @param vv (integer/character string)- index in \code{Dir} indicating folder
#' to create executable in. If set equal to \code{"all"}, the executable will be
#' create in all the folders declared in \code{Dir}.
#' @param Dir (character string)- vector of folder names.
#' @param verbose (logical)- flag (TRUE/FALSE) to print processing information.
#' @param logFiles (logical)- flag (TRUE/FALSE) to create log files during
#' executable creation.
#' @param ADMBpaths (character string)- (filepath): absolute or relative to
#' current working directory path to file defining required ADMB paths.
#' The default is `NULL`.
#'
#' @return Nothing.
#'
#' @details This function assumes that any additional cpp files are in a subfolder
#' of \code{Dir} named "lib", with the associated header files in a subfolder
#' named "include".
#' It merges the two tpl files together, then uses [.buildGmacs()] to
#' convert the gmacs.tpl code to a cpp files (gmacs.cpp and gmacs.htp), calls
#' ADMB to compile all .cpp files required to build gmacs and creates the gmacs
#' executable in the folder specified by \code{Dir[vv]}. The executable will be
#' "gmacs.exe" on windows platforms and "gmacs" on linux-like (including MacOS)
#' machines. Object files created during compilation from the additional cpp files
#' will be in the \code{Dir} folder on Windows machines and in the \code{Dir/lib}
#' folder on unix-like machines. These can be "cleaned" up using [clean_root()].
#'
#' @export
#'
createGmacsExe <-
  function(vv,
           Dir,
           verbose = FALSE,
           logFiles = FALSE,
           ADMBpaths = NULL) {

    fsep <- .Platform$file.sep

    # Check directories for ADMB
    # Define the name of the file containing the different pathways needed to build
    # the GMACS executable
    if(!file.exists(ADMBpaths)){
      warning("The 'ADMBpaths' file does not exist. Please check the ADMBpaths argument\n")
      stop()
    } else {
      PBSadmb::setupAD(ADMBpaths)
      suppressWarnings(PBSadmb::readADpaths(ADMBpaths))
    }
    
    cat("\n Verifying the paths for ADMB, the C/C++ compiler and the editor ....\n")
    if (!PBSadmb::checkADopts())
      stop(
        "The definition of the pathways to locate ADMB,the C/C++ compiler and/or the editer are wrong.\nPlease check the ADMBpaths file."
      )

    oldWD <- getwd()
    on.exit(setwd(oldWD))

    do <- function(vv = NULL, Dir, verbose, ADMBpaths) {
      oldWD <- getwd()
      # on.exit(setwd(oldWD))

      # cat("--Setting working directory to '", Dir[vv], "' \n", sep = "")
      # setwd(Dir[vv])
      # on.exit(setwd(dirname(Dir[vv])))

      # Clean directory from previous version
      clean_root(path = Dir[vv])

      # Temporary folder to test the compilation
      DirTmp <- file.path(Dir[vv], "tmpComp", fsep = fsep)
      if (!dir.exists(DirTmp)) {
        dir.create(DirTmp, recursive = TRUE)
        dir.create(file.path(DirTmp, "include", fsep = fsep), recursive = TRUE)
      }
      cat(
        "--Setting working directory to: \n\t'",
        DirTmp,
        "' \nto test the compilaiton of GMACS\n\n",
        sep = ""
      )
      setwd(DirTmp)

      # Copy necessary files to the DirTmp
      file.copy(
        from = file.path(Dir[vv], c("gmacsbase.tpl", "personal.tpl"), fsep = fsep),
        to = file.path(DirTmp, c("gmacsbase.tpl", "personal.tpl"), fsep = fsep),
        overwrite = TRUE,
        # recursive = TRUE,
        copy.date = TRUE
      )
      # Copy the files from include
      dir_include <- file.path(Dir[vv], "include", fsep = fsep)
      file.copy(
        from = list.files(dir_include, full.names = TRUE),
        to = file.path(DirTmp, "include", list.files(dir_include), fsep = fsep),
        overwrite = TRUE,
        # recursive = TRUE,
        copy.date = TRUE
      )

      #  Create gmacs.tpl from gmacsbase.tpl and personal.tpl
      cat("Now writing gmacs.tpl\n")
      write_TPL(vv = 1,
                # Dir = Dir,
                Dir = DirTmp,
                .update = FALSE)


      #--additional cpp files in "lib" subfolder of Dir[vv]
      #--header files are in "include" subfolder of Dir[vv]
      libFiles <-
        dir(
          file.path(Dir[vv], "lib", fsep = fsep),
          "*.cpp",
          ignore.case = TRUE,
          all.files = TRUE,
          full.names = TRUE
        )
      if (isWindowsOS()) {
        #--for compilation, path to "include" folder assumes it is a subfolder relative to the cpp  files
        #--need to copy cpp files to Dir[vv] so include folder is subfolder of working directory
        file.copy(libFiles, DirTmp, overwrite = TRUE)
        args <- get_nam(basename(libFiles))
        #--drop extensions
      } else {
        #--for compilation, path to "include" folder assumes it is in "parallel" folder relative to the cpp  files
        #--no need to copy files to Dir[vv]
        args <- get_nam(libFiles)
        #--drop extensions
      }

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
      cat("OK after conversion from .tpl to .cpp ...\n")
      cat("\n")

      # Compile files
      # Function to test compilation
      testComp <- function(fileToComp) {
        test <- tryCatch(
          PBSadmb::compAD(
            prefix = fileToComp,
            pathfile = ADMBpaths,
            safe = TRUE,
            debug = TRUE,
            logfile = logFiles,
            verbose = FALSE
          ),
          error = function(e)
            e,
          warning = function(w)
            w
        )
        if (!is.na(class(test)[2]) & class(test)[2] == "warning") {
          cat(
            "\n --> Something was wrong with the compilation of ",
            fileToComp,
            ".\n Please check the following error message:\n\n",
            "# ######################################################## #\n",
            "# ######################################################## #\n\n"
          )
          out <- PBSadmb::compAD(
            prefix = fileToComp,
            pathfile = ADMBpaths,
            safe = TRUE,
            debug = TRUE,
            logfile = logFiles,
            verbose = TRUE
          )
          cat(
            out,
            "\n\n # ######################################################## #\n",
            "# ######################################################## #\n",
            "\n\n\n"
          )
          setwd(oldWD)
          base::unlink(x = DirTmp,
                       recursive = TRUE,
                       force = TRUE)

          stop()
        }
      }
      compFiles <- c("gmacs", args)
      for (nm in 1:length(compFiles)) {
        cat("Now compiling file ", nm, ": '", compFiles[nm], "'\n", sep = "")
        # PBSadmb::compAD(
        #   prefix = compFiles[nm],
        #   pathfile = ADMBpaths,
        #   safe = TRUE,
        #   debug = TRUE,
        #   logfile = FALSE,
        #   verbose = verbose
        # )
        testComp(fileToComp = compFiles[nm])
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


      # Copy the .tpl and exe in the root dir
      nfile <-
        list.files(DirTmp, full.names = TRUE)[!list.files(DirTmp) %in% "include"]
      file.copy(
        from = nfile,
        to = file.path(Dir[vv], basename(nfile), fsep = fsep),
        overwrite = TRUE,
        copy.date = TRUE
      )
      # Remove the temporary folder
      setwd(oldWD)
      cat("\n--Deleting the temporary folder for compilation test ...\n")
      base::unlink(x = DirTmp,
                   recursive = TRUE,
                   force = TRUE)

    }

    if (vv == "all") {
      for (i in 1:length(Dir)) {
        do(
          vv = i,
          Dir = Dir,
          verbose = verbose,
          ADMBpaths = ADMBpaths
        )
        Sys.sleep(0.1)
        cat("\n")
      }
    } else {
      do(
        vv = vv,
        Dir = Dir,
        verbose = verbose,
        ADMBpaths = ADMBpaths
      )
    }

    cat("--Re-setting working directory to '", oldWD, "' \n", sep = "")
    #--setwd(oldWD) <-automatically does this on exit
  }
