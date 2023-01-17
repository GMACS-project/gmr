#' @title Create the Gmacs executable from the tpl file
#'
#' @description Function to create the GMACS executable from the gmacsbase.tpl
#' and personal.tpl files.
#'
#' @param vv - integer index in \code{Dir} indicating folder to create executable in
#' @param Dir - vector of folder names
#' @param verbose - flag (TRUE/FALSE) to print processing information
#' @param logFiles - flag (TRUE/FALSE) to create log files during executable creation
#' @param ADMBpaths (filepath): absolute or relative to current working directory
#' path to file defining required ADMB paths. The default is `NULL`.
#'
#' @return nothing
#'
#' @details This function assumes that any additional cpp files are in a subfolder
#' of \code{Dir} named "lib", with the associated header files in a subfolder named "include".
#' It merges the two tpl files together, then uses [.buildGmacs()] to
#' convert the gmacs.tpl code to a cpp files (gmacs.cpp and gmacs.htp), calls ADMB to compile all
#' .cpp files required to build gmacs and creates the gmacs executable in the
#' folder specified by \code{Dir[vv]}. The executable will be "gmacs.exe" on windows
#' platforms and "gmacs" on linux-like (including MacOS) machines. Object files
#'  created during compilation from the additional cpp files will be in the \code{Dir}
#'  folder on Windows machines and in the \code{Dir/lib} folder on unix-like machines.
#'  These can be "cleaned" up using [clean_root()].
#'
#' @export
#' @md
#'
createGmacsExe <-
  function(vv,
           Dir,
           verbose = FALSE,
           logFiles = FALSE,
           ADMBpaths = NULL) {
    # Check directories for ADMB
    # Define the name of the file containing the different pathways needed to build
    # the GMACS executable
    suppressWarnings(PBSadmb::readADpaths(ADMBpaths))

    cat("\n Verifying the paths for ADMB, the C/C++ compiler and the editor ....\n")
    if (!PBSadmb::checkADopts())
      stop(
        "The definition of the pathways to locate ADMB,the C/C++ compiler and/or the editer are wrong.\nPlease check the ADMBpaths file."
      )

    oldWD = getwd()

    on.exit(setwd(oldWD))

    cat("--Setting working directory to '", Dir[vv], "' \n", sep = "")
    setwd(Dir[vv])
    on.exit(setwd(dirname(Dir[vv])))


    # Clean directory from previous version
    # tmp <- gmr::.CallTerm(command = "clean_root.bat",
    #                .Dir = Dir[vv],
    #                verbose = verbose)
    # rstudioapi::terminalKill(id = tmp)
    clean_root(path = Dir[vv])


    #  Create gmacs.tpl from gmacsbase.tpl and personal.tpl
    cat("Now writing gmacs.tpl\n")
    write_TPL(vv = vv,
              Dir = Dir[vv],
              .update = FALSE)
    # cat("\n")

    #--additional cpp files in "lib" subfolder of Dir[vv]
    #--header files are in "include" subfolder of Dir[vv]
    libFiles <-
      dir(
        file.path(Dir[vv], "lib"),
        "*.cpp",
        ignore.case = TRUE,
        all.files = TRUE,
        full.names = TRUE
      )
    if (isWindowsOS()) {
      #--for compilation, path to "include" folder assumes it is a subfolder relative to the cpp  files
      #--need to copy cpp files to Dir[vv] so include folder is subfolder of working directory
      file.copy(libFiles, Dir[vv], overwrite = TRUE)
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
    #      compFiles <- c("gmacs", libFiles)
    compFiles <- c("gmacs", args)
    for (nm in 1:length(compFiles)) {
      cat("Now compiling file ", nm, ": '", compFiles[nm], "'\n", sep = "")
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
      logfile = logFiles,
      add = FALSE,
      verbose = verbose,
      pathfile = NULL,
      args = args
    )
    cat("OK after building gmacs executable ...\n")
    cat("--Re-setting working directory to '", oldWD, "' \n", sep = "")
    #--setwd(oldWD) <-automatically does this on exit
  }
