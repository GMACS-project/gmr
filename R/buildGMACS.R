

#' @title .buildGMACS
#'
#' @description Function to build the GMACS executable.
#' It converts the gmacs.tpl code to a gmacs.cpp code, calls ADMB to compile all
#'  .cpp files required to build gmacs and returns the gmacs executable.
#'
#' @param prefix String name prefix (i.e., without extension) of the ADMB project;
#' here "gmacs".
#' @param args list of string names corresponding to the various .cpp libraries
#' required to build gmacs. These files are stored in the lib/ folder.
#' @param ADMBpaths (character string)- (filepath): absolute or relative to
#' current working directory path to file defining required ADMB paths.
#' The default is `NULL`.
#' @inheritParams PBSadmb::convAD
#'
#' @return nothing.
#'
#' @export
#'
.buildGMACS <-
  function (prefix = NULL,
            raneff = NULL,
            safe = NULL,
            dll = NULL,
            debug = NULL,
            logfile = NULL,
            add = NULL,
            verbose = NULL,
            pathfile = NULL,
            args = NULL,
            ADMBpaths = NULL)
  {
    # Internal function imported from PBSadmb
    .version <- function (x)
    {
      if (is.null(x) || is.numeric(x))
        return(x)
      xpc = strsplit(x, split = "\\.")[[1]]
      npc = !grepl("[[:alpha:]]", xpc)
      xnu = as.numeric(paste(xpc[npc], collapse = "."))
      return(xnu)
    }
    .normPath <- function(path, winslash, mustWork = FALSE) {
      normalizePath(path, winslash, mustWork)
    }
    .setPath <- function (pathfile)
    {
      path_sep <- .Platform$path.sep
      dir_sep <- ifelse(.Platform$OS.type == "windows", "\\", "/")
      if (!missing(pathfile) &&
          !is.null(pathfile) && file.exists(pathfile))
        PBSadmb::readADpaths(pathfile)
      admb_home <- PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "admbpath")
      admb_path <- paste(admb_home, "bin", sep = dir_sep)
      if (.Platform$OS.type == "windows") {
        gcc_path = paste(PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "gccpath"),
                         "bin",
                         sep = dir_sep)
        msys_path = PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "msysbin")
        wsys_path = paste(Sys.getenv()["SystemRoot"], "System32", sep = dir_sep)
        path <- paste(
          .normPath(admb_path, dir_sep),
          .normPath(msys_path, dir_sep),
          .normPath(gcc_path, dir_sep),
          .normPath(wsys_path, dir_sep),
          sep = path_sep
        )
      }
      else {
        sys_path <- Sys.getenv("PATH")
        path <- paste(.normPath(admb_path, dir_sep),
                      .normPath(sys_path, dir_sep),
                      sep = path_sep)
      }
      Sys.setenv(PATH = path)
      Sys.setenv(ADMB_HOME = gsub("/*$", "", gsub(
        "\\\\*$", "", .normPath(admb_home, dir_sep)
      )))
    }
    .callSys <- function (..., wait = TRUE)
    {
      dots = list(...)
      if (.Platform$OS.type == "windows") {
        if ("edit" %in% names(dots))
          dots[[1]] = paste("start \"\"", dots[[1]], sep = " ")
        out = shell(dots, intern = TRUE)
      }
      else {
        cmd <- unlist(list(...))
        if (wait == FALSE)
          out = system(cmd, wait = FALSE)
        else
          out = system(cmd, intern = TRUE)
      }
      invisible(out)
    }
    # ==============================================

    if (missing(prefix))
      stop("argument 'prefix' is missing, with no default")
    old_path <- Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH = old_path))
    admbpath <- PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "admbpath")
    ext <- ifelse(isWindowsOS(), ifelse(file.exists(
      paste(admbpath, "/bin/adlink.cmd", sep = "")
    ), ".cmd", ".bat"), "")
    prog <- paste("adlink", ext, sep = "")
    if (is.null(PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "admbver")))
      PBSadmb::setADver(gccver = NULL)
    admbvernum = PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "admbver")

    flags <- c()
    if (dll)
      flags[length(flags) + 1] <- "-d"
    if (debug)
      flags[length(flags) + 1] <- "-g"
    if (safe && admbvernum < 11)
      flags[length(flags) + 1] <- "-s"
    if (!safe && admbvernum >= 11)
      flags[length(flags) + 1] <- "-f"
    if (raneff)
      flags[length(flags) + 1] <- "-r"
    flags <- paste(flags, collapse = " ")
    prefix <-
      paste(prefix, paste0(args, " ", collapse = ""), sep = " ")
    cmd <- paste(prog, flags, prefix, sep = " ")
    if (isWindowsOS())
      cmd = shQuote(cmd)
    .setPath(pathfile)
    if (logfile & !add)
      PBSadmb::startLog(prefix)
    if (verbose)
      cat(cmd, "\n")

    out <- .callSys(cmd, wait = TRUE)
    out2 <- c(cmd, out)
    if (logfile) {
      PBSadmb::appendLog(prefix, out2)
    }
    if (verbose)
      cat(out, sep = "\n")
    invisible(out2)

    if (out2[6] == "Successfully built 'gmacs.exe'.") {
      #--works for windows. TODO:implement for OSX/Linux
      del <- c(dir(pattern = ".obj"),
               dir(pattern = ".cpp"),
               dir(pattern = ".htp"))

      file.remove(del)

    }
    if (logfile == TRUE)
      file.remove(dir(pattern = ".txt"))#--why "txt"? log files are .log
  }
