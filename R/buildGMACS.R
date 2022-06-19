
# @title .buildGMACS
#
# @description Function to build the GMACS executable.
# It converts the gmacs.tpl code to a gmacs.cpp code, calls ADMB to compile all
#  .cpp files required to build gmacs and returns the gmacs executable.
# 
# @param prefix String name prefix (i.e., without extension) of the ADMB project,
# here "gmacs". 
# @param args list of string names corresponding to the various .cpp libraries 
# required to build gmacs. These files are stored in the lib/ folder.
# @inheritParams PBSadmb::convAD 
# 
# @return
#
# @examples
# \dontrun{
# }
# 
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
            args = NULL)
  {
    if (missing(prefix))
      stop("argument 'prefix' is missing, with no default")
    old_path <- Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH = old_path))
    admbpath <- PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "admbpath")
    ext <- ifelse(.Platform$OS.type == "windows",
                  ifelse(file.exists(
                    paste(admbpath, "/bin/adlink.cmd", sep = "")
                  ),
                  ".cmd", ".bat"),
                  "")
    prog <- paste("adlink", ext, sep = "")
    if (is.null(PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "admbver")))
      PBSadmb::setADver(gccver = NULL)
    admbvernum = .version(PBSmodelling::getOptions(PBSadmb::atcall(.PBSadmb), "admbver"))
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
    if (.Platform$OS.type == "windows")
      cmd = shQuote(cmd)
    .setPath(pathfile)
    if (logfile & !add)
      startLog(prefix)
    if (verbose)
      cat(cmd, "\n")
    
    out <- PBSadmb::.callSys(cmd, wait = TRUE)
    out2 <- c(cmd, out)
    if (logfile) {
      appendLog(prefix, out2)
    }
    if (verbose)
      cat(out, sep = "\n")
    invisible(out2)
    
    if (out2[6] == "Successfully built 'gmacs.exe'.")
      del <-
      c(dir(pattern = ".obj"),
        dir(pattern = ".cpp"),
        dir(pattern = ".htp"))
    file.remove(del)
    if (logfile == TRUE)
      file.remove(dir(pattern = ".txt"))
  }
