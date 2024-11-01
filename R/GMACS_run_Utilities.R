# @title get_nam
#
# @description This function returns a file name without its extension
#
# @param filenames Name of file
#
# @return character string: the file name
#
#
# @examples
# \dontrun{
# }
#
# Get file names without extension
get_nam <- function(filenames) {
  tmp <- c()
  for (i in 1:length(filenames))
    tmp <- c(tmp, unlist(strsplit(filenames, "\\.")[[i]][1]))
  return(tmp)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# @title Numeric Vectors
#
# @description Creates objects of type "numeric". Uses the basic function
# (\code{\link[base]{as.numeric}}).
#
# @param x object to be coerced
#
#
# @examples
# \dontrun{
# }
#
# as.numeric()
.an <- function(x) {
  return(as.numeric(x))
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# @title Numeric Vectors
#
# @description Creates objects of type "character". Uses the basic function
# (\code{\link[base]{as.character}}).
#
# @param x object to be coerced
#
# @examples
# \dontrun{
# }
#
# as.character()
.ac <- function(x) {
  out <- as.character(x)
  return(out)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# @title read_GMACS.dat
#
# @description Reads the gmacs.dat file and extract the names of input files
# for a given run.
#
# @param path Character string: directory and name of the gmacs.dat file
#
# @return list containing the names of the input files for GMACS
# (.dat, .ctl and .prj)
#
#
# @examples
# \dontrun{
# }
#
# Get file names without extension
read_GMACS.dat <- function(path) {
  tmp <- readLines(path)
  # Inddat <- which(stringr::str_detect(tmp, "data"))
  # namdat <- tmp[Inddat + 1]
  # Indctl <- which(stringr::str_detect(tmp, "controlfile"))
  # namctl <- tmp[Indctl + 1]
  # Indprj <- which(stringr::str_detect(tmp, "project"))
  # namprj <- tmp[Indprj + 1]
  Inddat <- which(stringr::str_detect(tmp, ".dat$"))
  namdat <- tmp[Inddat]
  Indctl <- which(stringr::str_detect(tmp, ".ctl$"))
  namctl <- tmp[Indctl]
  Indprj <- which(stringr::str_detect(tmp, ".prj$"))
  namprj <- tmp[Indprj]
  return(list(namdat, namctl, namprj))
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# @title GMACS_term
#
# @description Calls a R terminal to run the GMACS executable
#
# @param command Character string: the name of the GMACS executable
# ("gmacs.exe")
# @param .Dir Character string: the directory of the executable
# @param verbose Logical: if TRUE, report the shell call an its messages
# to the R console.
#
# @return the (unique) terminal identifier
#
GMACS_term <-
  function(command = NULL,
           .Dir = NULL,
           verbose = NULL) {
    termId <-
      rstudioapi::terminalExecute(command = command,
                                  workingDir = .Dir,
                                  show = verbose)
    return(termId)
  }

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

#' @title .CallTerm
#'
#' @description Calls a R terminal to run a specific executable
#'
#' @param command Character string: name of the command to run (.exe)
#' @param .Dir Character string: the directory of the executable
#' @param verbose Logical: if TRUE, report the shell call an its messages
#' to the R console.
#'
#' @return the (unique) terminal identifier
#'
#' @export
#'
.CallTerm <-
  function(command = command,
           .Dir = NULL,
           verbose = NULL) {
    termId <-
      rstudioapi::terminalExecute(command = command,
                                  workingDir = .Dir,
                                  show = verbose)
    return(termId)
  }

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

#'
#' @title Is the OS type windows?
#'
#' @description Function to determine if the OS type windows.
#'
#' @return TRUE/FALSE if OS is windows
#'
#' @export
#'
isWindowsOS<-function(){return(.Platform$OS.type=="windows")}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

#' @title Function to extract the Version number of GMACS
#'
#' @description Extract the version number and the compilation date from the gmacs.tpl
#'
#' @param DirTPL (character string)- the directory where the gmacsbase.TPL file
#' you are using for the stock assessment is hold.
#'
#' @return a named list with the variable \code{ver} that contains the version number
#' and \code{Comp} that holds the compilation date and time.
#'
#' @export
#'
GMACSversion <- function(DirTPL = NULL) {
  out <- list()

  oldWD = getwd()
  on.exit(setwd(oldWD))
  tmpDir <- DirTPL
  setwd(tmpDir)

  TPL <- readLines("gmacsbase.tpl")

  header <- which(stringr::str_detect(TPL, pattern = "!! TheHeader"))
  header <- TPL[header]

  header <- unlist(strsplit(header, "##"))[2]

  ver <- unlist(strsplit(header, ";"))[1]
  Comp <- unlist(strsplit(header, ";"))[2]
  Comp <- unlist(strsplit(Comp, '\\")', ))

  out$ver <- ver
  out$Comp <- Comp

  return(out)
}
