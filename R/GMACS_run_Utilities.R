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
  Inddat <- which(stringr::str_detect(tmp, "data"))
  namdat <- tmp[Inddat + 1]
  Indctl <- which(stringr::str_detect(tmp, "controlfile"))
  namctl <- tmp[Indctl + 1]
  Indprj <- which(stringr::str_detect(tmp, "project"))
  namprj <- tmp[Indprj + 1]
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
#
#
GMACS_term <-
  function(command = "gmacs.exe",
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
#'
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
