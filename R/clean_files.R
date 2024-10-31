#' @title Clean files in a folder
#'
#' @description Function to clean files in a folder.
#'
#' @param path (path)- path to the root folder to be cleaned.
#' @param filename (character vector)- file name(s) to be removed.
#' @param names (character vector)- character strings representing the files in
#' the root folder to be removed.
#' @param verbose (logical)- flag to print file names being deleted
#'
#' @return nothing
#'
#' @details removes (deletes) files with given names on given path.
#'
#' @export
#'
clean_files <- function(path = ".",
                        filename = NULL,
                        names = c("no_file"),
                        verbose = FALSE) {
  res <- NULL
  for (name in names) {
    if(!is.null(filename)){
      fns <- file.path(path, filename)
    } else {
      fns = list.files(path = path,
                       pattern = glob2rx(name),
                       full.names = TRUE)
    }
    if (length(fns) > 0) {
      if (verbose)
        cat("--Removing files:\n", paste0("\t-", basename(fns), collapse = "\n"))

      res[[name]] <- 1
      while (res[[name]] == 1) {
        res[[name]] <- unlink(fns)
      }
    }
  }
}

#'
#' @title Clean gmacs compilation-process files in the "root folder"
#'
#' @description Function to clean gmacs compilation-process files in the "root folder".
#'
#' @param path character string or file path representing root (default: ".")
#' @param verbose (TRUE/FALSE) flag to print file names being deleted
#'
#' @return nothing
#'
#' @seealso [clean_files()]
#'
#' @details
#' Deletes files matching \code{"\*.obj"},\code{"\*.cpp"},\code{"\*.htp"},
#' \code{"gmacs.exe"},and \code{"gmacs.tpl"} in the folder given by \code{path}.
#'
#' @export
#'
clean_root <- function(path = ".", verbose = TRUE) {
  names = c(
    "*.obj",
    "*.cpp",
    "*.htp",
    "gmacs.exe",
    "gmacs.tpl",
    "Error_compilation.txt",
    "Error_convertion.txt"
  )

  clean_files(path = path,
              names = names,
              verbose = verbose)

}


#' @title Clean gmacs output files in a folder
#'
#' @description Function to clean gmacs output files in a folder specified by \code{path}.
#' By default, this is the current working directory.
#'
#' @param path character string or file path representing root (default: ".")
#' @param verbose (TRUE/FALSE) flag to print file names being deleted
#'
#' @return nothing
#'
#' @seealso [clean_files()]
#'
#' @details
#' Deletes files matching \code{"admodel.\*"},\code{"\*.bar"},\code{"\*.eva"},
#' \code{"fmin.log"},\code{"\*.log"},\code{"\*.std"},\code{"gradient.\*"},
#' \code{"\*.r0\*"},\code{"\*.p0\*"},\code{"\*.b0\*"},\code{"derivatives"},
#' \code{"ders.dat"},\code{"mcout\*.\*"},\code{"checkfile.rep"},
#' \code{"personal.rep"},\code{"gmacs_in.\*"},\code{"gmacs_files_in.dat"},
#' \code{"\*.exe"}.
#'
#' @export
#'
clean_bat <- function(path = ".", verbose = TRUE) {
  names = c(
    "admodel.*",
    "*.bar",
    "*.eva",
    "fmin.log",
    "*.log",
    "*.std",
    "gradient.*",
    "*.r0*",
    "*.p0*",
    "*.b0*",
    "derivatives",
    "ders.dat",
    "mcout*.*",
    "checkfile.rep",
    "personal.rep",
    "gmacs_in.*",
    "gmacs_out.*",
    "gmacs_files_in.dat",
    "*.exe"
  )
  clean_files(path = path,
              names = names,
              verbose = verbose)
}

#' @title Clean the output files of the Gmacs simulation module
#'
#' @description Function to clean gmacs output files in a folder when a
#' simulation-estimation approach has been considered.
#'
#' @param path (character string)- names or file path representing root (default: ".")
#' @param verbose (TRUE/FALSE) flag to print file names being deleted
#'
#' @return nothing
#'
#' @seealso [clean_files()]
#'
#' @details
#' Deletes files matching \code{"admodel.\*"},\code{"\*.bar"},\code{"\*.eva"},
#' \code{"fmin.log"},\code{"\*.log"},\code{"\*.std"},\code{"gradient.\*"},
#' \code{"\*.r0\*"},\code{"\*.p0\*"},\code{"\*.b0\*"},\code{"derivatives"},
#' \code{"ders.dat"},\code{"mcout\*.\*"},\code{"checkfile.rep"},\code{"personal.rep"},
#' \code{"gmacs_in.\*"},\code{"gmacs_files_in.dat"},\code{"\*.exe"},\code{"\*.a"},
#' \code{"gmacs.par"},\code{"gmacs.dat"},\code{"gmacs.rep"},\code{"simdata.out"}.
#'
#' @export
#'
clean_bat_Sim <- function(path = ".", verbose = TRUE) {
  names = c(
    "admodel.*",
    "*.bar",
    "*.eva",
    "fmin.log",
    "*.log",
    "*.std",
    "gradient.*",
    "*.r0*",
    "*.p0*",
    "*.b0*",
    "derivatives",
    "ders.dat",
    "mcout*.*",
    "checkfile.rep",
    "personal.rep",
    "gmacs_in.*",
    "gmacs_out.*",
    "gmacs_files_in.dat",
    "*.exe",
    "*.a",
    "gmacs.par",
    "gmacs.dat",
    "gmacs.rep",
    "simdata.out"
  )

  clean_files(path = path,
              names = names,
              verbose = verbose)

}

#' @title Clean Gmacs input files in a folder
#'
#' @description Function to clean Gmacs input files in a specifc folder.
#'
#' @param path (character string; path)- the root folder to be cleaned
#' @param verbose (logical)- flag to print filenames being deleted
#'
#' @return nothing
#'
#' @details removes (deletes) Gmacs input files based on those available in the
#' 'gmacs.dat' file.
#'
#' @export
#'
clean_Inputfiles <- function(path = ".", verbose = FALSE) {
  fileName <- "gmacs.dat"
  fileName <- file.path(path, fileName, fsep = fsep)
  if (!file.exists(fileName)) {
    test1 <- paste(
      "The 'gmacs.dat' file does not exist in:\n",
      file.path(path, fsep = fsep),
      "\n\t-> Can't read input files.\n\n"
    )
    stop(test1)
  } else {
    if (verbose)
      cat("\n")
    if(inherits(
      try( readGMACS.dat(path = fileName, verbose = verbose), silent=TRUE),  "try-error")){
      GMACSdat <- readLines(fileName)
      dat_pos <- which(stringr::str_detect(string = out, pattern = "# Data file name"))+1
      ctl_pos <- which(stringr::str_detect(string = out, pattern = "# Control file name"))+1
      prj_pos <- which(stringr::str_detect(string = out, pattern = "# Projection file name"))+1
      To_del <- c(basename(fileName), GMACSdat[dat_pos], GMACSdat[ctl_pos], GMACSdat[prj_pos])
    } else {
      GMACSdat <- readGMACS.dat(path = fileName, verbose = verbose)
      To_del <- c(basename(fileName), GMACSdat[["DatFileName"]], GMACSdat[["CtlFileName"]], GMACSdat[["PrjFileName"]])
    }
  }
  if(verbose)
    cat(
      "\t--> In \n",
      file.path(path, fsep = fsep),
      "\n Deleting the following files:\n",
      paste("\t-", To_del, collapse = "\n")
    )

  res <- NULL
  for (f in To_del) {
    res[[f]] <- 1
    while (res[[f]] == 1) {
      res[[f]] <- unlink(file.path(path, f, fsep = fsep),
                         recursive = TRUE,
                         force = TRUE,)
    } # End while condition
  } # End loop on To_del
}
