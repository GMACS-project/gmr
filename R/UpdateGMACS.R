#' @title UpdateGMACS
#'
#' @description Function used when updating and releasing a new version of GMACS.
#' It allows to copy and paste all the GMACS files (for one or several stocks)
#' resulting from the Development version from this directory to the Latest_Version
#' directory.
#'
#' @param dirSrc (character string)- This is the path to the folder that has been
#' used to develop the new version of GMACS. Typically, this corresponds to the path
#' to the \code{Dvpt_Version} folder, if following the basic workflow for updating
#' GMACS.
#' @param dirNew (character string)- This is the path to the folder that will hold
#' the latest version of GMACS after the check steps. Typically, this corresponds
#' to the path to the \code{Latest_Version} folder, if following the basic workflow
#' for updating GMACS.
#'
#' @return Copy all the files from the development folder to the latest_version
#' folder and create the \code{GMACS_Version_details.txt} file that gives
#' specifications about the version of GMACS that is updating.
#'
#' @export
#'
#
UpdateGMACS <- function(dirSrc = NULL,
                        dirNew = NULL){
  fsep <- .Platform$file.sep

  # 1. Specify the new implementation in the gmacsbase.tpl file ----
  # Check if the changes constitute an update or an upgrade of the version
  # Read the detaile from the latest version
  GMACSdetails <- file.path(dirNew, "GMACS_Version_details.txt", fsep = fsep)
  text <- readLines(GMACSdetails)
  oldVer <- text[stringr::str_detect(string = text, pattern = "GMACS version")]
  oldVer <- stringr::str_squish(stringr::str_remove(string = stringr::str_squish(oldVer), pattern = "- GMACS version:"))[1]

  # Read the gmasbase.tpl for the new version
  gmacsbase <- file.path(dirSrc, "gmacsbase.tpl", fsep = fsep)
  text <- readLines(gmacsbase)
  # Get the number version ----
  header <-
    which(stringr::str_detect(text, pattern = " !! TheHeader"))
  Vers <- text[header]
  Vers <-
    sub(
      pattern = stringr::str_squish('!! TheHeader = adstring(\"## GMACS Version'),
      replacement = "",
      x = Vers,
      fixed = TRUE
    )
  Vers <- stringr::str_split(string = Vers, pattern = ';')[[1]][1]
  Vers <- stringr::str_squish(Vers)
  if(oldVer == Vers){
    updateVer <- TRUE
  } else {
    updateVer <- FALSE
  }

  NewGMACSFeat(dirSrc,
               updateGMACS = updateVer)

  # 2. Copy all files from the Dvpt_Version to the Latest_Version ----
  cop.files <- list.files(dirSrc)[!list.files(dirSrc) %in% ("build")]
  file.copy(
    from = file.path(dirSrc, c(cop.files)),
    to = dirNew,
    overwrite = TRUE,
    recursive = TRUE,
    copy.date = TRUE
  )

  stock.files <- list.files(file.path(dirSrc, "build", sep = ""))
  stock.files <- stock.files[!stock.files %in% c("debug", "release")]

  for (nm in 1:length(stock.files)) {
    # Clean the Latest_Version directory
    tmp <- file.path(dirNew, "build", stock.files[nm], fsep = fsep)

    while(length(list.files(path = tmp, recursive = FALSE))>15){
      Sys.sleep(0.1)
      clean_bat(path = tmp, verbose = FALSE)
    }

    # Clean the Dvpt_Version directory
    tmp <- file.path(dirSrc, "build/", stock.files[nm], sep = "")

    while(length(list.files(path = tmp, recursive = FALSE))>15){
      Sys.sleep(0.1)
      clean_bat(path = tmp, verbose = FALSE)
    }
    nam <- list.files(path = tmp)

    file.copy(
      from = file.path(tmp, nam),
      to = file.path(dirNew, "build/", stock.files[nm], sep = ""),
      overwrite = TRUE,
      recursive = TRUE,
      copy.date = TRUE
    )
  }
  GetVerSpec(Dir = dirNew)
}

