#' Read .par file - no version dependent
#'
#' @param Dir (path)- repertory where the 'gmacs.par' file is saved
#' @param FileName (character string)- name of the file to be read. By default
#' \code{FileName = "gmacs.par"}
#'
#' @return object of type 'list' the parameters as list elements
#' @export
#'
readParFile_Allversion <- function(Dir = NULL, FileName = "gmacs.par") {
  FileName2 <- file.path(Dir, FileName)
  dat <- readLines(FileName2, warn = FALSE)
  
  DatOut <- list()
  
  # Get the number of parameters, the value of the objective function and the
  # Maximum gradient component
  Specs <- dat[1]
  if (stringr::str_detect(string = Specs, pattern = "# Number of parameters")) {
    getSpecs <-
      stringr::str_extract_all(Specs, "[-+.e0-9]*\\d", simplify = T)
    DatOut[["Nparams"]] <- getSpecs[1]
    DatOut[["ObjFvalue"]] <- getSpecs[2]
    DatOut[["MaxGradComp"]] <- getSpecs[3]
  } else {
    which1 <- dat[stringr::str_detect(string = dat, pattern = "# Number of parameters")]
    DatOut[["Nparams"]] <-
      .an(stringr::str_extract_all(which1, "[-+.e0-9]*\\d", simplify = T))
    which2 <- dat[stringr::str_detect(string = dat, pattern = "# Objective function value")]
    DatOut[["ObjFvalue"]] <-
      .an(stringr::str_extract_all(which2, "[-+.e0-9]*\\d", simplify = T))
    which3 <- dat[stringr::str_detect(string = dat, pattern = "# Maximum gradient component")]
    DatOut[["MaxGradComp"]] <-
      .an(stringr::str_extract_all(which3, "[-+.e0-9]*\\d", simplify = T))
  }

  # Check the available parameters:
  # Get the name of the available param
  availParam <- dat[which(stringr::str_detect(string = dat, pattern = "#.*"))]
  availParam <- availParam[-1]
  availParam <- gsub("# |:", "", availParam)
  
  # Get the values
  values <- dat[-c(which(stringr::str_detect(string = dat, pattern = "#.*")))]
  values  <- gsub('[^[:digit:][:blank:].]', '', values, perl = TRUE) #remove all characters except numbers and blanks
  values <- strsplit(values, '[[:blank:]]')#split based on blanks
  values <- lapply(
    values,
    FUN = function(x) {
      return(x[x != ''])
    }
  )
  # Make a named list
  Param <- NULL
  for (x in 1:length(availParam)) {
    Param[[availParam[x]]] <- .an(values[[x]][1:length(values[[x]])])
  }
  return(Param)
}
