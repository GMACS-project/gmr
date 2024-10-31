
#' @title insertTime
#'
#' @description Function to insert time and date of the last compilation in the
#' gmacs.tpl file.
#'
#' @param object (character string)- Input vector/table
#' @param pattern (character string)- pattern to look for.
#' @param .update Logical: is it to update a new version of GMACS? If TRUE,
#' the name of the new version of GMACS and the date of the compilation will be
#' modified in the gmacsbase.tpl file.
#' @param devel (logical)- flag to specify if this is a development version.
#' @param New_ver (character)- the development version number.
#'
#' @return List of object: the first object is the line wherein the text should
#' added (it corresponds to the line where the header is specified in gmacsbase.tpl).
#' The second object is the new header definition. If this is not an update of
#' GMACS, then the header of the gmacsbase.tpl file will not be modified but
#' the gmacs.tpl file will specify the date of the new compilation.
#'
#'
#' @export
#'
insertTime <- function(object = NULL,
                       pattern = NULL,
                       .update = NULL,
                       devel = FALSE,
                       New_ver = NULL) {
  header <- which(stringr::str_detect(object, pattern = pattern))
  object1 <- object[1:(header - 1)]
  txt.header <- object[header]

  if (!.update && !devel) {
    txt.header <-
      sub("Compiled", "Previous compilation on: ", txt.header)
    txt.header <-
      sub('");',
          paste("; Last compilation on:  ", Sys.time(), '");' , sep = ""),
          txt.header)
    object <-
      c(object1, txt.header, object[(header + 1):length(object)])
  } else if (devel) {

    Dev.ID <- NA
    txt.header <- stringr::str_squish(txt.header)

    while(is.na(Dev.ID)){
      text = "Please enter your initials (refer to the .tpl file)."
      Dev.ID <- svDialogs::dlgInput(message = text, default = "AEP")$res
      Sys.sleep(0.1)
    }
    txt.header <-
      paste('!! TheHeader = adstring("## GMACS DEVELOPMENT VERSION ',
            New_ver,"; ** ", Dev.ID,
            ' **; Initialized ',
            Sys.time(),
            '");',
            sep = "")
    txt.header <- stringr::str_squish(txt.header)
    txt.header <- paste(" ", txt.header, sep="")
    object <-
      c(object1, txt.header, object[(header + 1):length(object)])
  } else {
    txt.header <- .getVerGMACS()
    object <- list(header, txt.header)
  }
  return(object)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

#' @title insertTime2
#'
#' @description Function to insert time and date of the last compilation in the
#' gmacs.tpl file.
#'
#' @param object (character string) Input vector/table
#' @param pattern (character string) pattern to look for.
#' @param .update Logical: is it to update a new version of GMACS? If TRUE,
#' the name of the new version of GMACS and the date of the compilation will be
#' modified in the gmacsbase.tpl file.
#' @param devel (logical)- flag to specify if this is a development version.
#'
#' @return a list. The first object is the line wherein the text should
#' added (it corresponds to the line where the header is specified in gmacsbase.tpl).
#' The second object is the new header definition. If this is not an update of
#' GMACS, then the header of the gmacsbase.tpl file will not be modified but
#' the gmacs.tpl file will specify the date of the new compilation.
#'
#' @export
#'
insertTime2 <- function(object = NULL,
                        pattern = NULL,
                        .update = NULL,
                        devel = FALSE) {

  header <- which(stringr::str_detect(object, pattern = pattern))
  object1 <- object[1:(header - 1)]
  txt.header <- object[header]

  if (!.update && !devel) {
    txt.header <-
      sub("Compiled", "Previous compilation on: ", txt.header)
    txt.header <-
      sub('");',
          paste("; Last compilation on:  ", Sys.time(), '");' , sep = ""),
          txt.header)
    object <-
      c(object1, txt.header, object[(header + 1):length(object)])

  } else if (devel){
    txt.header <-
      sub("Initialized", "Compiled", txt.header)

  } else {

    New.ver <- NA
    Dev.ID <- NA
    txt.header <- stringr::str_squish(txt.header)

    Ex.vers <-
      sub(pattern = '!! TheHeader = adstring(\"## GMACS Version',
          replacement = ";Version ",
          x = txt.header,
          fixed = TRUE)
    Ex.vers <-
      sub(pattern = '\");',
          replacement = " ",
          x = Ex.vers,
          fixed = TRUE)
    Ex.vers <- stringr::str_split(string = Ex.vers, pattern = ';')[[1]][2]
    Ex.vers <- stringr::str_squish(Ex.vers)


    while (is.na(New.ver)) {
      text = "========================================\nYou've been modifying GMACS.\nIf these modifications are an update,
then keep the same version name as displayed in the dialog box. If these modifications constistute an updgrade, please, provide a name for the new version.\n========================================\n
The name of the new version has to be of the form to: 'Version X.YY.X'.\n
\n*** The last updated version number is indicated in the dialog box - Please update this number. ***"
      New.ver <- svDialogs::dlgInput(message = text, default = print(Ex.vers))$res
      Sys.sleep(0.1)
    }
    New.ver <- stringr::str_squish(New.ver)

    while(is.na(Dev.ID)){
      text = "Please enter your initials (refer to the .tpl file)."
      Dev.ID <- svDialogs::dlgInput(message = text, default = "AEP")$res
      Sys.sleep(0.1)
    }

    txt.header <-
      paste('!! TheHeader = adstring("## GMACS ',
            New.ver,"; ** ", Dev.ID,
            ' **; Compiled ',
            Sys.time(),
            '");',
            sep = "")
    txt.header <- stringr::str_squish(txt.header)
    txt.header <- paste(" ", txt.header, sep="")

    object <- list(header, txt.header)
  }
  return(object)
}
