#' @title Write details of new implementations in the gmabase.TPL file
#'
#' @description This function ask the user to specify the details of the
#' new features implemented in the new version of GMACS.
#'
#' @param Dir (character string)- path of the folder name that holds the version
#' of GMACS that has been new developed.
#' @param updateGMACS (logical)- Flag specifying if the development version of GMACS
#' constitutes an update of the current version (in which case the version number
#' remains identical) or an upgrade version (in which case the version number
#' has to be incremented).
#'
#' @return Add descriptive lines at the end of the gmacsbase.tpl file to detail
#' the new features that have been implemented.
#'
#
NewGMACSFeat <- function(dirSrc, updateGMACS = NULL) {
  # 1. Read in the gmacsbase.tpl ----
  gmacsbase <- file.path(dirSrc, "gmacsbase.tpl")
  text <- readLines(gmacsbase)
  while (text[length(text)] == "") {
    text <- text[-c(length(text))]
  }
  # text <- c(text, "")
  unlink(gmacsbase, recursive = FALSE, force = TRUE)
  fs::file_create(gmacsbase)
  fileConn <- file(gmacsbase)
  writeLines(text = text, fileConn)
  close(fileConn)

  # 2. Get the number version ----
  header <-
    which(stringr::str_detect(text, pattern = " !! TheHeader"))
  Vers <- text[header]
  if (updateGMACS) {
    Vers <-
      sub(
        pattern = stringr::str_squish('!! TheHeader = adstring(\"## GMACS Version'),
        replacement = ";(Update GMACS version",
        x = Vers,
        fixed = TRUE
      )
  } else {
    Vers <-
      sub(
        pattern = stringr::str_squish('!! TheHeader = adstring(\"## GMACS Version'),
        replacement = ";(Upgrade GMACS to version",
        x = Vers,
        fixed = TRUE
      )
  }
  Vers <- stringr::str_split(string = Vers, pattern = ';')[[1]][2:3]
  Vers <- stringr::str_squish(Vers)
  Vers <-
    paste("//", Sys.Date(), Vers[2], paste0(Vers[1], ")"), sep = " ")

  Nimpl <- NA
  while (is.na(Nimpl)) {
    text = "========================================
You've been implementing new features in GMACS.\nPlease, provide the number of new
features you implemented.
========================================

You'll be then asked to provide details for each item. For example:

  - item 1: 'Added lots of diagnostic output when reading input files'
  - item 2: 'Added ECHOSTR, WriteCtlStr, WriteProjStr macros for 1'
  - item 3: 'Reformatted calc_relative_abundance in preparation for adding ability to handle immature data'

***
"
    Nimpl <- svDialogs::dlgInput(message = text,
                                 default = print("Number of new features"))$res
    Sys.sleep(0.1)
  }

  TxtImpl <- NULL

  while (is.null(TxtImpl)) {
    tmpTxtImpl <- NULL

    for (i in 1:Nimpl) {
      eval(parse(text = paste0(
        "Impl_", i, " <- readline(prompt = 'Item ", i, " : ')"
      )))

      eval(parse(text = paste0(
        "Impl_", i, " <- paste0('- ", i, ". ', Impl_", i, ")"
      )))

      eval(parse(text = paste0(
        "Imp_nchar <- base::nchar(Impl_", i, ")"
      )))

      maxChar <- ifelse(test = i == 1,
                        yes = 140 - 50,
                        no = 140)

      if (Imp_nchar > maxChar) {
        eval(parse(
          text = paste0(
            "Impl_",
            i,
            " <- stringr::str_wrap(Impl_",
            i,
            ", width = ",
            maxChar,
            ",
                                    indent = 0,
                                    exdent = 0,
                                    whitespace_only = TRUE)"
          )
        ))

        eval(parse(
          text = paste0(
            "tmp <- unlist(stringr::str_split(Impl_",
            i,
            ", pattern = '\n'))"
          )
        ))
        eval(parse(text = paste0("Impl_", i, " <- tmp")))
      }

      eval(parse(text = paste0(
        "tmpTxtImpl <- c(tmpTxtImpl, Impl_", i, ")"
      )))

      if (i == Nimpl)
        TxtImpl <- tmpTxtImpl
    }
    Sys.sleep(0.1)
  }

  Vers1 <-
    stringr::str_replace(string = Vers,
                         pattern = '"',
                         replacement = '')

  TxtImpl[1] <- paste(Vers1, TxtImpl[1], sep = " ")
  if (length(TxtImpl) > 1)
    for (i in 2:length(TxtImpl))
      TxtImpl[i] <- paste0("// ", TxtImpl[i])

  TxtImpl <-
    c("// ================================================ //",
      TxtImpl)

  # 2. Append the comment to gmacsbase.tpl
  cat(TxtImpl,
      sep = "\n",
      file = gmacsbase,
      append = TRUE)
}
