#' @title Write details of new implementations in the gmabase.TPL file
#'
#' @description This function ask the user to specify the details of the
#' new features implemented in the new version of GMACS.
#'
#' @param Dir (character string)- path of the folder name that holds the version
#' of GMACS that has been new developed.
#'
#' @return Add descriptive lines at the end of the gmacsbase.tpl file to detail
#' the new features that have been implemented.
#'
#
NewGMACSFeat <- function(dirSrc) {
  # 1. Read in the gmacsbase.tpl ----
  gmacsbase <- file.path(dirSrc, "gmacsbase.tpl")
  text <- readLines(gmacsbase)

  # 2. Get the number version ----
  header <-
    which(stringr::str_detect(text, pattern = " !! TheHeader"))
  Vers <- text[header]
  Vers <-
    sub(
      pattern = '!! TheHeader =  adstring(\"## GMACS Version',
      replacement = ";(Upgrade GMACS to version",
      x = Vers,
      fixed = TRUE
    )
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
\n***
You'll be then asked to provide details for each item. For example:
item 1: 'Added lots of diagnostic output when reading input files'
item 2: 'Added ECHOSTR, WriteCtlStr, WriteProjStr macros for 1'
item 3: 'Reformatted calc_relative_abundance in preparation for adding ability to handle immature data'\n
***
"
    Nimpl <- svDialogs::dlgInput(message = text,
                                 default = print("Number of new features"))$res
    Sys.sleep(0.1)
  }

  TxtImpl <- NA
  while (is.na(TxtImpl)) {
    for (i in 1:Nimpl) {
      eval(parse(text = paste0(
        "Impl_", i, " <- readline(prompt = 'Item ", i, " : ')"
      )))

      eval(parse(text = paste0(
        "Impl_", i, " <- paste0('- ", i, ". ', Impl_", i, ")"
      )))

    }
    if (i == Nimpl)
      eval(parse(text =
                   paste0(
                     'TxtImpl <-paste(',
                     paste(' Impl_', 1:Nimpl, sep = '', collapse = ','),
                     ')'
                   )))

    Sys.sleep(0.1)
  }
  TxtImpl <- paste(Vers, TxtImpl, sep = " ")
  TxtImpl <- paste(TxtImpl, "\n")

  # 2. Append the comment to gmacsbase.tpl
  cat(TxtImpl, file = gmacsbase, append = TRUE)
}
