
#' @title Give a new version name
#
#' @description Function to ask the user the name of the new Version of GMACS
#' and specify the compilation date for the new gmacbase.TPL file when updating it.
#'
#' @return Character string: the user-defined name of the new GMACS version
#'
#
.getVerGMACS <- function () {
  New.ver <- NA
  while (is.na(New.ver)) {
    text = "========================================\nYou've been modifying GMACS.\nPlease, provide a name for the new version.\n========================================\nThe name of the new version has to be of the form to: 'Version X.YY.X'.\n
\n*** The last updated version number is indicated in the dialog box - Please update this number.***"
    # New.ver <- svDialogs::dlgInput(message = text, Sys.info())$res
    New.ver <- svDialogs::dlgInput(message = text, default = print(""))$res
    Sys.sleep(0.1)
  }
  New.ver <-
    paste(' !! TheHeader =  adstring("## GMACS ',
          New.ver,
          '; Compiled ',
          Sys.time(),
          '");',
          sep = "")
  return(New.ver)
}


#' @title Get the specifications of the latest version
#'
#' @description This function creates the GMACS_Version_details.txt file holding
#' that holds the specifications of the latest version of GMACS. This function
#' is called by the UpdateGMACS() function when a developer wants to udpate GMACS
#' before submitting a pull request for a potential new release.
#'
#' @param Dir (character string)- path of the folder name that holds the version of
#' GMACS that is updated.
#'
#' @return Creates the GMACS_Version_details.txt which is store in the Latest_version
#' folder.
#
GetVerSpec <- function(Dir = NULL){

  gmacsbase <- file.path(Dir, "gmacsbase.tpl")
  out <- file.path(Dir, "GMACS_Version_details.txt")
  text <- readLines(gmacsbase)
  unlink(out)
  fs::file_create(path = out)

  # 1. Get the number version
  header <- which(stringr::str_detect(text, pattern = " !! TheHeader"))
  Vers <- text[header]

  # 2. Get the last comment
  LastComm <- which(stringr::str_detect(text, pattern = "// ================================================ //"))
  LastComm <- LastComm[length(LastComm)]
  LastComm2 <- which(stringr::str_detect(text, pattern = "//"))
  LastComm2 <- LastComm2[length(LastComm2)]
  LastComm <- text[(LastComm+1):LastComm2]

  Vers <-
    sub(pattern = ' !! TheHeader = adstring(\"## GMACS Version',
        replacement = ";- GMACS version: ",
        x = Vers,
        fixed = TRUE)
  Vers <-
    sub(pattern = '\");',
        replacement = " ",
        x = Vers,
        fixed = TRUE)
  Vers <- stringr::str_split(string = Vers, pattern = ';')[[1]][2:4]
  Vers[3] <- sub(pattern = ' Compiled ',
                 replacement = "- Last compilation date: ",
                 x = Vers[3],
                 fixed = TRUE)

  # Auth_ID <- unlist(strsplit(LastComm,split = ' ** ',fixed = TRUE))[2]
  Auth_ID <- unlist(strsplit(Vers[2],split = ' ** ',fixed = TRUE))[2]
  Auth_ID <- unlist(strsplit(Auth_ID,split = ' **',fixed = TRUE))

  Auth_nam <- which(stringr::str_detect(text, pattern = paste("// ",Auth_ID,":", sep="")))
  Auth_nam <- text[Auth_nam[1]]
  Auth_nam <- sub(pattern = paste("// ",Auth_ID, sep=""),
                  replacement = "- Last update(s) have been made by",
                  x = Auth_nam)

  fileConn <- file(out)
  writeLines(text = paste0(Vers[1],"\n",
                           Vers[3],"\n",
                           Auth_nam,"\n",
                           "\n",
                           "- Details about the last modification:","\n",
                           paste(LastComm, collapse = "\n"),
                           sep = ""), fileConn)
  # writeLines(text = paste(LastComm, collapse = "\n"), fileConn)
  close(fileConn)

}

