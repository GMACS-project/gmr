#' @title write_TPL
#'
#' @description Function to write the gmacs.TPL file from gmacsbase.tpl and
#' personnal.tpl.
#'
#' @param vv (Numeric)- indicate the version - loop on the length(Dir).
#' @param Dir (character vector)- hold the directories for the versions of
#' GMACS considered in the analysis.
#' @param .update (logical)- Flag to specify if this is to update a new version
#' of GMACS. If TRUE, the name of the new version of GMACS and the date of the
#' compilation will be modified in the `gmacsbase.tpl` file.
#' @param End_Devel (logical)- Flag to specify if gmacs.tpl is written at the end
#' of a development period. Used when updating Gmacs.
#'
#' @return a new gmacs.tpl file corresponding to the merging of gmacsbase.tpl
#' and personnal.tpl files. This gmacs.tpl will then be used to build the GMACS
#' executable.
#'
#' @export
#'
write_TPL <- function(vv = NULL,
                      Dir = NULL,
                      .update = NULL,
                      End_Devel = FALSE) {
  # Internal function ----

  # Change the header to update/upgrade Gmacs
  changeHeader <- function(inFile) {
    text <- readLines(inFile)
    # Get the number version ----
    header <-
      which(stringr::str_detect(text, pattern = " !! TheHeader"))
    Vers <- text[header]

    # Change the header in inFile if applicable
    if (stringr::str_detect(string = Vers, pattern = "DEVELOPMENT VERSION")) {
      unlink(inFile, recursive = FALSE, force = FALSE)
      fs::file_create(inFile)

      Vers <- gsub(pattern = "## GMACS DEVELOPMENT VERSION ",
                   replacement = "## GMACS Version ",
                   x = Vers)
      Vers <- gsub(pattern = "Initialized",
                   replacement = "Compiled",
                   x = Vers)
      text[header] <- Vers

      add.text <- paste0(text, collapse = "\n")
      add.text <- c(add.text, "\n", "")
      fileConn <- file(inFile)
      writeLines(text = add.text, fileConn)
      close(fileConn)
      return(Vers)
    }
  }
  # --------------------------------------------------------------------
  # --------------------------------------------------------------------
  gmacsbase <- file.path(Dir[vv], "gmacsbase.tpl")
  if (file.exists(gmacsbase) == FALSE)
    stop(cat("\ngmacsbase.tpl does not exist\n"))
  personal <- file.path(Dir[vv], "personal.tpl")
  if (file.exists(personal) == FALSE)
    stop(cat("\npersonal.tpl does not exist\n"))
  gmacs <- file.path(Dir[vv], "gmacs.tpl")

  if (End_Devel) {
    # Read the gmacs.tpl to get the last compilation date
    header <- changeHeader(gmacs)
  }

  fs::file_create(gmacs)

  # Write gmacsbase.tpl
  if (.update) {
    add.text <- readLines(gmacsbase)
    while (add.text[length(add.text)] == "") {
      add.text <- add.text[-c(length(add.text))]
    }
    # text <- c(text, "")
    unlink(gmacsbase, recursive = FALSE, force = TRUE)
    fs::file_create(gmacsbase)

    Insert <-
      insertTime2(object = add.text,
                  pattern = ' !! TheHeader',
                  .update = TRUE)
    header <- Insert[[1]]
    txt.header <- Insert[[2]]

    fileConn <- file(gmacsbase)
    writeLines(text = paste0(c(add.text[1:(header - 1)], txt.header, add.text[(header + 1):length(add.text)]), collapse = "\n"), fileConn)
    close(fileConn)
  }

  # Write gmacs.tpl
  add.text <- readLines(gmacsbase)
  while (add.text[length(add.text)] == "") {
    add.text <- add.text[-c(length(add.text))]
  }
  # text <- c(text, "")
  if (!.update && !End_Devel) {
    add.text <-
      insertTime(object = add.text,
                 pattern = ' !! TheHeader',
                 .update = FALSE)
  }
  if (End_Devel) {
    # Get the number version ----
    header_pos <-
      which(stringr::str_detect(add.text, pattern = " !! TheHeader"))
    add.text[header_pos] <- header
  }
  add.text <- paste0(add.text, collapse = "\n")
  add.text <- c(add.text, "\n", "")

  add.text2 <- readLines(personal)
  while (add.text2[length(add.text2)] == "") {
    add.text2 <- add.text2[-c(length(add.text2))]
  }
  add.text2 <- paste0(add.text2, collapse = "\n")
  add.text2 <- c(add.text2, "\n", "")

  fileConn <- file(gmacs)
  writeLines(text = paste0(c(add.text, add.text2), collapse = "\n"), fileConn)
  close(fileConn)
  if (!End_Devel)
    cat("gmacs.tpl was created\n")
}
