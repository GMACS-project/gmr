#' @title write_TPL
#'
#' @description Function to write the gmacs.TPL file from gmacsbase.tpl and
#' personnal.tpl.
#'
#' @param vv Numeric: indicate the version - loop on the length(Dir).
#' @param Dir list of Character string: hold the directories for the versions of
#' GMACS considered in the analysis.
#' @param .update Logical: is it to update a new version of GMACS? If TRUE,
#' the name of the new version of GMACS and the date of the compilation will be
#' modified in the gmacsbase.tpl file.
#'
#' @return a new gmacs.tpl file corresponding to the merging of gmacsbase.tpl
#' and personnal.tpl files. This gmacs.tpl will then be used to build the GMACS
#' executable.
#'
#' @export
#'
write_TPL <- function(vv = NULL,
                      Dir = NULL,
                      .update = NULL) {
  gmacsbase <- file.path(Dir[vv], "gmacsbase.tpl")
  if (file.exists(gmacsbase) == FALSE)
    stop(cat("\ngmacsbase.tpl does not exist\n"))
  personal <- file.path(Dir[vv], "personal.tpl")
  if (file.exists(personal) == FALSE)
    stop(cat("\npersonal.tpl does not exist\n"))
  gmacs <- file.path(Dir[vv], "gmacs.tpl")
  fs::file_create(gmacs)

  if (.update) {
    add.text <- readLines(gmacsbase)
    unlink(gmacsbase, recursive = FALSE, force = FALSE)
    fs::file_create(gmacsbase)

    Insert <-
      insertTime2(object = add.text,
                       pattern = ' !! TheHeader',
                       .update = TRUE)
    # Insert <- insertTime(object = add.text, pattern = ' !! TheHeader', .update = TRUE)
    header <- Insert[[1]]
    txt.header <- Insert[[2]]

    fileConn <- file(gmacsbase)
    writeLines(text = paste0(c(add.text[1:(header - 1)],
                               txt.header, add.text[(header + 1):length(add.text)]),
                             collapse = "\n"), fileConn)
    close(fileConn)
  }

  add.text <- readLines(gmacsbase)
  if (!.update)
    add.text <-
    insertTime(object = add.text,
                    pattern = ' !! TheHeader',
                    .update = FALSE)
  add.text <- paste0(add.text, collapse = "\n")
  add.text <- c(add.text, "\n", "")

  add.text2 <- readLines(personal)
  add.text2 <- paste0(add.text2, collapse = "\n")
  add.text2 <- c(add.text2, "\n", "")

  fileConn <- file(gmacs)
  writeLines(text = paste0(c(add.text, add.text2), collapse = "\n"), fileConn)
  close(fileConn)

  cat("gmacs.tpl was created\n")
}
