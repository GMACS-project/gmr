
# @title insertTime
#
# @description Function to insert time and date of the last compilation in the 
# gmacs.tpl file.
# 
# @param object Character string: Input vector/table
# @param pattern Character string: pattern to look for.
# @param .update Logical: is it to update a new version of GMACS? If TRUE,
# the name of the new version of GMACS and the date of the compilation will be 
# modified in the gmacsbase.tpl file. 
#
# @return List of object: the first object is the line wherein the text should
# added (it corresponds to the line where the header is specified in gmacsbase.tpl).
# The second object is the new header definition. If this is not an update of 
# GMACS, then the header of the gmacsbase.tpl file will not be modified but
# the gmacs.tpl file will specify the date of the new compilation.
# 
#
# @examples
# \dontrun{
# }
# 
# Insert Time and Date in the new gmacs.tpl compiled
insertTime <- function(object = NULL,
                       pattern = NULL,
                       .update = NULL) {
  header <- which(stringr::str_detect(object, pattern = pattern))
  object1 <- object[1:(header - 1)]
  txt.header <- object[header]
  
  if (!.update) {
    txt.header <-
      sub("Compiled", "Previous compilation on: ", txt.header)
    txt.header <-
      sub('");',
          paste("; Last compilation on:  ", Sys.time(), '");' , sep = ""),
          txt.header)
    object <-
      c(object1, txt.header, object[(header + 1):length(object)])
  } else {
    txt.header <- gmr::.getVerGMACS()
    object <- list(header, txt.header)
  }
  return(object)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# @title insertTime2
#
# @description Function to insert time and date of the last compilation in the 
# gmacs.tpl file.
# 
# @param object Character string: Input vector/table
# @param pattern Character string: pattern to look for.
# @param .update Logical: is it to update a new version of GMACS? If TRUE,
# the name of the new version of GMACS and the date of the compilation will be 
# modified in the gmacsbase.tpl file. 
#
# @return List of object: the first object is the line wherein the text should
# added (it corresponds to the line where the header is specified in gmacsbase.tpl).
# The second object is the new header definition. If this is not an update of 
# GMACS, then the header of the gmacsbase.tpl file will not be modified but
# the gmacs.tpl file will specify the date of the new compilation.
# 
#
# @examples
# \dontrun{
# }
# 
# Insert Time and Date in the new gmacs.tpl compiled
insertTime2 <- function(object = NULL,
                        pattern = NULL,
                        .update = NULL) {
  header <- which(stringr::str_detect(object, pattern = pattern))
  object1 <- object[1:(header - 1)]
  txt.header <- object[header]
  
  if (!.update) {
    txt.header <-
      sub("Compiled", "Previous compilation on: ", txt.header)
    txt.header <-
      sub('");',
          paste("; Last compilation on:  ", Sys.time(), '");' , sep = ""),
          txt.header)
    object <-
      c(object1, txt.header, object[(header + 1):length(object)])
  } else {
    New.ver <- NA
    
    while (is.na(New.ver)) {
      text = "You've been modifying GMACS. Please, provide a name for the new version.\nIt should be similar to: 'Verison 2.01.A'"
      New.ver <- svDialogs::dlgInput(text, Sys.info())$res
      Sys.sleep(0.1)
    }
    txt.header <-
      paste(' !! TheHeader =  adstring("## GMACS ',
            New.ver,
            '; Compiled ',
            Sys.time(),
            '");',
            sep = "")
    object <- list(header, txt.header)
  }
  return(object)
}
