
# @title .getVerGMACS
#
# @description Function to ask the user the name of the new Version of GMACS 
# and specify the compilation date for the new gmacbase.TPL file when updating it.
#
# @return Character string: the user-defined name of the new GMACS version
#
#
# @examples
# \dontrun{
# }
# 
# 
.getVerGMACS <- function () {
  New.ver <- NA
  while (is.na(New.ver)) {
    text = "You've been modifying GMACS. Please, provide a name for the new version.\nIt should be similar to: 'Verison 2.01.A'"
    New.ver <- svDialogs::dlgInput(text, Sys.info())$res
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
