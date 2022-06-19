# On load hook
#
# This is a load hook that is called by R when the package is loaded. This should not be exported
# 
# 
.onLoad <- function(libname, pkgname)
{
  cat("\n")
  cat("=====================================================================\n")
  cat("A R package to work with GMACS:                                      \n")
  cat("A Generalized size-structured Assessment Model for Crustaceans       \n")
  cat(gmr.version())
  cat("For help see https://gmacs-project.github.io/gmr/                    \n")
  cat("=====================================================================\n")
  cat("\n")
}


