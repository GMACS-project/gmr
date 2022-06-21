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
  suppressPackageStartupMessages
}

# No Visible Bindings
# ===================
utils::globalVariables(
  names = c(
    ".ADMBpaths",
    ".PBSadmb",
    ".FLEET",
    '.MATURITY',
    '.OVERLAY',
    ".SEAS",
    ".SEX",
    '.SHELL',
    '.THEME',
    '.TYPE',
    'Density',
    'Knot',
    'Length',
    'M',
    'MP',
    'Maturity',
    'Model',
    'N',
    'Postmolt',
    'Premolt',
    'Sex',
    'Shell',
    'Weight',
    'Year',
    'cpue',
    "fleet",
    "type",
    "year",
    'ssb',
    "ub",
    "lb",
    "ube",
    "lbe",
    "pred",
    "resd",
    "variable",
    "value",
    "molt_inc",
    "log_rec",
    "modname",
    "maturity",
    "mid_points",
    "model",
    "sex",
    "o",
    "observed",
    "predicted"
  )
)

