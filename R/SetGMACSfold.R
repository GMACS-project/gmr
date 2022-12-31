#' @title Create the needed folders to run GMACS
#'
#' @description Function to create empty folders needed to run GMACS: \code{/build},
#' \code{/build/debug}, \code{/build/release}, and a folder for the species (Spc) of
#' interest (\code{/build/Spc})
#'
#' @param vv - integer index in \code{Dir} indicating root folder to create these
#' base folders.
#' @param Dir - vector of root folder names
#' @param Spc - vector of species names
#' @param Sp - integer index in \code{Spc} indicating the name(s) of the species
#' for which the folder needs to be created. If \code{Sp = "all"}, then a
#' folder will be created for all species designated in \code{Spc}.
#' @param verbose - flag (TRUE/FALSE) to print processing information
#'
#' @return nothing
#'
#' @details Both the input and output files of GMACS will be contained in the
#' folder whose name corresponds to the species of interest. When starting from
#' scratch, you will need to provide this folder with all the assessment files
#' needed by GMACS (.dat, .ctl, and .prj files).
#'
#' @export
#' @md
#'
SetGMACSfold <- function(vv = NULL,
                         Dir = NULL,
                         Spc = NULL,
                         Sp = NULL,
                         verbose=FALSE){

  oldWD = getwd();
  on.exit(setwd(oldWD));
  if(verbose)
    cat("--Setting working directory to \n'", Dir[vv], "' \n",sep="")

  setwd(Dir[vv])
  # on.exit(setwd(dirname(Dir[vv])));

  # Create build directory
  dirbuild <- file.path(Dir[vv], "build")

  if (!dir.exists(dirbuild)){
    dir.create(file.path(dirbuild), recursive = TRUE)
    if(verbose)
      cat("--Creating the following directory \n'", dirbuild, "' \n",sep="")
  } else {
    cat("\n The 'build' folder already exists in \n'", Dir[vv], "' \n",sep="")
  }

  if (!dir.exists(file.path(dirbuild, 'debug'))){
    dir.create(file.path(dirbuild, 'debug'), recursive = TRUE)
    if(verbose)
      cat("--Creating the following directory \n'", file.path(dirbuild, 'debug'), "' \n",sep="")
  } else {
    cat("\n The 'debug' folder already exists in \n'", dirbuild, "' \n",sep="")
  }

  if (!dir.exists(file.path(dirbuild, 'release'))){
    dir.create(file.path(dirbuild, 'release'), recursive = TRUE)
    if(verbose)
      cat("--Creating the following directory \n'", file.path(dirbuild, 'release'), "' \n",sep="")
  } else {
    cat("\n The 'release' folder already exists in \n'", dirbuild, "' \n",sep="")
  }

  # Build folder for species
  if(Sp != 'all'){
    dirSp <- file.path(dirbuild,Spc[Sp])
    if (!dir.exists(dirSp)){
      dir.create(file.path(dirSp), recursive = TRUE)
      if(verbose)
        cat("\n--Creating the following directory \n'", dirSp, "' \n",sep="")
    } else {
      cat("\n The '",Spc[Sp],"' folder already exists in '", dirbuild, "' \n",sep="")
    }
  } else{
    for(nm in 1:length(Spc)){
      dirSp <- file.path(dirbuild,Spc[nm])
      if(nm == 1)
        cat("\n ")
      if (!dir.exists(dirSp)){
        dir.create(file.path(dirSp), recursive = TRUE)
        if(verbose)
          cat("--Creating the following directory \n'", dirSp, "' \n",sep="")
      } else {
        cat("\n The '",Spc[nm],"' folder already exists in '", dirbuild, "' \n",sep="")
      }
    }
  }
}
