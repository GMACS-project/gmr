#'
#' @title Clean files in a folder
#'
#' @description Function to clean files in a folder.
#'
#' @param path character string or file path representing the root folder to be cleaned
#' @param names vector of character strings representing files in the root folder to be removed
#' @param verbose (TRUE/FALSE) flag to print filenames being deleted
#'
#' @return nothing
#'
#' @details removes (deletes) files with given names on given path.
#'
#' @export
#'
clean_files<-function(path=".",
                     names=c("no_file"),
                     verbose=FALSE){
  for (name in names){
    fns = list.files(path=path,pattern=glob2rx(name),full.names=TRUE);
    if (length(fns)>0) {
      if (verbose) cat("--Removing files:\n",paste0("\t",fns,"\n"));
      file.remove(fns);
    }
  }
}

#'
#' @title Clean gmacs compilation-process files in the "root folder"
#'
#' @description Function to clean gmacs compilation-process files in the "root folder".
#'
#' @param path character string or file path representing root (default: ".")
#' @param verbose (TRUE/FALSE) flag to print file names being deleted
#'
#' @return nothing
#'
#' @seealso [clean_files()]
#'
#' @details
#' Deletes files matching \code{"\*.obj"},\code{"\*.cpp"},\code{"\*.htp"},
#' \code{"gmacs.exe"},and \code{"gmacs.tpl"} in the folder given by \code{path}.
#'
#' @export
#'
clean_root <- function(path=".",
                     verbose=TRUE){
  names=c("*.obj","*.cpp","*.htp","gmacs.exe","gmacs.tpl");
  clean_files(path=path,names=names,verbose=verbose);
}


#' @title Clean gmacs output files in a folder
#'
#' @description Function to clean gmacs output files in a folder specified by \code{path}.
#' By default, this is the current working directory.
#'
#' @param path character string or file path representing root (default: ".")
#' @param verbose (TRUE/FALSE) flag to print file names being deleted
#'
#' @return nothing
#'
#' @seealso [clean_files()]
#'
#' @details
#' Deletes files matching \code{"admodel.\*"},\code{"\*.bar"},\code{"\*.eva"},
#' \code{"fmin.log"},\code{"\*.log"},\code{"\*.std"},\code{"gradient.\*"},
#' \code{"\*.r0\*"},\code{"\*.p0\*"},\code{"\*.b0\*"},\code{"derivatives"},
#' \code{"ders.dat"},\code{"mcout\*.\*"},\code{"checkfile.rep"},
#' \code{"personal.rep"},\code{"gmacs_in.\*"},\code{"gmacs_files_in.dat"},
#' \code{"\*.exe"}.
#'
#' @export
#'
clean_bat<-function(path=".",verbose=TRUE){
  names = c("admodel.*","*.bar","*.eva","fmin.log","*.log","*.std","gradient.*",
            "*.r0*","*.p0*","*.b0*","derivatives","ders.dat","mcout*.*","checkfile.rep",
            "personal.rep","gmacs_in.*","gmacs_files_in.dat","*.exe");
  clean_files(path=path,names=names,verbose=verbose);
}

#' @title Clean the output files of the Gmacs simulation module
#'
#' @description Function to clean gmacs output files in a folder when a
#' simulation-estimation approach has been considered.
#'
#' @param path (character string)- names or file path representing root (default: ".")
#' @param verbose (TRUE/FALSE) flag to print file names being deleted
#'
#' @return nothing
#'
#' @seealso [clean_files()]
#'
#' @details
#' Deletes files matching \code{"admodel.\*"},\code{"\*.bar"},\code{"\*.eva"},
#' \code{"fmin.log"},\code{"\*.log"},\code{"\*.std"},\code{"gradient.\*"},
#' \code{"\*.r0\*"},\code{"\*.p0\*"},\code{"\*.b0\*"},\code{"derivatives"},
#' \code{"ders.dat"},\code{"mcout\*.\*"},\code{"checkfile.rep"},\code{"personal.rep"},
#' \code{"gmacs_in.\*"},\code{"gmacs_files_in.dat"},\code{"\*.exe"},\code{"\*.a"},
#' \code{"gmacs.par"},\code{"gmacs.dat"},\code{"gmacs.rep"},\code{"simdata.out"}.
#'
#' @export
#'
clean_bat_Sim <- function(path=".",verbose=TRUE){
  names = c("admodel.*","*.bar","*.eva","fmin.log","*.log","*.std","gradient.*",
            "*.r0*","*.p0*","*.b0*","derivatives","ders.dat","mcout*.*","checkfile.rep",
            "personal.rep","gmacs_in.*","gmacs_files_in.dat","*.exe",
            "*.a","gmacs.par", "gmacs.dat","gmacs.rep","simdata.out");
  clean_files(path=path,names=names,verbose=verbose);
}


















