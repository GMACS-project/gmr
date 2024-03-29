% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_TPL.R
\name{write_TPL}
\alias{write_TPL}
\title{write_TPL}
\usage{
write_TPL(vv = NULL, Dir = NULL, .update = NULL)
}
\arguments{
\item{vv}{Numeric: indicate the version - loop on the length(Dir).}

\item{Dir}{list of Character string: hold the directories for the versions of
GMACS considered in the analysis.}

\item{.update}{Logical: is it to update a new version of GMACS? If TRUE,
the name of the new version of GMACS and the date of the compilation will be
modified in the gmacsbase.tpl file.}
}
\value{
a new gmacs.tpl file corresponding to the merging of gmacsbase.tpl
and personnal.tpl files. This gmacs.tpl will then be used to build the GMACS
executable.
}
\description{
Function to write the gmacs.TPL file from gmacsbase.tpl and
personnal.tpl.
}
