% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readGMACSpar.R
\name{readGMACSpar}
\alias{readGMACSpar}
\title{readGMACSpar}
\usage{
readGMACSpar(
  Dir = NULL,
  FileName = "gmacs.par",
  verbose = NULL,
  DatFile = NULL,
  CtlFile = NULL
)
}
\arguments{
\item{Dir}{(character string)- path to the folder where the parameter file is stored.}

\item{FileName}{(character string)- name of the parameter file - By default, "gmacs.par"}

\item{verbose}{(logical)- flag to print processing information}

\item{DatFile}{(list)- Object containing the .dat file - This is the output
of the \code{\link[=readGMACSdat]{readGMACSdat()}} function.}

\item{CtlFile}{(list)- Object (list) containing the .ctl file - This is the output
of the \code{\link[=readGMACSctl]{readGMACSctl()}} function.}
}
\value{
the gmacs.par file as a named list. Where data frame are used, the
columns are parameter_ID/value.
}
\description{
Read the gmacs.par file. This is an output of GMACS that contains
estimates of all parameters
}
\seealso{
\code{\link{readGMACS.dat}},\code{\link{readGMACSdat}},\code{\link{readGMACSctl}},
\code{\link{readGMACSprj}}
}
