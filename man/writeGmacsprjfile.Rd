% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeGmacsprjfile.R
\name{writeGmacsprjfile}
\alias{writeGmacsprjfile}
\title{Write the projection file}
\usage{
writeGmacsprjfile(
  Dir = NULL,
  FileName = NULL,
  PrjFile = NULL,
  stock = "",
  model_name = "",
  Ass_Year = "",
  DirTPL = NULL,
  Ver_number = NULL
)
}
\arguments{
\item{Dir}{(character string)- path where to save the new Spc.prj file}

\item{FileName}{(character string)- name of the new Spc.prj file}

\item{PrjFile}{(character string)- Object (list) containing the ex Spc.prj file - The list is
created using the \code{\link[=readGMACSprj]{readGMACSprj()}} function.}

\item{stock}{(character string)- name of the stock of interest}

\item{model_name}{(character string)- name of the model currently considered (e.g., "model 22.A")}

\item{Ass_Year}{(character string)- Year of this assessment.}

\item{DirTPL}{(character string)- the directory where the gmacsbase.TPL file
you are using for the stock assessment is hold. This is used to identify the
version of Gmacs.}

\item{Ver_number}{(character)- The version of Gmacs. This is used only when
developing new code versions of Gmacs.}
}
\value{
create a new .prj file.
}
\description{
Write a new Spc.prj file. This function is used to modify within
R a pre-existent Spc.prj file.
}
\seealso{
\code{\link{readGMACSprj}}
}
