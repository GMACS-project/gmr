% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CompareCodeVersion.R
\name{CompareCodeVersion}
\alias{CompareCodeVersion}
\title{Compare Gmacs version}
\usage{
CompareCodeVersion(
  stock = NULL,
  dir_test = NULL,
  dir_old_version = NULL,
  dir_OLD_Gmacs_Exe = NULL,
  dir_Gmacs_Exe = NULL,
  dir_InputFiles = NULL,
  usePin = NULL,
  Run_old_version = TRUE,
  compareWithPin = NULL,
  Threshold = 1e-05,
  verbose = TRUE,
  verbose_shell = TRUE,
  Clean_Files = NULL
)
}
\arguments{
\item{stock}{(character; vector)- the of the stock to be tested. This specified
the input file folder names in the \code{dir_test} repertory.}

\item{dir_test}{(path)- path to the repertory where the output of Gmacs
ran for each \code{stock} using the new version will be saved saved. This
repertory will be used to run tests and make comparison between Gmacs code
version.}

\item{dir_old_version}{(path)- path to the repertory where the output of Gmacs
ran for each \code{stock} using the version to be compared with are saved. The
names of the folders in that repertory have to match the ones in the \code{stock}
argument.}

\item{dir_OLD_Gmacs_Exe}{(path)- path to the repertory where the .TPL file of the
old Gmacs version is saved. If empty, this is set equal to
\code{dirname(dir_old_version)}.}

\item{dir_Gmacs_Exe}{(path)- path to the repertory where the .TPL file of the
new Gmacs version is saved. If empty, this is set equal to \code{dir_test}.}

\item{dir_InputFiles}{(path)- path to the repertory where the input files for
each stock for the newly developed version of Gmacs are saved.}

\item{usePin}{(logical)- flag to use the \code{gmacs.pin} as pin file to initialize
parameter values when running the new version. This assumes that the \code{gmacs.par}
file structure remained the same between the two versions.}

\item{Run_old_version}{(logical)- flag to re run the old version of Gmacs using
the estimated values for the parameters as a pin file. This will allow to get
a new \code{gmacs.par} file that will be then used for comparison. Default,
\code{Run_old_version = TRUE}.}

\item{compareWithPin}{(logical)- flag to make comparison with the .pin file
instead of the gmacs.par file obtained with the version to be compared with.}

\item{Threshold}{(numeric)- Threshold to test if the comparison between the
two versions passed. It is used to compared parameter values between the two
version. by default, \code{Threshold = 1.0e-5}.}

\item{Clean_Files}{(logical)- flag to delete Gmacs files in stock specific folder
after running the new version. See the \code{\link[=clean_bat]{clean_bat()}} function for more details.}

\item{verbose;verbose_shell}{(logical)- flag to print processing information
and shell output. Default, \code{verbose;verbose_shell = TRUE}.}
}
\value{
A list with the results of the comparison. This list is also saved
as a \code{.Rdata} file under the name \code{Results_testing_version_XXX_[runDate]} in
the relative folder of the \code{dir_test} repertory, i.e., in the \code{Testing_Versions}
folder if the workflow to update/upgrade Gmacs has been followed.
}
\description{
Function to run test of comparison between two code versions of
Gmacs.
}
\details{
When \code{Run_old_version = TRUE}, the function assumes that the \code{gmacs.exe}
for the old version is saved in the relative folder of the \code{dir_old_version}
folder. If that is not the case, the \code{gmacs.exe} will be build based on the
Gmacs codes available in that folder. See the \code{\link[=createGmacsExe]{createGmacsExe()}} function.

Using the pin file to initialize a model run (\code{usePin=TRUE}) also sets
the starting phase to 10. If all is well, the model should converge almost
immediately if the pin file is a copy of a par file from a previous converged
model run. Ideally, the model should converge immediately, with the resulting
par file identical to the pin file. However, this is frequently not the case
if parameter values are near a bound in the pin file and thus the resulting par
file values may not be identical to those in the pin file (i.e., the model
converged to a slightly different place in parameter space). For the purpose of
quickly testing the impact of changes to the code on previous model results, it
is probably best to test the par file obtained using the new code run with the
pin file against a par file from the old code that was the result of using the
same pin file to initialize that model run.
}
\examples{
\dontrun{
# Load package ----
library(gmr)

# Set directories ----
Dir_Dvpt_Vers <- file.path(here::here(), "Dvpt_Version", fsep = fsep)
Dir_Last_Vers <- file.path(here::here(), "Latest_Version", fsep = fsep)
dir_test <- file.path(here::here(), "Testing_Versions")

# Testing new version of Gmacs ----
Res <- CompareCodeVersion(stock = "all",
dir_test = dir_test,
dir_old_version = file.path(Dir_Last_Vers, 'build', fsep = fsep),
dir_new_version = file.path(Dir_Dvpt_Vers, 'build', fsep = fsep),
dir_Gmacs_Exe = dir_Gmacs_Exe,
dir_InputFiles = dir_InputFiles,
dir_OLD_Gmacs_Exe = NULL,
Run_old_version = TRUE,
usePin = TRUE,
compareWithPin = FALSE,
verbose = TRUE,
Threshold = 1.0e-5,
Clean_Files = TRUE
)
}

}
\author{
W. Stockhausen; M. Veron
}
