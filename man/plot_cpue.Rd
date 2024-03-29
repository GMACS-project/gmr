% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-cpue.R
\name{plot_cpue}
\alias{plot_cpue}
\title{Plot cpue or other indices}
\usage{
plot_cpue(
  M,
  subsetby = "",
  xlab = "Year",
  ylab = "CPUE",
  slab = "Sex",
  ShowEstErr = FALSE,
  logy = FALSE
)
}
\arguments{
\item{M}{list object created by read_admb function}

\item{subsetby}{the fleet to subset the data to}

\item{xlab}{the x-axis label for the plot}

\item{ylab}{the y-axis label for the plot}

\item{slab}{the sex label for the plot that appears above the key}

\item{ShowEstErr}{Shows errorbars from estimated CVs as well}

\item{logy}{Plot the CPUE in log-space}
}
\value{
plot of all observed and predicted incices
}
\description{
Plot cpue or other indices
}
