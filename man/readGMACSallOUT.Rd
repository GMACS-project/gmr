% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readGMACSallOUT.R
\name{readGMACSallOUT}
\alias{readGMACSallOUT}
\title{Read GmacsAll.out}
\usage{
readGMACSallOUT(
  FileName = NULL,
  verbose = TRUE,
  DatFile = NULL,
  CtlFile = NULL,
  GmacsFile = NULL,
  nyrRetro = NULL
)
}
\arguments{
\item{FileName}{(character string)- path and name (by default, gmacsAll.out) of the file to read.}

\item{verbose}{(logical)- (TRUE/FALSE); flag to print processing information}

\item{DatFile}{(list)- Object containing the .dat file - This is the output
of the \code{\link[=readGMACSdat]{readGMACSdat()}} function.}

\item{CtlFile}{(list)- Object containing the .ctl file - This is the output
of the \code{\link[=readGMACSctl]{readGMACSctl()}} function.}

\item{GmacsFile}{(list)- Object containing the gmacs.dat file - This is the output
of the \code{\link[=readGMACS.dat]{readGMACS.dat()}} function.}

\item{nyrRetro}{(integer)- Number of year for the retrospective analysis}
}
\value{
the gmacsall.out file as a named list.

\itemize{
\item \code{sourcefile} - The file source.
\item \code{Comments} - Specifications about the GMACS version used and the stock
assessed.
\item \code{Stock_info} - A named list with information relative to the stock assessed.
Objects are the following:
\itemize{
\item \code{Stock} - The name of the stock assessed.
\item \code{Year_range} - The start and end year of the assessment.
\item \code{Number_of_seasons} - The number of season.
\item \code{Number_of_fleets} - The number of fleet.
\item \code{Fleets} - The names of the fleets (fishing and surveys).
\item \code{Number_of_sexes} - The number of sex.
\item \code{Number_of_shell_conditions} - The number of shell conditions.
\item \code{Number_of_maturity_states} - The number of maturity states.
\item \code{Weight_unit_is} - The unit for weight.
\item \code{Numbers_unit_is} - The unit for numbers.
}
\item \code{Lik_type} - The likelihood for each data component (raw and weighted).
\item \code{Penalties} - The total penalties.
\item \code{Priors} - The likelihood for priors.
\item \code{Initial_size_structure} - The likelihood for the initial size
composition.
\item \code{Total} - The total likelihood.
\item \code{Lik_type_fleet} - The likelihood (raw, emphasis, net) for each data source
including recruitment (deviations and sex-ratio).
\item \code{Penalties_type} - The Penalties and emphasis for each "priors".
\item \code{Maximum_gradient} - The maximum gradient.
\item \code{Param} - A named list with the estimated parameters. Objects are the following:
\itemize{
\item \code{theta} - The key parameter controls (core parameters - theta parameters).
\item \code{Grwth} - The growth parameters.
\item \code{Vul} - The vulnerability (selectivity and retention) parameters.
\item \code{Envpar_Slx} - The environmental-linked selectivity parameters.
\item \code{Slx_Devs} - The selectivity deviations.
\item \code{Fbar} - The mean fishing mortality parameters.
\item \code{Fdev} - The fishing fleet-specific weights for male.
\item \code{Foff} - The female fishing mortality offset to male F.
\item \code{Fdov} - The fishing fleet-specific weights for female.
\item \code{rec_ini} - The initial recruitment by size-class.
\item \code{rec_dev_est} - The year-specific recruitment deviations.
\item \code{logit_rec_prop_est} - The year-specific sex-ratio recruitment.
\item \code{Mdev} - The natural mortality deviation.
\item \code{EffSamp_size} - The effective sample size.
\item \code{survey_Q} - The survey-specific catchability.
\item \code{log_add_cvt} - The survey-specific additional CV.
}
\item \code{Management_Quantities} - The estimated management quantities
(SPR; MSY, OFL, ...) and recruitment.
\item \code{Overall_Summ} - Overall summary per year (SSB, SSA, Dynamic B0,
Recruitment, mortality, ...).
\item \code{mean_weight} - The sex-specific mean weight per size-class for
each year and the mid-point used in the model.
\item \code{maturity} - The sex-specific proportion of mature per size-class.
\item \code{dCatchData} - The catch data with predicted values.
\item \code{log_q_catch} - The estimated catchability (log space).
\item \code{dSurveyData} - The survey data with predicted values.
\item \code{sdnr_MAR_cpue} - The survey-specific standard devaiation and median.
\item \code{Size_fit_summary} - The summary of size-specific fit for each fleet.
\item \code{sdnr_MAR_lf} - The standard deviation and median for each size composition data.
\item \code{Francis_weights} - The francis weights.
\item \code{Selectivity} - The size-specific selectivity for the capture, retained and discards.
\item \code{Select_control} - The selectivity controls.
\item \code{m_prop} - The proportion of natural mortality per season and year.
\item \code{M_size_class} - The sex- and mature-state specific natural mortality
per size-class for each year.
\item \code{Fully_selected_fishing_F_Fl_Yr_Sex} - The sex-specific fully-selected
fishing mortality for each season and each year.
\item \code{Fully_F} - The fully-selected fishing mortality for each fleet.
\item \code{F_SizeC_Continuous} - The fully-selected fishing mortality by
size class (continuous).
\item \code{F_SizeC_Discrete} - The fully-selected fishing mortality by
size class (discrete).
\item \code{TotMorta_SizeC_Continuous} - The total mortality by size class
(continuous).
\item \code{TotMorta_SizeC_Discrete} - The total mortality by size class (discrete).
\item \code{N_at_size} - The matrix of number-at-size.
\item \code{molt_probability} - The sex-specific probability of molting
for each year by size class.
\item \code{Growth_transition_Matrix} - The sex-specific growth transition matrix.
\item \code{Size_transition_Matrix} - The sex-specific size transition matrix.
\item \code{fhitfut} - A season-specific matrix for F by fleet.
\item \code{spr_syr} - The first year for computing Rbar.
\item \code{spr_nyr} - The last year for computing Rbar.
\item \code{spr_rbar} - The mean recruitment for SPR calculation.
\item \code{proj_rbar} - The mean recruitment for the projections.
\item \code{spr_sexr} - The sex-ratio for SPR calculation.
\item \code{SR_alpha_prj} - The alpha parameter of the stock recruitment
relationship used in the projections.
\item \code{SR_beta_prj} - The beta parameter of the stock recruitment
relationship used in the projections.
\item \code{spr_fofl} - The fishing mortality relative to MSY for the OFL.
\item \code{spr_cofl_ret} - The retained portion of the OFL.
\item \code{nloglike} - A list containing the negative log-likelihood for all
data sources (catch, survey, size composition, growth data (tagging data),
and recruitment)
\item \code{nlogPenalty} - The log penalties.
\item \code{priorDensity} - The values of the prior densities.
}
}
\description{
This functions reads the gmacsall.out file as a named list
within R.
}
\examples{
\dontrun{
# Stock ----
stock <- "SNOW_crab"
# GMACS input files ----
datfileName <- "snow_21_M09.dat"
ctlfileName <- "snow_21_M09.ctl"
# read gmacs.dat ----
fileName <- "gmacs.dat"
fileName <- file.path(dir_Base, stock, fileName, fsep = fsep)
GMACSdat <- readGMACS.dat(path = fileName, verbose = TRUE)
# Read the data file ----
datFile <- file.path(dir_Base, stock, datfileName, fsep = fsep)
datFile <- readGMACSdat(FileName = datFile, verbose = T)
# Read the control file ----
ctlFile <- file.path(dir_Base, stock, ctlfileName, fsep = fsep)
ctlFile <- readGMACSctl(
  FileName = ctlFile,
  verbose = T,
  DatFile = datFile,
  nyrRetro = GMACSdat$N_Year_Retro
)
# Read the Gmacsall.out file ----
GMACSalloutfile <-
  readGMACSallOUT(
    FileName = file.path(Dir_Dvpt_Vers, "build", stock, "Gmacsall.out", fsep = fsep),
    verbose = TRUE,
    DatFile = datFile,
    CtlFile = ctlFile,
    GmacsFile = GMACSdat,
    nyrRetro = GMACSdat$N_Year_Retro
  )
}

}
\seealso{
\code{\link{readGMACSdat}}, \code{\link{readGMACSctl}}, \code{\link{readGMACSrep}}
}
