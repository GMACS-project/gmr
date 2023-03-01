#' @title Write data file
#'
#' @description Write a new Spc.dat file. This function is used to modify within
#' R a pre-existent Spc.dat file.
#'
#' @param Dir (character string)- path where to save the new Spc.dat file
#' @param FileName (character string)- name of the new Spc.dat file
#' @param DatFile (character string)- Object (list) containing the ex Spc.dat file - The list is
#' created using the [readGMACSdat()] function.
#' @param stock (character string)- name of the stock of interest
#' @param model_name (character string)- name of the model currently considered (e.g., "model 22.A")
#' @param Ass_Year (character string)- Year of this assessment
#'
#' @return create a new .dat file.
#'
#' @seealso \code{\link{readGMACSdat}}
#'
#' @export
#' @md
#
writeGmacsdatfile <- function(Dir = NULL,
                              FileName = NULL,
                              DatFile = NULL,
                              stock = "",
                              model_name = "",
                              Ass_Year = "") {

  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # Get GMACS version number and compilation date
  tmp <- GMACSversion(Dir = Dir)
  Ver <- tmp$ver
  Comp <- tmp$Comp

  obj <- DatFile

  base::sink(FileName)
  cat("# ============================================================ #\n")
  cat("#                    GMACS main data file \n")
  cat("# \n")
  cat("# *** \n")
  cat("#", Ver, "\n")
  cat("# Last GMACS mofification made by: ", Comp, "\n")
  cat("# Date of writing the data file:", .ac(Sys.time()))
  cat("# *** \n")
  cat("# \n")
  cat("# Stock of interest: ", stock, "\n")
  cat("# Model name: ", model_name, "\n")
  cat("# Year of assessment: ", Ass_Year, "\n")
  cat("# ============================================================ #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Model dimensions\n")
  cat("# -------------------------------------- #\n")
  cat("# Start year\n")
  cat(obj$Start_Y, "\n")
  cat("# End year\n")
  cat(obj$End_Y, "\n")
  cat("# Number of seasons\n")
  cat(obj$N_seasons, "\n")
  cat("# Number of fleets\n")
  cat(obj$N_fleet, "\n")
  cat("# Number of sexes\n")
  cat(obj$N_sexes, "\n")
  cat("# Number of shell condition types\n")
  cat(obj$N_shell_cdt, "\n")
  cat("# Number of maturity types\n")
  cat(obj$N_maturity, "\n")
  cat("# Number of size-classes in the	model\n")
  cat(obj$N_sizeC, "\n")
  cat("# Season recruitment occurs\n")
  cat(obj$Recr_Season, "\n")
  cat("# Season molting and growth occurs\n")
  cat(obj$Grwth_Season, "\n")
  cat("# Season to calculate SSB\n")
  cat(obj$SSB_Season, "\n")
  cat("# Season for N output\n")
  cat(obj$N_Season, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Size classes definition\n")
  cat("# -------------------------------------- #\n")
  cat("# maximum size-class (males then females)\n")
  cat(obj$Max_sizeC, "\n")
  cat(
    "#	size_breaks (a vector	giving the break points between size intervals, dim=nclass+1)\n"
  )
  cat(obj$Size_breaks, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Natural mortality\n")
  cat("# -------------------------------------- #\n")
  cat("#	Natural	mortality per season input type\n")
  cat("## (1 = vector	by season, 2 = matrix	by season/year)\n")
  cat(obj$M_in_Type, "\n")
  cat("# Proportion of the natural mortality to be applied each season\n")
  if (obj$M_in_Type == 1) {
    cat(obj$M_Seas_prop, "\n")
  } else if (obj$M_in_Type == 2) {
    utils::write.table(obj$M_Seas_prop,
                       row.names = FALSE,
                       col.names = FALSE)
    # cat("\n")
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Fishery and survey definition\n")
  cat("# -------------------------------------- #\n")
  cat("# Fishing fleet	names (delimited with a space - no spaces in names)\n")
  cat(obj$F_Fleet_names, "\n")
  cat("# Survey names (delimited with a space - no spaces in names)\n")
  cat(obj$Survey_names, "\n")
  cat("# Are the seasons: 0 = instantaneous or 1 = continuous\n")
  cat(obj$F_Season_Type, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Catch data\n")
  cat("# -------------------------------------- #\n")
  cat("# Number of catch data frames\n")
  cat(obj$N_CatchDF, "\n")
  cat("# Number of rows in each data frame\n")
  cat(obj$Nrows_CatchDF, "\n")
  cat("\n")
  cat("# ************************************** #\n")
  cat("#         ** CATCH DATA **\n")
  cat("# Sex: 0 = both; 1 = male; 2 = female\n")
  cat("# Type of catch: 0 = total; 1 = retained; 2 = discard\n")
  cat("# Units of catch: 1 = biomass; 2 = numbers\n")
  cat("# Mult: 1 = use data as they are; 2 = multiply by this number (e.g., lbs to kg)\n")
  cat("# Year | Season | Fleet | Sex | Obs | CV | Type | Units | Mult | Effort | Discard_mortality ##\n")
  cat("# ************************************** #\n")
  cat("\n")
  for (n in 1:obj$N_CatchDF) {
    # cat("\n")
    # cat("# **", names(obj$Catch)[n], " **\n")
    # cat("# Year | Season | Fleet | Sex | Obs | CV | Type | Units | Mult | Effort | Discard_mortality\n")
    utils::write.table(obj$Catch[[n]], row.names = FALSE, col.names = FALSE)
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Relative abundance data\n")
  cat("# -------------------------------------- #\n")
  cat("# Number of relative abundance indices\n")
  cat(obj$N_SurveyDF, "\n")
  cat("# Index Type (1 = Selectivity; 2 = Selectivity + retention)\n")
  cat(obj$Sv_type, "\n")
  cat("# Number of rows in each data frame of index data\n")
  cat(obj$Nrows_SvDF, "\n")
  cat("\n")
  cat("# ************************************** #\n")
  cat("#    ** RELATIVE ABUNDANCE	DATA **       #\n")
  cat(
    "# Index: One q is estimated for each index (the number of index values should match nSurveys)\n"
  )
  cat("# Sex: 0 = both; 1 = male; 2 = female\n")
  cat("# Maturity: 0 = both; 1 = mature; 2 = immature\n")
  cat("# Units of survey: 1 = biomass; 2 = numbers\n")
  cat("# Index | Year | Season | Fleet | Sex | Maturity | Obs | CV | Units | CPUE_time ##\n")
  cat("# ************************************** #\n")
  for (n in 1:obj$N_SurveyDF) {
    # cat("\n")
    # cat("# **", names(obj$Surveys)[n], " ** #\n")
    # cat("# Index | Year | Season | Fleet | Sex | Maturity | Obs | CV | Units | CPUE_time\n")
    utils::write.table(obj$Surveys[[n]], row.names = FALSE, col.names = FALSE)
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Size composition for all fleets\n")
  cat("# -------------------------------------- #\n")
  cat("# Number of size frequency matrices\n")
  cat(obj$N_SizeFreq_df, "\n")
  cat("# Number of rows in each size frequency matrix\n")
  cat(obj$Nrows_SiseFreqDF, "\n")
  cat(
    "# Number of bins in each length frequency matrix\n# (i.e., number of columns in each data frame\n"
  )
  cat(obj$Nbins_SiseFreq, "\n")
  cat("# ************************************** #\n")
  cat("#      ** SIZE COMPOSITION DATA **       #\n")
  cat("# Sex: 0 = both; 1 = male; 2 = female\n")
  cat("# Type of catch: 0 = total; 1 = retained; 2 = discard\n")
  cat("# Shell: 0 = both; 1 = new shell; 2 = old shell\n")
  cat("# Maturity: 0 = both; 1 = mature; 2 = immature\n")
  cat("# Nsamp: the stage-1 effective sample size (this can be modified in the .CTL file)\n")
  cat("# Year | Season | Fleet | Sex | Type | Shell | Maturity | Nsamp | Data Vector ##\n")
  cat("# ************************************** #\n")
  for (n in 1:obj$N_SizeFreq_df) {
    # cat("\n")
    # cat("# **", names(obj$SizeFreq)[n], " ** #\n")
    # cat("# Year | Season | Fleet | Sex | Type | Shell | Maturity | Nsamp | Data Vector\n")
    utils::write.table(obj$SizeFreq[[n]],
                       row.names = FALSE,
                       col.names = FALSE)
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Growth data\n")
  cat("# -------------------------------------- #\n")
  cat("# Type of observation (increment or change in size-class)\n")
  cat(
    "# 0: no growth data; 1: growth increment data; 2: growth size-class data; 3: growth size-class values\n"
  )
  cat(obj$GrowthObsType, "\n")
  cat("# Number of observation\n")
  cat(obj$NGrowthObs, "\n")
  cat("# ************************************** #\n")
  cat("#          ** GROWTH DATA **\n")
  cat("# Sex: 0 = both; 1 = male; 2 = female\n")
  cat("# Premolt: premolt size\n")
  cat("# Molt_Inc: size-increment\n")
  cat("# Size_rel: size-at-release\n")
  cat("# Size_Recap: size-at-recapture\n")
  cat("# T_at_sea: time-at-liberty\n")
  cat("# Recap_Year: year of recapture\n")
  cat("# Number: sample size\n")
  cat("\n")
  cat("# If growth increment data\n")
  cat("# Premolt | Sex | Molt_Inc | CV ##\n")
  cat("\n")
  cat("# If growth size-class data\n")
  cat("# Size_rel | Size_Recap | T_at_sea ##\n")
  cat("\n")
  cat("# If growth size-class values\n")
  cat("# Size_rel | sex | Size_Recap | T_at_sea | fleet | Recap_Year | Number ##\n")
  cat("# ************************************** #\n")
  if (obj$GrowthObsType == 0) {
    cat("# \n")
  } else {
    base::switch(
      .ac(obj$GrowthObsType),
      "1" = cat("# Premolt | Sex | Molt_Inc | CV ##\n"),
      "2" = cat("# Size_rel | Size_Recap | T_at_sea ##\n"),
      "3" = cat(
        "# Size_rel | sex | Size_Recap | T_at_sea | fleet | Recap_Year | Number ##\n"
      )
    )
    utils::write.table(obj$GrowthData,
                       row.names = FALSE,
                       col.names = FALSE)
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## End of data file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")

  base::sink()
}
