#' @title Write data file
#'
#' @description Write a new Spc.dat file. This function is used to modify within
#' R a pre-existent Spc.dat file.
#'
#' @param Dir (character string)- path where to save the new Spc.dat file.
#' @param FileName (character string)- name of the new Spc.dat file.
#' @param overwrite (logical)- Specify if the existing file should be overwritten.
#' The default value is \code{overwrite = FALSE}.
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
                              overwrite = FALSE,
                              DatFile = NULL,
                              stock = "",
                              model_name = "",
                              Ass_Year = "") {

  File <- file.path(Dir, FileName)
  # Check the existence of the file
  if (file.exists(File)) {
    if (!overwrite) {
      FileName <-  stringr::str_split(FileName, "[.]")[[1]][1]
      tmp <- stringr::str_replace_all(string = Sys.Date(), pattern = "-", replacement = "_")
      message(FileName, "already exists and 'overwrite'=FALSE. The new data file
 will be saved as : ", FileName, "_new_",tmp, ".dat")
      FileName <- paste0(FileName, "_new_",tmp,".dat")
    }
  }
  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # Get GMACS version number and compilation date
  tmp <- GMACSversion(Dir = Dir)
  Ver <- stringr::str_squish(tmp$ver)

  Comp <- tmp$Comp

  obj <- DatFile

  base::sink(FileName)
  cat("#_============================================================ #\n")
  cat("#                    GMACS main data file \n")
  cat("# \n")
  cat("#_*** \n")
  cat("#_", Ver, "\n")
  cat("#_Last GMACS mofification made by: ", Comp, "\n")
  cat("#_Date of writing the data file:", .ac(Sys.time()), "\n")
  cat("#_*** \n")
  cat("# \n")
  cat("#_Stock of interest: ", stock, "\n")
  cat("#_Model name: ", model_name, "\n")
  cat("#_Year of assessment: ", Ass_Year, "\n")
  cat("#_============================================================ #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Model dimensions\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Start year\n")
  cat(obj$Start_Y, "\n")
  cat("#_End year\n")
  cat(obj$End_Y, "\n")
  cat("#_Number of seasons\n")
  cat(obj$N_seasons, "\n")
  cat("#_Number of fleets\n")
  cat(obj$N_fleet, "\n")
  cat("#_Number of sexes\n")
  cat(obj$N_sexes, "\n")
  cat("#_Number of shell condition types\n")
  cat(obj$N_shell_cdt, "\n")
  cat("#_Number of maturity types\n")
  cat(obj$N_maturity, "\n")
  cat("#_Number of size-classes in the	model\n")
  cat(obj$N_sizeC, "\n")
  cat("#_Season recruitment occurs\n")
  cat(obj$Recr_Season, "\n")
  cat("#_Season molting and growth occurs\n")
  cat(obj$Grwth_Season, "\n")
  cat("#_Season to calculate SSB\n")
  cat(obj$SSB_Season, "\n")
  cat("#_Season for N output\n")
  cat(obj$N_Season, "\n")
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Size classes definition\n")
  cat("#_-------------------------------------- #\n")
  cat("#_maximum size-class (males then females)\n")
  cat(obj$Max_sizeC, "\n")
  cat(
    "#_size_breaks (a vector giving the break points between size intervals, dim=nclass+1)\n"
  )
  cat(obj$Size_breaks, "\n")
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Natural mortality\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Natural mortality per season input type\n")
  cat("##_(1 = vector	by season, 2 = matrix	by season/year)\n")
  cat(obj$M_in_Type, "\n")
  cat("#_Proportion of the natural mortality to be applied each season\n")
  if (obj$M_in_Type == 1) {
    cat(obj$M_Seas_prop, "\n")
  } else if (obj$M_in_Type == 2) {
    utils::write.table(obj$M_Seas_prop,
                       row.names = FALSE,
                       col.names = FALSE)
    # cat("\n")
  }
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Fishery and survey definition\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Fishing fleet	names (delimited with a space - no spaces in names)\n")
  cat(obj$F_Fleet_names, "\n")
  cat("#_Survey names (delimited with a space - no spaces in names)\n")
  cat(obj$Survey_names, "\n")
  cat("#_Are the seasons: 0 = instantaneous or 1 = continuous\n")
  cat(obj$F_Season_Type, "\n")
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Catch data\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Number of catch data frames\n")
  cat(obj$N_CatchDF, "\n")
  cat("#_Number of rows in each data frame\n")
  cat(obj$Nrows_CatchDF, "\n")
  cat("\n")
  cat("#_************************************** #\n")
  cat("#         ** CATCH DATA **\n")
  cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
  cat("#_Type of catch: 0 = total; 1 = retained; 2 = discard\n")
  cat("#_Units of catch: 1 = biomass; 2 = numbers\n")
  cat("#_Mult: 1 = use data as they are; 2 = multiply by this number (e.g., lbs to kg)\n")
  cat("#_Year_| Season_| Fleet_| Sex_| Obs_| CV_| Type_| Units_| Mult_| Effort_| Discard_mortality ##\n")
  cat("#_************************************** #\n")
  cat("\n")
  if(obj$N_CatchDF > 0){
    for (n in 1:obj$N_CatchDF) {
      cat("\n")
      cat("# **", names(obj$Catch)[n], " **\n")
      cat("# Year | Season | Fleet | Sex | Obs | CV | Type | Units | Mult | Effort | Discard_mortality\n")
      utils::write.table(obj$Catch[[n]], row.names = FALSE, col.names = FALSE)
    }
  } else {
    cat("\n")
    cat("# Year | Season | Fleet | Sex | Obs | CV | Type | Units | Mult | Effort | Discard_mortality\n")
    cat("\n")
  }
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Relative abundance data\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Number of relative abundance indices\n")
  cat(obj$N_SurveyDF, "\n")
  cat("#_Index Type (1 = Selectivity; 2 = Selectivity + retention)\n")
  cat(obj$Sv_type, "\n")
  cat("#_Number of rows in each data frame of index data\n")
  cat(obj$Nrows_SvDF, "\n")
  cat("\n")
  cat("#_************************************** #\n")
  cat("#    ** RELATIVE ABUNDANCE	DATA **       #\n")
  cat(
    "#_Index: One q is estimated for each index (the number of index values should match nSurveys)\n"
  )
  cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
  cat("#_Maturity: 0 = both; 1 = mature; 2 = immature\n")
  cat("#_Units of survey: 1 = biomass; 2 = numbers\n")
  cat("#_Index_| Year_| Season_| Fleet_| Sex_| Maturity_| Obs_| CV_| Units_| CPUE_time ##\n")
  cat("#_************************************** #\n")
  if(obj$N_SurveyDF > 0 ){
    for (n in 1:obj$N_SurveyDF) {
      cat("\n")
      cat("# **", names(obj$Surveys)[n], " ** #\n")
      cat("# Index | Year | Season | Fleet | Sex | Maturity | Obs | CV | Units | CPUE_time\n")
      utils::write.table(obj$Surveys[[n]], row.names = FALSE, col.names = FALSE)
    }
  } else {
    cat("\n")
    cat("# Index | Year | Season | Fleet | Sex | Maturity | Obs | CV | Units | CPUE_time\n")
    cat("\n")
  }
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Size composition for all fleets\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Number of size frequency matrices\n")
  cat(obj$N_SizeFreq_df, "\n")
  cat("#_Number of rows in each size frequency matrix\n")
  cat(obj$Nrows_SiseFreqDF, "\n")
  cat(
    "#_Number of bins in each length frequency matrix\n# (i.e., number of columns in each data frame\n"
  )
  cat(obj$Nbins_SiseFreq, "\n")
  cat("#_************************************** #\n")
  cat("#      ** SIZE COMPOSITION DATA **       #\n")
  cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
  cat("#_Type of catch: 0 = total; 1 = retained; 2 = discard\n")
  cat("#_Shell: 0 = both; 1 = new shell; 2 = old shell\n")
  cat("#_Maturity: 0 = both; 1 = mature; 2 = immature\n")
  cat("#_Nsamp: the stage-1 effective sample size (this can be modified in the .CTL file)\n")
  cat("#_Year_| Season_| Fleet_| Sex_| Type_| Shell_| Maturity_| Nsamp_| Data Vector ##\n")
  cat("#_************************************** #\n")
  if(obj$N_SizeFreq_df > 0){
    for (n in 1:obj$N_SizeFreq_df) {
      cat("\n")
      cat("# **", names(obj$SizeFreq)[n], " ** #\n")
      cat("# Year | Season | Fleet | Sex | Type | Shell | Maturity | Nsamp | Data Vector\n")
      utils::write.table(obj$SizeFreq[[n]],
        row.names = FALSE,
        col.names = FALSE)
    }
  } else {
    cat("\n")
    cat("# Year | Season | Fleet | Sex | Type | Shell | Maturity | Nsamp | Data Vector\n")
    cat("\n")
  }
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Growth data\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Type of observation (increment or change in size-class)\n")
  cat(
    "#_0: no growth data; 1: growth increment data; 2: growth size-class data; 3: growth size-class values\n"
  )
  cat(obj$GrowthObsType, "\n")
  cat("#_Number of observation\n")
  cat(obj$NGrowthObs, "\n")
  cat("#_************************************** #\n")
  cat("#          ** GROWTH DATA **\n")
  cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
  cat("#_Premolt: premolt size\n")
  cat("#_Molt_Inc: size-increment\n")
  cat("#_Size_rel: size-at-release\n")
  cat("#_Size_Recap: size-at-recapture\n")
  cat("#_T_at_sea: time-at-liberty\n")
  cat("#_Recap_Year: year of recapture\n")
  cat("#_Number: sample size\n")
  cat("\n")
  cat("#_If growth increment data\n")
  cat("#_Premolt_| Sex_| Molt_Inc_| CV ##\n")
  cat("\n")
  cat("#_If growth size-class data\n")
  cat("#_Size_rel_| Size_Recap_| T_at_sea ##\n")
  cat("\n")
  cat("#_If growth size-class values\n")
  cat("#_Size_rel_| sex_| Size_Recap_| T_at_sea_| fleet_| Recap_Year_| Number ##\n")
  cat("#_************************************** #\n")
  if (obj$GrowthObsType == 0) {
    cat("# \n")
  } else {
    base::switch(
      .ac(obj$GrowthObsType),
      "1" = cat("#_Premolt_| Sex_| Molt_Inc_| CV ##\n"),
      "2" = cat("#_Size_rel_| Size_Recap_| T_at_sea ##\n"),
      "3" = cat(
        "#_Size_rel_| sex_| Size_Recap_| T_at_sea_| fleet_| Recap_Year_| Number ##\n"
      )
    )
    utils::write.table(obj$GrowthData,
                       row.names = FALSE,
                       col.names = FALSE)
  }
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Environmental data\n")
  cat("#_-------------------------------------- #\n")
  cat("\n")
  cat("#_Number of environmental time series (different indices)\n")
  cat(obj$NenvIndics, "\n")
  cat("#_Year ranges for each index (One line per index - From 1 to N env indices)\n")
  cat("#_Start_year_| End_year\n")
  if(obj$NenvIndics > 0){
    # cat(obj$EnvYrs, "\n")
    utils::write.table(obj$EnvYrs,
                       row.names = FALSE,
                       col.names = FALSE)
    cat("\n")
  } else {
    cat("\n")
  }
  cat("#_Environmental Indices\n")
  cat("#_Index_| Year_| Value\n")
  if(obj$NenvIndics > 0){
    # cat(obj$EnvYrs, "\n")
    utils::write.table(obj$EnvData,
                       row.names = FALSE,
                       col.names = FALSE)
    cat("\n")
  } else {
    cat("\n")
  }
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_End of data file\n")
  cat("#_-------------------------------------- #\n")
  cat(9999)
  cat("\n")

  base::sink()
}
