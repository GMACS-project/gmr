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
#' @param stock (character string)- name of the stock of interest.
#' @param model_name (character string)- name of the model currently considered (e.g., "model 22.A").
#' @param Ass_Year (character string)- Year of this assessment.
#' @param DirTPL (character string)- the directory where the gmacsbase.TPL file
#' you are using for the stock assessment is hold.
#' @param CatchDF_format (integer)- Overwrite the catch data format. If 0, catch
#'  data will be written using the 'old' format; if 1, catch data will be written
#'  using the 'new' format. Both the `maturity states` and `shell condition`
#'  that are incorporated in the 'new format' are not used in the model but this
#'  is for a future option. By default, both of them will be set as undetermined
#'  when switching from the 'old' to the 'new' format.
#' @param SurveyDF_format (integer)- Overwrite the relative abundance index data
#'  format. If 0, survey data will be written using the 'old' format; if 1, survey
#'  data will be written using the 'new' format.
#' @param SizeFreqDF_format  (integer)- Overwrite the size frequency data
#'  format. If 0, size frequency data will be written using the 'old' format;
#'  if 1, size frequency data will be written using the 'new' format.
#'  @param Ver_number (character)- The version of Gmacs. This is used only when
#' developing new code versions of Gmacs.
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
                              Ass_Year = "",
                              DirTPL = NULL,
                              CatchDF_format = NULL,
                              SurveyDF_format = NULL,
                              SizeFreqDF_format = NULL,
                              Ver_number = NULL) {
  # Local functions

  # @title namSizeFq
  #
  # @description Name the Size Frequency data frame.
  #
  # @param df one Size frequency data frame
  #
  # @return the name of the size frequency data frame as "Sex_Type_Fleet_Seas".
  #
  namSizeFq <- function(df) {
    if (length(unique(df[, "sex"])) > 1) {
      sex <- "Males_Females_"
    } else {
      sex <- base::switch(.ac(unique(df[, "sex"])),
                          "0" = "Both",
                          "1" = "Males",
                          "2" = "Females")
    }
    type <- base::switch(.ac(unique(df[, "Type"])),
                         "0" = "Total",
                         "1" = "Retained",
                         "2" = "Discard")
    mat <- base::switch(.ac(unique(df[, "Maturity"])),
                        "0" = "BothMat",
                        "1" = "Mature",
                        "2" = "Immat")
    fleet <-
      c(DatFile$F_Fleet_names, DatFile$Survey_names)[unique(df[, "fleet"])]
    nam <- paste(sex, mat, type, fleet, sep = "_")
    # Seas <- paste0("Seas", unique(df[, "seas"]))
    # nam <- paste(sex, mat, type, fleet, Seas, sep = "_")
    return(nam)
  }

  # @title namCatch
  #
  # @description Name the catch data frame.
  #
  # @param df one catch data frame
  #
  # @return the name of the catch data frame as "Sex_Type_Fleet_Seas".
  #
  namCatch <- function(df, format = NULL) {
    sex <- base::switch(.ac(unique(df[, "sex"])),
                        "0" = "Both",
                        "1" = "Males",
                        "2" = "Females")
    type <- base::switch(.ac(unique(df[, "Type"])),
                         "0" = "Total",
                         "1" = "Retained",
                         "2" = "Discard")
    fleet <- obj$F_Fleet_names[unique(df[, "fleet"])]
    Seas <- unique(df[, "seas"])
    if (format == 0) {
      nam <-
        paste0("# ** Fleet: ",
               fleet,
               "; Catch type: ",
               type,
               "; Sex: ",
               sex,
               "; Season: ",
               Seas,
               " **\n")
    } else if (format == 1) {
      nam <-
        paste0(
          "# ** Fleet: ",
          fleet,
          "; Catch type: ",
          type,
          "; Sex: ",
          sex,
          "; Season: ",
          Seas,
          "; Units:",
          unique(df[, "units"]),
          " **\n"
        )
    }
    return(nam)
  }

  # @title swithFormatDF
  #
  # @description Re-format the catch, relative abundance index and size frequency
  # matrices if required (switching from the 'old' to the 'new' format and vice versa).
  #
  # @param obj the data file as given to the `writeGmacsdatfile()` function
  # @param CatchDF_format (integer)- Overwrite the catch data format. If 0, catch
  #  data will be written using the 'old' format; if 1, catch data will be written
  #  using the 'new' format. Both the `maturity states` and `shell condition`
  #  that are incorporated in the 'new format' are not used in the model but this
  #  is for a future option. By default, both of them will be set as undetermined
  #  when switching from the 'old' to the 'new' format.
  # @param SurveyDF_format (integer)- Overwrite the relative abundance index data
  #  format. If 0, survey data will be written using the 'old' format; if 1, survey
  #  data will be written using the 'new' format.
  # @param SizeFreqDF_format  (integer)- Overwrite the size frequency data
  #  format. If 0, size frequency data will be written using the 'old' format;
  #  if 1, size frequency data will be written using the 'new' format.
  #
  # retrun the obj as a list with the new format for each matrix (if applicable).
  swithFormatDF <- function(obj = NULL,
                            CatchDF_Newformat = NULL,
                            SurveyDF_Newformat = NULL,
                            SizeFreqDF_Newformat = NULL) {
    # *************************
    # obj = obj
    # CatchDF_Newformat = CatchDF_format
    # SurveyDF_Newformat = SurveyDF_format
    # SizeFreqDF_Newformat = SizeFreqDF_format
    # # *************************



    # Catch data ----
    if (!is.null(CatchDF_Newformat) &
        obj$CatchDF_format != CatchDF_Newformat) {
      # Update the format for the catch data
      obj$CatchDF_format <- CatchDF_Newformat

      # Check for changes in catch data
      if (obj$N_CatchDF > 0) {
        # Check if there is at least 1 dataframe
        if (CatchDF_Newformat == 0) {
          # set the old catch format

          obj$Nrows_CatchDF <- NULL
          oldCatch <- obj$Catch
          Catch <- NULL

          # Check if some dataframe can be rbind based on the Unit
          # The old format integrate the two type of units in one dataframe
          check_nam <- mapply(
            names(oldCatch),
            FUN = function(X) {
              tmp <- stringr::str_split(string = X, pattern = '_')[[1]]
              return(paste(tmp[-c(length(tmp))], collapse = '_'))
            }
          )

          n_pos <- 1
          for (n in 1:length(oldCatch)) {
            tmp <- oldCatch[[n]]
            F_Fleet_names <- which(tmp$fleet == obj$F_Fleet_names)
            tmp$data <- tmp$data %>%
              dplyr::mutate(
                units = base::switch(
                  .ac(tmp$unit),
                  "BIOMASS" = "1",
                  "ABUNDANCE" = "2"
                ),
                Type =  base::switch(
                  .ac(tmp$type),
                  "TOTAL" = "0",
                  "RETAINED" = "1",
                  "DISCARD" = "2"
                ),
                fleet = .an(F_Fleet_names),
                sex = base::switch(
                  .ac(tmp$sex),
                  "undetermined" = "0",
                  "male" = "1",
                  "female" = "2"
                )
              ) %>% as.data.frame() %>%
              dplyr::mutate_at(c('units', 'Type', 'fleet', 'sex'), as.numeric) %>%
              dplyr::relocate(dplyr::any_of(
                c(
                  "year",
                  "seas",
                  "fleet",
                  "sex",
                  "obs",
                  "CV",
                  "Type",
                  "units",
                  "mult",
                  "effort",
                  "discard_mortality"
                )
              ))
            # Update the catch data
            if (n_pos > 1) {
              if (check_nam[n] == check_nam[n_pos - 1]) {
                Catch[[n_pos - 1]] <- rbind(Catch[[n_pos - 1]], tmp$data)
                # Update the vector of number of rows
                obj$Nrows_CatchDF[n_pos - 1] <- obj$Nrows_CatchDF[n_pos -
                                                                    1] + tmp$Nrows
                n_pos <- n
              } else {
                Catch[[n_pos]] <- tmp$data
                names(Catch)[n_pos] <- paste(base::switch(
                  .ac(tmp$sex),
                  "undetermined" = "Both",
                  "male" = "Males",
                  "female" = "Females"
                ), base::switch(
                  .ac(tmp$type),
                  "TOTAL" = "Total",
                  "RETAINED" = "Retained",
                  "DISCARD" = "Discard"
                ), tmp$fleet, sep = "_")
                # Update the vector of number of rows
                obj$Nrows_CatchDF[n_pos] <- tmp$Nrows
                n_pos <- n_pos + 1
              }
            } else {
              Catch[[n_pos]] <- tmp$data
              names(Catch)[n_pos] <- paste(base::switch(
                .ac(tmp$sex),
                "undetermined" = "Both",
                "male" = "Males",
                "female" = "Females"
              ), base::switch(
                .ac(tmp$type),
                "TOTAL" = "Total",
                "RETAINED" = "Retained",
                "DISCARD" = "Discard"
              ), tmp$fleet, sep = "_")
              # Update the vector of number of rows
              obj$Nrows_CatchDF[n_pos] <- tmp$Nrows
              n_pos <- n_pos + 1
            }
          }
        } else {
          # set the new catch format
          oldCatch <- obj$Catch
          Catch <- NULL
          n_pos <- 1
          for (n in 1:length(oldCatch)) {
            if (n > 1)
              Catch[[n_pos]] <- list()

            # Check for unity of Sex/Type/Fleet/Season/Unit
            Spc <- oldCatch[[n]] %>%
              dplyr::select(units, Type, fleet, sex) %>%
              unique()

            if (dim(Spc)[1] > 1) {
              # Update the number of data frame
              obj$N_CatchDF <- obj$N_CatchDF + (dim(Spc)[1] - 1)
              # Split the data based on the selection level


              for (i in 1:dim(Spc)[1]) {
                if (n_pos > 1)
                  Catch[[n_pos]] <- list()

                tmp_data <- oldCatch[[n]] %>%
                  dplyr::filter(units == Spc[i, "units"],
                                Type == Spc[i, "Type"],
                                fleet == Spc[i, "fleet"],
                                sex == Spc[i, "sex"])
                tmp_Spc <- tmp_data %>%
                  dplyr::select(units, Type, fleet, sex) %>%
                  unique() %>%
                  dplyr::mutate(
                    units = base::switch(.ac(units), "1" = "BIOMASS", "2" = "ABUNDANCE"),
                    Type =  base::switch(
                      .ac(Type),
                      "0" = "TOTAL",
                      "1" = "RETAINED",
                      "2" = "DISCARD"
                    ),
                    fleet = obj$F_Fleet_names[fleet],
                    sex = base::switch(
                      .ac(sex),
                      "0" = "undetermined",
                      "1" = "male",
                      "2" = "female"
                    )
                  ) %>% as.data.frame()

                Catch[[n_pos]][["unit"]] <- tmp_Spc$units
                Catch[[n_pos]][["type"]] <- tmp_Spc$Type
                Catch[[n_pos]][["fleet"]] <- tmp_Spc$fleet
                Catch[[n_pos]][["sex"]] <- tmp_Spc$sex
                Catch[[n_pos]][["maturity"]] <- "undetermined"
                Catch[[n_pos]][["shell_cond"]] <- "undetermined"
                Catch[[n_pos]][["Nrows"]] <- dim(tmp_data)[1]
                Catch[[n_pos]][["data"]] <- tmp_data %>%
                  dplyr::select(year,
                                seas,
                                obs,
                                CV,
                                mult,
                                effort,
                                discard_mortality) %>%
                  as.data.frame()

                names(Catch)[n_pos] <- paste(base::switch(
                  .ac(tmp_Spc$sex),
                  "undetermined" = "Both",
                  "male" = "Males",
                  "female" = "Females"
                ),
                base::switch(
                  .ac(tmp_Spc$Type),
                  "TOTAL" = "Total",
                  "RETAINED" = "Retained",
                  "DISCARD" = "Discard"
                ),
                tmp_Spc$fleet,
                base::switch(
                  .ac(tmp_Spc$units),
                  "ABUNDANCE" = "Abundance",
                  "BIOMASS" = "Biomass"
                ),
                sep = "_")
                # Updqte the position in the list
                n_pos <- n + i
              }
            } else {
              Spc <- oldCatch[[n]] %>%
                dplyr::select(units, Type, fleet, sex) %>%
                unique() %>%
                dplyr::mutate(
                  units = base::switch(.ac(units), "1" = "BIOMASS", "2" = "ABUNDANCE"),
                  Type =  base::switch(
                    .ac(Type),
                    "0" = "TOTAL",
                    "1" = "RETAINED",
                    "2" = "DISCARD"
                  ),
                  fleet = obj$F_Fleet_names[fleet],
                  sex = base::switch(
                    .ac(sex),
                    "0" = "undetermined",
                    "1" = "male",
                    "2" = "female"
                  )
                ) %>% as.data.frame()
              Catch[[n_pos]][["unit"]] <- Spc$units
              Catch[[n_pos]][["type"]] <- Spc$Type
              Catch[[n_pos]][["fleet"]] <- Spc$fleet
              Catch[[n_pos]][["sex"]] <- Spc$sex
              Catch[[n_pos]][["maturity"]] <- "undetermined"
              Catch[[n_pos]][["shell_cond"]] <- "undetermined"
              Catch[[n_pos]][["Nrows"]] <- obj$Nrows_CatchDF[[n]]
              Catch[[n_pos]][["data"]] <- oldCatch[[n]] %>%
                dplyr::select(year,
                              seas,
                              obs,
                              CV,
                              mult,
                              effort,
                              discard_mortality) %>%
                as.data.frame()
              names(Catch)[n_pos] <- paste(base::switch(
                .ac(Spc$sex),
                "undetermined" = "Both",
                "male" = "Males",
                "female" = "Females"
              ),
              base::switch(
                .ac(Spc$Type),
                "TOTAL" = "Total",
                "RETAINED" = "Retained",
                "DISCARD" = "Discard"
              ),
              Spc$fleet,
              base::switch(
                .ac(Spc$units),
                "ABUNDANCE" = "Abundance",
                "BIOMASS" = "Biomass"
              ),
              sep = "_")
              # update n_pos
              n_pos <- n_pos + 1
            } # end condition on dim(Spc)[1]
          } # end loop on n
        } # end new catch format
        # Affect the new catch data format to obj
        obj$Catch <- Catch
      } # end obj$N_CatchDF
    } # end changes in catch data
    # ----------------------------------------------------------------------------


    # Survey data ----
    if (!is.null(SurveyDF_Newformat) &
        obj$SurveyDF_format != SurveyDF_Newformat) {
      # Update the format for the survey data
      obj$SurveyDF_format <- SurveyDF_Newformat

      # Check for changes in survey data
      if (obj$N_SurveyDF > 0) {
        # Check if there is at least 1 dataframe
        if (SurveyDF_Newformat == 0) {
          # set the old survey format

          obj$Nrows_SvDF <- NULL
          obj$Sv_type <- NULL
          oldSurvey <- obj$Surveys
          Surveys <-
            tmpSurveys <- NULL

          for (n in 1:length(oldSurvey)) {
            tmp <- oldSurvey[[n]]

            Survey_names <- which(tmp$fleet == obj$Survey_names)

            tmp$data <- tmp$data %>%
              dplyr::mutate(
                units = base::switch(
                  .ac(tmp$unit),
                  "biomass" = "1",
                  "abundance" = "2"
                ),
                Type =  base::switch(
                  .ac(tmp$type),
                  "sel" = "1",
                  "sel+ret" = "2"
                ),
                fleet = .an(Survey_names),
                sex = base::switch(
                  .ac(tmp$sex),
                  "undetermined" = "0",
                  "male" = "1",
                  "female" = "2"
                ),
                Mature = base::switch(
                  .ac(tmp$maturity),
                  "undetermined" = "0",
                  "mature" = "1",
                  "immature" = "2"
                )
              ) %>%
              dplyr::mutate_at(c('units', 'Type', 'fleet', 'sex', 'Mature'),
                               as.numeric) %>%
              dplyr::relocate(dplyr::any_of(
                c(
                  "Index",
                  "year",
                  "seas",
                  "fleet",
                  "sex",
                  "Mature",
                  "Abundance",
                  "CV",
                  "units",
                  "Timing"
                )
              )) %>%
              as.data.frame()

            # rbind the survey data
            tmpSurveys <- rbind(tmpSurveys, tmp$data)
          }

          # Set the Survey data as a list by fleet
          for (i in 1:max(tmpSurveys$fleet)) {
            if (i > 1)
              Surveys[[i]] <- list()
            # Create the survey data as the old format
            Surveys[[i]] <- tmpSurveys %>%
              dplyr::filter(fleet == i) %>%
              dplyr::select(-Type, -mult)
            names(Surveys)[i] <- obj$Survey_names[i]
            # Update the survey type
            obj$Sv_type[i] <- tmpSurveys %>%
              dplyr::filter(fleet == i) %>%
              dplyr::select(Type) %>% unique() %>% as.numeric()

          }
          # Update the vector of number of rows
          obj$Nrows_SvDF <- dim(tmpSurveys)[1]
          obj$N_SurveyDF <- length(unique(tmpSurveys$Index))
        } else {
          # set the new survey format
          oldSurvey <- obj$Surveys
          oldSurvey <-
            dplyr::bind_rows(oldSurvey, .id = "column_label")
          oldSurvey <- oldSurvey[order(oldSurvey$Index), ]

          Surveys <- NULL
          namFleets <- c(obj$F_Fleet_names, obj$Survey_names)
          nbOpt <- oldSurvey %>%
            dplyr::select(Index, fleet, sex, Mature, units) %>%
            unique() %>% as.data.frame
          nbDF <- (nbOpt %>% dim())[1]

          for (n in 1:nbDF) {
            if (n > 1)
              Surveys[[n]] <- list()

            tmp <- oldSurvey %>%
              dplyr::filter(
                Index == nbOpt$Index[n] & fleet  == nbOpt$fleet[n] &
                  sex == nbOpt$sex[n] &
                  Mature == nbOpt$Mature[n] &
                  units == nbOpt$units[n]
              ) %>%
              dplyr::mutate(mult = 1)

            Spc <- tmp %>%
              dplyr::select(fleet, sex, Mature, units) %>%
              unique() %>%
              dplyr::mutate(Type = obj$Sv_type[nbOpt$Index[n]]) %>%
              dplyr::mutate(
                # fleet = obj$Survey_names[unique(tmp$Index)[i]],
                fleet = namFleets[nbOpt$fleet[n]],
                sex = base::switch(
                  .ac(sex),
                  "0" = "undetermined",
                  "1" = "male",
                  "2" = "female"
                ),
                Mature = base::switch(
                  .ac(Mature),
                  "0" = "undetermined",
                  "1" = "mature",
                  "2" = "immature"
                ),
                units = base::switch(.ac(units), "1" = "biomass", "2" = "abundance"),
                Type =  base::switch(.ac(Type), "1" = "sel", "2" = "sel+ret")
              ) %>% as.data.frame()

            Surveys[[n]][["type"]] <- Spc$Type
            Surveys[[n]][["unit"]] <- Spc$units
            Surveys[[n]][["fleet"]] <- Spc$fleet
            Surveys[[n]][["sex"]] <- Spc$sex
            Surveys[[n]][["maturity"]] <- Spc$Mature
            Surveys[[n]][["shell_cond"]] <- "undetermined"
            Surveys[[n]][["Nrows"]] <- dim(tmp)[1]
            Surveys[[n]][["data"]] <- tmp %>%
              dplyr::select(Index, year, seas, Abundance, CV, mult, Timing) %>%
              as.data.frame()

            names(Surveys)[n] <- paste(base::switch(
              .ac(Spc$sex),
              "undetermined" = "Both",
              "male" = "Males",
              "female" = "Females"
            ), base::switch(
              .ac(Spc$Type),
              "sel" = "Sel",
              "sel+ret" = "SelRet"
            ), Spc$fleet, sep = "_")
          }
          # Update stuff in obj
          obj$N_SurveyDF <- nbDF
          obj$Nrows_SvDF <- ""
          obj$Sv_type <- ""
        } # end new survey format
        # Affect the new survey data format to obj
        obj$Surveys <- Surveys
      } # end obj$N_SurveyDF
    } # end changes in survey data
    # ----------------------------------------------------------------------------


    # Size composition data ----
    if (!is.null(SizeFreqDF_Newformat) &
        obj$SizeFreqDF_format != SizeFreqDF_Newformat) {
      # Update the format for the size freqency data
      obj$SizeFreqDF_format <- SizeFreqDF_Newformat

      # Check for changes in size comp data
      if (obj$N_SizeFreq_df > 0) {
        # Check if there is at least 1 dataframe
        if (SizeFreqDF_Newformat == 0) {
          # set the old size composition format

          namFleets <- c(obj$F_Fleet_names, obj$Survey_names)

          obj$Nrows_SiseFreqDF <- NULL
          obj$Nbins_SiseFreq <- NULL

          oldSizeFreq <- obj$SizeFreq
          SizeFreq <-
            tmpSizeFreq <- NULL

          for (n in 1:length(oldSizeFreq)) {
            tmp <- oldSizeFreq[[n]]

            SizeFreq_names <- which(tmp$fleet == namFleets)
            namBins <- paste0("bin_", 1:tmp$Nbins_SiseFreq)
            colnames(tmp$data) <- c(colnames(tmp$data)[colnames(tmp$data) !=
                                                         ""], namBins)

            tmp$data <- tmp$data %>%
              dplyr::mutate(
                Type =  base::switch(
                  .ac(tmp$type),
                  "TOTAL" = "0",
                  "Retained" = "1",
                  "Discard" = "2"
                ),
                fleet = .an(SizeFreq_names),
                sex = base::switch(
                  .ac(tmp$sex),
                  "undetermined" = "0",
                  "male" = "1",
                  "female" = "2"
                ),
                Maturity = base::switch(
                  .ac(tmp$maturity),
                  "undetermined" = "0",
                  "mature" = "1",
                  "immature" = "2"
                ),
                Shell =  base::switch(
                  .ac(tmp$shell_cond),
                  "undetermined" = "0",
                  "new shell" = "1",
                  "old shell" = "2"
                )
              ) %>%
              dplyr::mutate_at(c('Shell', 'Type', 'fleet', 'sex', 'Maturity'),
                               as.numeric) %>%
              dplyr::relocate(dplyr::any_of(
                c(
                  "year",
                  "seas",
                  "fleet",
                  "sex",
                  "Type",
                  "Shell",
                  "Maturity",
                  "Nsamp",
                  dplyr::all_of(namBins)
                )
              )) %>%
              as.data.frame()
            # rbind the size frequency data
            tmpSizeFreq <- rbind(tmpSizeFreq, tmp$data)
          }

          nbOpt <- tmpSizeFreq %>%
            dplyr::select(fleet, sex, Maturity, Type) %>%
            unique() %>% as.data.frame
          nbDF <- (nbOpt %>% dim())[1]

          # Set the size frequency data as a list
          for (i in 1:nbDF) {
            if (i > 1)
              SizeFreq[[i]] <- list()

            # Create the Size freq data as the old format
            SizeFreq[[i]] <- tmpSizeFreq %>%
              dplyr::filter(
                fleet == nbOpt$fleet[i] &
                  sex == nbOpt$sex[i] &
                  Maturity == nbOpt$Maturity[i] &
                  Type ==  nbOpt$Type[i]
              )
            names(SizeFreq)[i] <- namSizeFq(SizeFreq[[i]])


            # Update the Size frequency number of rows and bins
            obj$Nrows_SiseFreqDF[i] <- dim(SizeFreq[[i]])[1]
            obj$Nbins_SiseFreq[i] <-
              length(which(
                stringr::str_detect(
                  string = colnames(SizeFreq[[i]]),
                  pattern = "bin_"
                )
              ))
          }
          # Update the number of size frequency matrices
          obj$N_SizeFreq_df <- length(SizeFreq)

        } else {
          # set the new survey format

          oldSizeFreq <- obj$SizeFreq
          SizeFreq <- NULL
          namFleets <- c(obj$F_Fleet_names, obj$Survey_names)

          d <- 1 # pointer for the number of dataframe for each combinaison
          # fleet,sex,Type,Shell,Maturity from one dataframe from the old
          # format

          for (n in 1:obj$N_SizeFreq_df) {
            tmp <- oldSizeFreq[[n]]
            namBins <- paste0("bin_", 1:obj$Nbins_SiseFreq[n])
            colnames(tmp) <-
              c(colnames(tmp)[colnames(tmp) != ""], namBins)

            nbOpt <- tmp %>%
              dplyr::select(fleet, sex, Type, Shell, Maturity) %>%
              unique() %>% as.data.frame
            nbDF <- (nbOpt %>% dim())[1]

            for (a in 1:nbDF) {
              if (d > 1)
                SizeFreq[[d]] <- list()
              tmpOut <- tmp %>%
                dplyr::filter(
                  fleet == nbOpt$fleet[a] & sex == nbOpt$sex[a] &
                    Type ==  nbOpt$Type[a] &
                    Shell == nbOpt$Shell[a] &
                    Maturity == nbOpt$Maturity[a]
                )

              Spc <- tmpOut %>%
                dplyr::select(Type, fleet, sex, Maturity, Shell) %>%
                unique() %>%
                dplyr::mutate(
                  fleet = namFleets[nbOpt$fleet[a]],
                  sex = base::switch(
                    .ac(sex),
                    "0" = "undetermined",
                    "1" = "male",
                    "2" = "female"
                  ),
                  Maturity = base::switch(
                    .ac(Maturity),
                    "0" = "undetermined",
                    "1" = "mature",
                    "2" = "immature"
                  ),
                  Type =  base::switch(
                    .ac(Type),
                    "0" = "TOTAL",
                    "1" = "Retained",
                    "2" = "Discard"
                  ),
                  Shell = base::switch(
                    .ac(Shell),
                    "0" = "undetermined",
                    "1" = "new shell",
                    "2" = "old shell"
                  )
                ) %>% as.data.frame()

              SizeFreq[[d]][["type"]] <- Spc$Type
              SizeFreq[[d]][["fleet"]] <- Spc$fleet
              SizeFreq[[d]][["sex"]] <- Spc$sex
              SizeFreq[[d]][["maturity"]] <- Spc$Maturity
              SizeFreq[[d]][["shell_cond"]] <- Spc$Shell
              SizeFreq[[d]][["Nrows"]] <- dim(tmpOut)[1]
              SizeFreq[[d]][["Nbins_SiseFreq"]] <-
                obj$Nbins_SiseFreq[n]
              SizeFreq[[d]][["data"]] <- tmpOut %>%
                dplyr::select(year, seas, Nsamp, dplyr::all_of(namBins)) %>%
                as.data.frame()

              names(SizeFreq)[d] <- paste(base::switch(
                .ac(Spc$sex),
                "undetermined" = "Both",
                "male" = "Males",
                "female" = "Females"
              ),
              base::switch(
                .ac(Spc$Type),
                "TOTAL" = "Total",
                "RETAINED" = "Retained",
                "DISCARD" = "Discard"
              ),
              base::switch(
                .ac(Spc$Maturity),
                "undetermined" = "BothMat",
                "mature" = "Mature",
                "immature" = "Immat"
              ),
              Spc$fleet,
              sep = "_")

              d <- d + 1
            }
          }
          # Update stuff in obj
          obj$N_SizeFreq_df <- length(SizeFreq)
          obj$Nrows_SiseFreqDF <- ""
          obj$Nbins_SiseFreq <- ""
        } # end new size frequency format
        # Affect the new size frequency data format to obj
        obj$SizeFreq <- SizeFreq
      } # end obj$N_SizeFreq_df
    } # end changes in size frequency data
    # ----------------------------------------------------------------------------

    # Return obj with the new formats
    return(obj)
  }


  # Start the function ----
  File <- file.path(Dir, FileName)
  # Check the existence of the file
  if (file.exists(File)) {
    if (!overwrite) {
      FileName <-  stringr::str_split(FileName, "[.]")[[1]][1]
      tmp <-
        stringr::str_replace_all(string = Sys.Date(),
                                 pattern = "-",
                                 replacement = "_")
      message(
        FileName,
        "already exists and 'overwrite'=FALSE. The new data file
 will be saved as : ",
        FileName,
        "_new_",
        tmp,
        ".dat"
      )
      FileName <- paste0(FileName, "_new_", tmp, ".dat")
    }
  }
  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # Get GMACS version number and compilation date
  if(!is.null(Ver_number)){
    Ver <- paste0("GMACS Version: ",Ver_number)
    Comp <- "_Gmacs Development version"
  } else {
    tmp <- GMACSversion(Dir = DirTPL)
    Ver <- stringr::str_squish(tmp$ver)
    Comp <- tmp$Comp
  }


  obj <- DatFile

  # Check for changes in data format (catch; abundance indices and size composition)
  if (!is.null(CatchDF_format) || !is.null(SurveyDF_format) ||
      !is.null(SizeFreqDF_format)) {
    obj <- swithFormatDF(
      obj = obj,
      CatchDF_Newformat = CatchDF_format,
      SurveyDF_Newformat = SurveyDF_format,
      SizeFreqDF_Newformat = SizeFreqDF_format
    )

  }

  base::sink(FileName)
  cat("#_============================================================ #\n")
  cat("#                    GMACS data file \n")
  cat("# \n")
  cat("#_*** \n")
  cat(stringr::str_squish(string = paste0("#_", Ver)), "\n")
  if(is.null(Ver_number)){
    cat(stringr::str_squish(string = paste0("#_Last GMACS mofification made by: ", Comp)), "\n")
  } else {
    cat(stringr::str_squish(string = paste0("#", Comp)), "\n")
  }
  cat(stringr::str_squish(string = paste0("#_Date of writing the data file:", .ac(Sys.time()))), "\n")
  cat("#_*** \n")
  cat("# \n")
  cat(stringr::str_squish(string = paste0("#_Stock of interest: ", stock)), "\n")
  cat(stringr::str_squish(string = paste0("#_Model name: ", model_name)), "\n")
  cat(stringr::str_squish(string = paste0("#_Year of assessment: ", Ass_Year)), "\n")
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

  cat("#_Input format for catch data (0: old format; 1: new format)\n")
  cat(obj$CatchDF_format, "\n")
  cat("#_Number of catch data frames\n")
  cat(obj$N_CatchDF, "\n")

  if (obj$CatchDF_format == 0) {
    # old format
    cat("#_Number of rows in each data frame\n")
    if (obj$N_CatchDF > 0) {
      cat(obj$Nrows_CatchDF, "\n")
    } else {
      cat("\n")
    }
    cat("\n")
    cat("#_************************************** #\n")
    cat("#         ** CATCH DATA **\n")
    cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
    cat("#_Type of catch: 0 = total; 1 = retained; 2 = discard\n")
    cat("#_Units of catch: 1 = biomass; 2 = abundance (numbers)\n")
    cat(
      "#_Mult: 1 = use data as they are; otherwise (!=1) = multiply by this number (e.g., lbs to kg)\n"
    )
    cat(
      "#_Year_| Season_| Fleet_| Sex_| Obs_| CV_| Type_| Units_| Mult_| Effort_| Discard_mortality ##\n"
    )
    cat("#_************************************** #\n")
    cat("\n")
    if (obj$N_CatchDF > 0) {
      for (n in 1:obj$N_CatchDF) {
        if (n > 1)
          cat("\n")
        # cat("# **", names(obj$Catch)[n], " **\n")
        cat(namCatch(
          df = obj$Catch[[n]],
          format = obj$CatchDF_format
        ))
        cat(
          "# Year | Season | Fleet | Sex | Obs | CV | Type | Units | Mult | Effort | Discard_mortality\n"
        )
        utils::write.table(obj$Catch[[n]],
                           row.names = FALSE,
                           col.names = FALSE)
      }
    } else {
      cat(
        "# Year | Season | Fleet | Sex | Obs | CV | Type | Units | Mult | Effort | Discard_mortality\n"
      )
      cat("\n")
    }
  } else if (obj$CatchDF_format == 1) {
    # new format
    cat("\n")
    cat("#_************************************** #\n")
    cat("#         ** CATCH DATA **\n")
    cat("#_Units of catch: 'BIOMASS' = biomass; 'ABUNDANCE' = numbers\n")
    cat("#_Type of catch: 'TOTAL' = total; 'RETAINED' = retained; 'DISCARD' = discard\n")
    cat("#_Fleet name\n")
    cat("#_Sex: 'undetermined' = both; 'male' = male(s); 'female' = female(s)\n")
    cat("#_Maturity: 'undetermined' = both; 'immature'; 'mature'\n")
    cat("#_Shell condition: 'undetermined'; 'old shell'; 'new shell'\n")
    cat(
      "#_Mult: 1 = use data as they are; otherwise (!=1) = multiply by this number (e.g., lbs to kg)\n"
    )
    cat("#_Year_| Season_| Obs_| CV_| Mult_| Effort_| Discard_mortality ##\n")
    cat("#_************************************** #\n")
    if (obj$N_CatchDF > 0) {
      for (n in 1:obj$N_CatchDF) {
        # cat(namCatch(df = obj$Catch[[n]], format = obj$CatchDF_format))
        cat("\n")
        cat(
          paste0(
            "# ** Fleet: ",
            obj$Catch[[n]]$fleet,
            "; Catch type: ",
            obj$Catch[[n]]$type,
            "; Sex: ",
            obj$Catch[[n]]$sex,
            "; Season: ",
            unique(obj$Catch[[n]]$data["seas"]),
            "; Units:",
            obj$Catch[[n]]$unit,
            " **\n"
          )
        )
        cat("# ----------------------------------- \n")
        cat(obj$Catch[[n]]$unit, "\t\t\t#--Units type\n")
        cat(obj$Catch[[n]]$type, "\t\t\t\t#--Catch type\n")
        cat(obj$Catch[[n]]$fleet, "\t\t\t\t#--Fleet\n")
        cat(obj$Catch[[n]]$sex, "\t\t\t\t#--Sex\n")
        cat(obj$Catch[[n]]$maturity, "\t#--Maturity\n")
        cat(obj$Catch[[n]]$shell_cond, "\t#--Shell condition\n")
        cat(obj$Catch[[n]]$Nrows,
            "\t#--number of rows in the data frame\n")
        cat("# Year | Season | Obs | CV | Mult | Effort | Discard_mortality\n")
        utils::write.table(obj$Catch[[n]]$data,
                           row.names = FALSE,
                           col.names = FALSE)
        cat("# ----------------------------------- \n")
      }
    } else {
      cat("\n")
      cat("# Year | Season | Obs | CV | Mult | Effort | Discard_mortality\n")
      cat("\n")
    }
  } else if (obj$CatchDF_format > 1) {
    cat("The input format for catch data must be 0 (old format) or 1 (new format) !")
    stop()
  }
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Relative abundance data\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Input format for the relative abundance data (0: old format; 1: new format)\n")
  cat(obj$SurveyDF_format, "\n")
  if (obj$SurveyDF_format == 0) {
    cat("#_Number of relative abundance indices\n")
  } else {
    cat("#_Number of dataframe of relative abundance indices\n")
  }
  cat(obj$N_SurveyDF, "\n")

  if (obj$SurveyDF_format == 0) {
    # old format
    cat("#_Index Type (1 = Selectivity; 2 = Selectivity + retention)\n")
    if (obj$N_SurveyDF > 0) {
      cat(obj$Sv_type, "\n")
    } else {
      cat("\n")
    }
    cat("#_Total number of rows of index data\n")
    if (obj$N_SurveyDF > 0) {
      cat(obj$Nrows_SvDF, "\n")
    } else {
      cat("\n")
    }
    cat("\n")
    cat("#_************************************** #\n")
    cat("#     ** RELATIVE ABUNDANCE	DATA **     #\n")
    cat(
      "#_Index: One q is estimated for each index (the number of index values should match nSurveys)\n"
    )
    cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
    cat("#_Maturity: 0 = both; 1 = mature; 2 = immature\n")
    cat("#_Units of survey: 1 = biomass; 2 = numbers\n")
    cat(
      "#_Index_| Year_| Season_| Fleet_| Sex_| Maturity_| Obs_| CV_| Units_| CPUE_time ##\n"
    )
    cat("#_************************************** #\n")
    if (obj$N_SurveyDF > 0) {
      for (n in 1:obj$N_SurveyDF) {
        cat("\n")
        cat("# **", names(obj$Surveys)[n], " ** #\n")
        cat("# Index | Year | Season | Fleet | Sex | Maturity | Obs | CV | Units | CPUE_time\n")
        utils::write.table(obj$Surveys[[n]],
                           row.names = FALSE,
                           col.names = FALSE)
      }
    } else {
      cat("\n")
      cat("# Index | Year | Season | Fleet | Sex | Maturity | Obs | CV | Units | CPUE_time\n")
      cat("\n")
    }

  } else if (obj$SurveyDF_format == 1) {
    # new format
    cat("\n")
    cat("#_************************************** #\n")
    cat("#    ** RELATIVE ABUNDANCE DATA **       #\n")
    cat(
      "#_Index: One q is estimated for each index (the number of index values should match nSurveys)\n"
    )
    cat("#_Index type: 'sel' = selectivity; 'sel+ret'= selectivity & retention\n")
    cat("#_Units of survey: 'biomass'; 'abundance'\n")
    cat("#_Fleet name\n")
    cat("#_Sex: 'undetermined' = both; 'male' = male(s); 'female' = female(s)\n")
    cat("#_Maturity: 'undetermined' = both; 'immature'; 'mature'\n")
    cat("#_Shell condition: 'undetermined'; 'old shell'; 'new shell'\n")
    cat("#_Mult: 1 = use data as they are; 2 = multiply by this number (e.g., lbs to kg)\n")
    cat("#_Index_| Year_| Season_| Obs_| CV_| Mult_| CPUE_time ##\n")
    cat("#_************************************** #\n")
    if (obj$N_SurveyDF > 0) {
      for (n in 1:obj$N_SurveyDF) {
        tmp <- stringr::str_split(names(obj$Surveys)[n], pattern = "_")[[1]]

        cat("\n")
        cat("# ** Fleet: ",
            tmp[3],
            "; Index type: ",
            tmp[2],
            "; Sex: ",
            tmp[1],
            # "; Season: ",
            # stringr::str_remove(string = tmp[4], pattern = "Seas"),
            " **\n")
        cat("# ----------------------------------- \n")
        cat(obj$Surveys[[n]]$type, "\t\t\t\t#--Index type\n")
        cat(obj$Surveys[[n]]$unit, "\t\t\t#--Units type\n")
        cat(obj$Surveys[[n]]$fleet, "\t\t\t\t#--Fleet\n")
        cat(obj$Surveys[[n]]$sex, "\t\t\t\t#--Sex\n")
        cat(obj$Surveys[[n]]$maturity, "\t#--Maturity\n")
        cat(obj$Surveys[[n]]$shell_cond, "\t#--Shell condition\n")
        cat(obj$Surveys[[n]]$Nrows,
            "\t#--number of rows in the data frame\n")
        cat("# Index | Year | Season | Obs | CV | Mult | CPUE_time\n")
        utils::write.table(obj$Surveys[[n]]$data,
                           row.names = FALSE,
                           col.names = FALSE)
      }
    } else {
      cat("\n")
      cat("# Index | Year | Season | Obs | CV | Mult | CPUE_time\n")
      cat("\n")
    }
    # #############################################################################
    # #############################################################################
    # #############################################################################

  } else if (obj$SurveyDF_format > 1) {
    cat("The input format for relative abundance data must be 0 (old format) or 1 (new format) !")
    stop()
  }
  cat("#_-------------------------------------- #\n")
  cat("\n")

  cat("#_-------------------------------------- #\n")
  cat("##_Size composition for all fleets\n")
  cat("#_-------------------------------------- #\n")
  cat("#_Input format for the size composition data (0: old format; 1: new format)\n")
  cat(obj$SizeFreqDF_format, "\n")
  cat("#_Number of size frequency matrices\n")
  cat(obj$N_SizeFreq_df, "\n")


  if (obj$SizeFreqDF_format == 0) {
    # old format
    cat("#_Number of rows in each size frequency matrix\n")
    if (obj$N_SizeFreq_df > 0) {
      cat(obj$Nrows_SiseFreqDF, "\n")
    } else {
      cat("\n")
    }
    cat(
      "#_Number of bins in each length frequency matrix\n# (i.e., number of columns in each data frame\n"
    )
    if (obj$N_SizeFreq_df > 0) {
      cat(obj$Nbins_SiseFreq, "\n")
    } else {
      cat("\n")
    }
    cat("#_************************************** #\n")
    cat("#      ** SIZE COMPOSITION DATA **       #\n")
    cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
    cat("#_Type of catch: 0 = total; 1 = retained; 2 = discard\n")
    cat("#_Shell: 0 = both; 1 = new shell; 2 = old shell\n")
    cat("#_Maturity: 0 = both; 1 = mature; 2 = immature\n")
    cat("#_Nsamp: the stage-1 effective sample size (this can be modified in the .CTL file)\n")
    cat("#_Year_| Season_| Fleet_| Sex_| Type_| Shell_| Maturity_| Nsamp_| Data Vector ##\n")
    cat("#_************************************** #\n")
    if (obj$N_SizeFreq_df > 0) {
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
  } else if (obj$SizeFreqDF_format == 1) {
    # new format
    cat("#_************************************** #\n")
    cat("#      ** SIZE COMPOSITION DATA **       #\n")
    cat("#_Type of catch: 'TOTAL' = total; 'RETAINED' = retained; 'DISCARD' = discard\n")
    cat("#_Fleet name\n")
    cat("#_Sex: 'undetermined' = both; 'male' = male(s); 'female' = female(s)\n")
    cat("#_Maturity: 'undetermined' = both; 'immature'; 'mature'\n")
    cat("#_Shell condition: 'undetermined'; 'old shell'; 'new shell'\n")
    cat("#_Nsamp: the stage-1 effective sample size (this can be modified in the .CTL file)\n")
    cat("#_Year_| Season_| Nsamp_| Data Vector ##\n")
    cat("#_************************************** #\n")
    if (obj$N_SizeFreq_df > 0) {
      for (n in 1:obj$N_SizeFreq_df) {
        tmp <-
          stringr::str_split(names(obj$SizeFreq)[n], pattern = "_")[[1]]
        cat("\n")
        cat("# ** Fleet: ",
            tmp[4],
            "; Catch type: ",
            tmp[2],
            "; Sex: ",
            tmp[1],
            # "; Season: ",
            # stringr::str_remove(string = tmp[5], pattern = "Seas"),
            " **\n")
        cat("# ----------------------------------- \n")
        cat(obj$SizeFreq[[n]]$type, "\t\t\t\t#--Catch type\n")
        cat(obj$SizeFreq[[n]]$fleet, "\t\t\t\t#--Fleet\n")
        cat(obj$SizeFreq[[n]]$sex, "\t\t\t\t#--Sex\n")
        cat(obj$SizeFreq[[n]]$maturity, "\t#--Maturity\n")
        cat(obj$SizeFreq[[n]]$shell_cond, "\t#--Shell condition\n")
        cat(obj$SizeFreq[[n]]$Nrows,
            "\t#--number of rows in the data frame\n")
        cat(obj$SizeFreq[[n]]$Nbins_SiseFreq,
            "\t#--number of size bins in dataframe\n")
        cat("# Year | Season | Nsamp | Data Vector\n")
        utils::write.table(obj$SizeFreq[[n]]$data,
                           row.names = FALSE,
                           col.names = FALSE)
      }
    } else {
      cat("\n")
      cat("# Year | Season | Nsamp | Data Vector\n")
      cat("\n")
    }
    # #############################################################################
    # #############################################################################
    # #############################################################################

  } else if (obj$SizeFreqDF_format > 1) {
    cat("The input format for size composition data must be 0 (old format) or 1 (new format) !")
    stop()
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
  if (obj$GrowthObsType == 0) {
    cat(0, "\n")
  } else {
    cat(obj$NGrowthObs, "\n")
  }
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
  if (obj$NenvIndics > 0) {
    # cat(obj$EnvYrs, "\n")
    utils::write.table(obj$EnvYrs, row.names = FALSE, col.names = FALSE)
    cat("\n")
  } else {
    cat("\n")
  }
  cat("#_Environmental Indices\n")
  cat("#_Index_| Year_| Value\n")
  if (obj$NenvIndics > 0) {
    # cat(obj$EnvYrs, "\n")
    utils::write.table(obj$EnvData, row.names = FALSE, col.names = FALSE)
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
