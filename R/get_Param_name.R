#' @title Get the name of all parameters
#'
#' @description This functions returns the name of each parameter based on the
#' model specification (sexes, maturity, fleet, ...).
#'
#' @param DatFile (list)- Object containing the .dat file - This is the output
#' of the [readGMACSdat()] function.
#' @param CtlFile (list)- Object containing the .ctl file - This is the output
#' of the [readGMACSctl()] function.
#' @param nyrRetro (integer)- Number of year for the retrospective analysis
#'
#' @return the name as the parameters as a named list.
#'
#' @seealso \code{\link{readGMACSallOUT}}.
#'
#' @md
#
get_Param_name <- function(DatFile = NULL,
                           CtlFile = NULL,
                           nyrRetro = NULL) {
  nsex <- DatFile$N_sexes
  Start_Y <- DatFile$Start_Y
  End_Y <- DatFile$End_Y
  nyrRetro <- End_Y - nyrRetro
  nseason <- DatFile$N_seasons
  nclass <- DatFile$N_sizeC
  N_fleet <- DatFile$N_fleet
  nmature <- DatFile$N_maturity
  nshell <- DatFile$N_shell_cdt
  fleetname <-
    stringi::stri_remove_empty(c(DatFile$F_Fleet_names, DatFile$Survey_names))

  nam_sex <- base::switch(.ac(nsex),
                          "0" = "Undet_Both",
                          "1" = "Male",
                          "2" = c("Male", "Female"))
  nam_mat <- base::switch(.ac(nmature),
                          "1" = "BothMature",
                          "2" = c("Mature", "Immature"))
  nam_shell <- base::switch(.ac(nshell),
                            "1" = "Undet_shell",
                            "2" = c("New_shell", "Old_shell"))

  # @title getNrowsVulCtl
  #
  # @description Find the number of rows in each vulnerability matrix (selectivity and retention).
  #
  # @param slx_period the number of vulnerability periods for each fleet
  # @param bsex the vulnerability sex specific (1 value for each fleet)
  # @param type_in the type of vulnerability by sex
  # @param extra_in the number of extra parameter for each pattern
  # @param N_fleet the number of fleets (fishing fleet and surveys)
  # @param nsex the number of sex in the population
  # @param nclass the number of size class in the model
  #
  # @return the number of rows for the vulnerability matrix considered
  #
  getNrowsVulCtl <- function(slx_period = NULL,
                             bsex = NULL,
                             type_in = NULL,
                             extra_in = NULL,
                             N_fleet = NULL,
                             nsex = NULL,
                             nclass = NULL) {
    nslx_rows_in = 0
    slx_type_npar <- matrix(NA, nrow = nsex, ncol = N_fleet)

    for (f in 1:N_fleet) {
      nperiod <- slx_period[f]

      for (h in 1:(bsex[f] + 1)) {
        type <- base::switch (.ac(nsex),
                              "1" = type_in[f],
                              "2" = type_in[h, f])
        extra <- extra_in[h, f]

        if (type == 0) {
          # parametric (SELEX_PARAMETRIC)
          nslx_rows_in <- nslx_rows_in  + nclass * nperiod
          slx_type_npar[h, f] <- nclass
        }
        if (type == 1) {
          # coefficients (SELEX_COEFFICIENTS)
          nslx_rows_in <- nslx_rows_in  + extra * nperiod
          slx_type_npar[h, f] <- extra

        }
        if (type == 2) {
          # SELEX_STANLOGISTIC (logistic has 2 parameters)
          nslx_rows_in <- nslx_rows_in  + 2 * nperiod
          slx_type_npar[h, f] <- 2

        }
        if (type == 3) {
          # SELEX_5095LOGISTIC (logistic95 has 2 parameters)
          nslx_rows_in <- nslx_rows_in  + 2 * nperiod
          slx_type_npar[h, f] <- 2

        }
        if (type == 10) {
          # SELEX_ONE_PAR_LOGISTIC (logisticOne has 1 parameters)
          nslx_rows_in <- nslx_rows_in  + 1 * nperiod
          slx_type_npar[h, f] <- 1

        }
        if (type == 8) {
          # SELEX_DECLLOGISTIC (declining logistic has 2 + extra parameters)
          nslx_rows_in <- nslx_rows_in  + (2 + extra) * nperiod
          slx_type_npar[h, f] <- 2 + extra

        }
        if (type == 4) {
          # SELEX_DOUBLENORM (double normal has 3 parameters)
          nslx_rows_in <- nslx_rows_in  + 3 * nperiod
          slx_type_npar[h, f] <- 3

        }
        if (type == 7) {
          # SELEX_DOUBLENORM4 (double normal has 4 parameters)
          nslx_rows_in <- nslx_rows_in  + 4 * nperiod
          slx_type_npar[h, f] <- 4

        }
        if (type == 5) {
          # SELEX_UNIFORM1 (uniform has 1 parameter)
          nslx_rows_in <- nslx_rows_in  + 1 * nperiod
          slx_type_npar[h, f] <- 1

        }
        if (type == 6) {
          # SELEX_UNIFORM0 (uniform has 1 parameter)
          nslx_rows_in <- nslx_rows_in  + 1 * nperiod
          slx_type_npar[h, f] <- 1

        }
        if (type == 9) {
          # SELEX_CUBIC_SPLINE (spline has parameters for knots and values)
          nslx_rows_in <- nslx_rows_in  + 2 * extra * nperiod
          slx_type_npar[h, f] <- 2 * extra

        }
        if (type < 0) {
          # mirror has 1 parameter
          nslx_rows_in <- nslx_rows_in  + 1 * nperiod
          slx_type_npar[h, f] <- 1

        }
      }
    }
    out <- list(nslx_rows_in = nslx_rows_in,
                slx_type_npar = slx_type_npar)
    return(out)
  }
  # Get fhit and yhit for the fishing deviations
  fhit <- array(NA, dim = c(nyrRetro-Start_Y+1, nseason, N_fleet))
  yhit <- array(NA, dim = c(nyrRetro-Start_Y+1, nseason, N_fleet))
  rownames(fhit) <-
    rownames(yhit) <- Start_Y:nyrRetro
  for (k in 1:DatFile$N_CatchDF){
    for (i in 1:DatFile$Nrows_CatchDF[k])
      if (DatFile$Catch[[k]][i,"year"] <= nyrRetro && DatFile$Catch[[k]][i,"year"] >= Start_Y){
        y <- DatFile$Catch[[k]][i,"year"]
        j <- DatFile$Catch[[k]][i,"seas"]
        g <- DatFile$Catch[[k]][i,"fleet"]
        h <- DatFile$Catch[[k]][i,"sex"]

        if (is.na(fhit[rownames(fhit) == y,j,g])){
          fhit[rownames(fhit) == y,j,g] <- 1
        }

        if (is.na(yhit[rownames(fhit) == y,j,g]) && h == 2 ){
          yhit[rownames(fhit) == y,j,g] <- 1
        }
      }
  }
  # Total name
  name1 <- NULL

  # Get parameters from theta control matrix ----
  parname1 <- NULL
  # Natural mortality
  parname1 <- "M-base"
  if (nsex > 1) {
    if (CtlFile$MrelFem == 1)
      parname1 <- c(parname1, "M-fem-offset")
    if (CtlFile$MrelFem == 0)
      parname1 <- c(parname1, "M-female")
  }
  # Recruitment
  parname1 <- c(parname1, "Log_R0")
  parname1 <- c(parname1, "Log_Rinitial")
  parname1 <- c(parname1, "Log_Rbar")
  parname1 <- c(parname1, "Recruitment_ra-males")
  parname1 <- c(parname1, "Recruitment_rb-males")
  if (nsex > 1) {
    parname1 <- c(parname1, "Recruitment_ra-females")
    parname1 <- c(parname1, "Recruitment_rb-females")
  }
  parname1 <- c(parname1, "log_SigmaR")
  parname1 <- c(parname1, "Steepness")
  parname1 <- c(parname1, "Rho")
  # Estimate initial numbers as absolute
  anystring <- NULL
  Ipnt <- 0
  if (CtlFile$bInitializeUnfished == 2 ||
      CtlFile$bInitializeUnfished == 3) {
    for (s in  1:nsex)
      for (m in 1:nmature)
        for (o in 1:nshell) {
          for (l in 1:nclass)
          {
            if (CtlFile$bInitializeUnfished == 2) {
              anystring <-
                c(
                  anystring,
                  paste0(
                    "Initial_logN_for_",
                    nam_sex[s],
                    "_mature_",
                    m,
                    "_shell_",
                    o,
                    "_class_",
                    l
                  )
                )
            } else if (CtlFile$bInitializeUnfished == 3) {
              if (Ipnt == 0) {
                anystring <- NULL
              } else {
                anystring <-
                  c(
                    anystring,
                    paste0(
                      "Scaled_logN_for_",
                      nam_sex[s],
                      "_mature_",
                      m,
                      "_shell_",
                      o,
                      "_class_",
                      l
                    )
                  )
              }
              Ipnt <- Ipnt + 1
            }
          }
        }
  }
  parname1 <- c(parname1, anystring)
  name1 <- c(name1, parname1)
  # -------------------------------------------------------------------------

  # Get Growth & Molting parameters ----
  nameGrwth <- NULL
  for (h in 1:nsex)
    for (igrow in 1:CtlFile$nSizeIncVaries[h])
    {
      if (CtlFile$bUseGrowthIncrementModel == 1) {
        # LINEAR_GROWTHMODEL
        nameGrwth <-
          c(nameGrwth, paste0("Alpha_", nam_sex[h], "_period_", igrow))

        nameGrwth <-
          c(nameGrwth, paste0("Beta_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth,
            paste0("Gscale_", nam_sex[h], "_period_", igrow))
      }
      if (CtlFile$bUseGrowthIncrementModel == 2 ||
          CtlFile$bUseGrowthIncrementModel == 3) {
        #INDIVIDUAL_GROWTHMODEL1||INDIVIDUAL_GROWTHMODEL2
        for (l in 1:nclass) {
          nameGrwth <-
            c(
              nameGrwth,
              paste0(
                "Molt_increment_",
                nam_sex[h],
                "_period_",
                igrow,
                "_class_",
                l
              )
            )
        }
        nameGrwth <-
          c(nameGrwth,
            paste0("Gscale_", nam_sex[h], "_period_", igrow))
      }

      # Kappa varies
      if (CtlFile$bUseGrowthIncrementModel == 5) {
        #GROWTH_VARYK
        nameGrwth <-
          c(nameGrwth, paste0("Linf_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth, paste0("Kappa_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth,
            paste0("SigmaKappa_", nam_sex[h], "_period_", igrow))
      }
      # Linf varies
      if (CtlFile$bUseGrowthIncrementModel == 6) {
        #GROWTH_VARYLINF
        nameGrwth <-
          c(nameGrwth, paste0("Linf_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth, paste0("Kappa_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth,
            paste0("SigmaLinf_", nam_sex[h], "_period_", igrow))
      }
      # Linf and Kappa varies
      if (CtlFile$bUseGrowthIncrementModel == 7) {
        #GROWTH_VARYKLINF
        nameGrwth <-
          c(nameGrwth, paste0("Linf_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth, paste0("Kappa_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth,
            paste0("SigmaLinf_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth,
            paste0("SigmaKappa_", nam_sex[h], "_period_", igrow))
      }
    }

  for (h in 1:nsex)
    for (igrow in 1:CtlFile$nMoltVaries[h]) {
      if (CtlFile$bUseCustomMoltProbability == 2) {
        #LOGISTIC_PROB_MOLT
        nameGrwth <-
          c(nameGrwth,
            paste0("Molt_probability_mu_", nam_sex[h], "_period_", igrow))
        nameGrwth <-
          c(nameGrwth,
            paste0("Molt_probability_CV_", nam_sex[h], "_period_", igrow))
      }
      if (CtlFile$bUseCustomMoltProbability == 3) {
        #FREE_PROB_MOLT
        for (l in 1:nclass) {
          nameGrwth <-
            c(
              nameGrwth,
              paste0(
                "Molt_probability_",
                nam_sex[h],
                "_period_",
                igrow,
                "_class_",
                l
              )
            )
        }
      }
    }

  if(nmature == 2){
    for (h in 1:nsex)
      for (igrow in 1:CtlFile$nMatureVaries[h]) {
        if (CtlFile$bUseCustomMatureProbability == 2) {
          #LOGISTIC_PROB_MATURE
          nameGrwth <-
            c(nameGrwth,
              paste0("Mature_probability_mu_", nam_sex[h], "_period_", igrow))
          nameGrwth <-
            c(nameGrwth,
              paste0("Mature_probability_CV_", nam_sex[h], "_period_", igrow))
        }
        if (CtlFile$bUseCustomMatureProbability == 3) {
          #FREE_PROB_MATURE
          for (l in 1:nclass) {
            nameGrwth <-
              c(
                nameGrwth,
                paste0(
                  "Mature_probability_",
                  nam_sex[h],
                  "_period_",
                  igrow,
                  "_class_",
                  l
                )
              )
          }
        }
      }
  }
  name1 <- c(name1, nameGrwth)
  # -------------------------------------------------------------------------

  # Vulnerability parameter names ----
  selname1 <- NULL
  # Selectivity
  slx_type_npar <- getNrowsVulCtl(
    slx_period = CtlFile[["slx_nsel_period_in"]],
    bsex = CtlFile[["slx_bsex_in"]],
    type_in = CtlFile[["slx_type_in"]],
    extra_in = CtlFile[["slx_extra_in"]],
    N_fleet = N_fleet,
    nsex = nsex,
    nclass = nclass
  )[['slx_type_npar']]
  for (k in 1:N_fleet) {
    hh <- 1 + CtlFile$slx_bsex_in[k]
    for (h in 1:hh)
      for (i in 1:CtlFile$slx_nsel_period_in[k]) {
        for (ll in 1:slx_type_npar[h, k])
          selname1 <-
            c(selname1,
              paste0(
                "Sel_",
                fleetname[k],
                "_",
                nam_sex[h],
                "_period_",
                i,
                "_par_",
                ll
              ))
      }
  }
  # Retention
  ret_type_npar <- getNrowsVulCtl(
    slx_period = CtlFile[["ret_nret_period_in"]],
    bsex = CtlFile[["ret_bsex_in"]],
    type_in = CtlFile[["ret_type_in"]],
    extra_in = CtlFile[["ret_extra_in"]],
    N_fleet = N_fleet,
    nsex = nsex,
    nclass = nclass
  )[['slx_type_npar']]
  for (k in 1:N_fleet) {
    hh = 1 + CtlFile$ret_bsex_in[k]
    for (h in 1:hh)
      for (i in 1:CtlFile$ret_nret_period_in[k]) {
        for (ll in 1:ret_type_npar[h, k])
          selname1 <-
            c(selname1,
              paste0(
                "Ret_",
                fleetname[k],
                "_",
                nam_sex[h],
                "_period_",
                i,
                "_par_",
                ll
              ))
      }
  }
  name1 <- c(name1, selname1)
  # -------------------------------------------------------------------------

  # Asymptotic retention parameters ----
  Asymptname <- NULL
  if (CtlFile$NumAsympRet > 0) {
    for (Ipar in 1:CtlFile$NumAsympRet) {
      anystring <-
        paste0(
          "AsympRet_fleet_",
          fleetname[CtlFile$AsympSel_control[Ipar, "Fleet"]],
          "_sex_",
          nam_sex[CtlFile$AsympSel_control[Ipar, "Sex"]],
          "_year_",
          CtlFile$AsympSel_control[Ipar, "Year"]
        )


      Asymptname <- c(Asymptname, anystring)
    }
  }
  name1 <- c(name1, Asymptname)
  # -------------------------------------------------------------------------

  # Extract the time-varying parameters for the vulnerability ----
  selenvnames1 <-
    seldevnames1 <- NULL

  if (CtlFile$nslx_envpars > 0) {
    Selex_control <- rbind(CtlFile$Selex_control, CtlFile$Ret_control)
    nslx <- 0
    for (k in 1:N_fleet) {
      nslx = nslx + CtlFile$slx_nsel_period_in[k] * (1 + CtlFile$slx_bsex_in[k])

      nslx = nslx + CtlFile$ret_nret_period_in[k] * (1 + CtlFile$ret_bsex_in[k])

    }
    slx_type <- NULL
    kk <- 1
    for (k in 1:N_fleet) {
      hh <- 1 + CtlFile$slx_bsex_in[k]
      for (h in 1:hh) {
        for (i in 1:CtlFile$slx_nsel_period_in[k]) {
          slx_type[kk]  <- base::switch (.ac(nsex),
                                         "1" = CtlFile[["slx_type_in"]][k],
                                         "2" = CtlFile[["slx_type_in"]][h, k])
          kk <- kk + 1
        }
      }
    }
    for (k in 1:N_fleet) {
      hh = 1 + CtlFile$ret_bsex_in[k]

      for (h in 1:hh)
        for (i in 1:CtlFile$ret_nret_period_in[k]) {
          slx_type[kk]  <- base::switch (.ac(nsex),
                                         "1" = CtlFile[["ret_type_in"]][k],
                                         "2" = CtlFile[["ret_type_in"]][h, k])
          kk <- kk + 1
        }
    }
    # slx_indx is an ivector of the index for the first parameter of each nslx in the slx_control_in matrix
    kk <- 1
    slx_indx <- NULL
    for (k in 1:nslx) {
      slx_indx[k] <- kk
      kk <- kk + CtlFile$slx_cols[k]
    }
    nslx_envpars <-
      nseldevnames <- 0
    slx_timeVar <-
      slx_envlink <-
      slx_RdWalk_dev_type <-
      slx_styr_RdWalk <- slx_edyr_RdWalk <- NULL
    for (k in 1:nslx)
      slx_timeVar[k] = 0

    for (k in 1:nslx)
      if (slx_type[k] >= 0) {
        kk <- slx_indx[k]
        for (j in 1:CtlFile$slx_cols[k]) {
          jj <- kk + (j - 1)

          # Deal with environmental links
          slx_envlink[jj] <-  Selex_control[jj, "Env_Link"]
          slx_RdWalk_dev_type[jj] <-  Selex_control[jj, "Rand_Walk"]
          slx_styr_RdWalk[jj] <-  Selex_control[jj, "Start_year_RW"]
          slx_edyr_RdWalk[jj] <-  Selex_control[jj, "End_year_RW"]

          if (slx_envlink[jj] == 1) {
            nslx_envpars <- nslx_envpars + 1
            slx_timeVar[k] = 1
            selenvnames1[nslx_envpars] <-
              paste0("Selpattern_", k, "_par_", j, "_env_dev")
          }
          if (slx_RdWalk_dev_type[jj] == 1) {
            nslx_devpars <-
              nslx_devpars + (slx_edyr_RdWalk[j] - slx_styr_RdWalk[j] + 1)
            slx_timeVar[k] = 1
            for (ii in 1:(slx_edyr_RdWalk[j] - slx_styr_RdWalk[j]) + 1)
              seldevnames1[nseldevnames + ii] <-
              paste0("Selpattern_",
                     k,
                     "_par_",
                     j,
                     "_dev_for_year_",
                     slx_styr_RdWalk[j] + ii - 1)
            nseldevnames <-
              nseldevnames + (slx_edyr_RdWalk[j] - slx_styr_RdWalk[j]) + 1
          }
          if (slx_RdWalk_dev_type[jj] == 2) {
            nslx_devpars <-
              nslx_devpars + (slx_edyr_RdWalk[j] - slx_styr_RdWalk[j] + 1)
            slx_timeVar[k] = 1
            for (ii in 1:(slx_edyr_RdWalk[j] - slx_styr_RdWalk[j] + 1))
              seldevnames1[nseldevnames + ii] <-
              paste0("Selpattern_",
                     k,
                     "_par_",
                     j,
                     "_for_year_",
                     slx_styr_RdWalk[j] + ii - 1)
            nseldevnames <-
              nseldevnames + (slx_edyr_RdWalk[j] - slx_styr_RdWalk[j]) + 1
          }
        }
      }
  } else {
    selenvnames1[1] <- "Dummy_sel_env_par"
    seldevnames1[1] <- "Dummy_sel_dev_par"
  }
  name1 <- c(name1, selenvnames1, seldevnames1)
  # -------------------------------------------------------------------------

  # Extract fishing mortality parameters ----
  Sum_fleet <- do.call(rbind, DatFile$Catch)
  Sum_fleet <- Sum_fleet %>%
    dplyr::select(seas, fleet, year) %>%
    unique()

  # Extract the mean fishing mortality rate parameters
  nameFbar <- NULL
  for (f in 1:N_fleet)
    nameFbar <- c(nameFbar, paste0("log_fbar_", fleetname[f]))

  # Extract the male mean fishing mortality rate deviations
  out <- list()
  for (f in 1:N_fleet) {
    anystring <- NULL
    yearf <- Sum_fleet[Sum_fleet$fleet == f, "year"]
    seasf <- unique(Sum_fleet[Sum_fleet$fleet == f, "seas"])
  #   for (y in yearf)
  #     for (s in seasf)
  #       if(!is.na(fhit[rownames(fhit)==y, s, f]) && fhit[rownames(fhit)==y, s, f]>0){
  #         anystring <-
  #           c(anystring,
  #             paste0("Log_fdev_", fleetname[f], "_year_", y, "_season_", s))
  #       } else {
  #         anystring <- c(anystring, paste0("Log_fdev_", fleetname[f]))
  #       }
  #   out[[f]] <- anystring
  #
  #   if (!f %in% Sum_fleet$fleet)
  #     out[[f]] <- paste0("Log_fdev_", fleetname[f])
  # }
    if (!f %in% Sum_fleet$fleet){
      anystring <- paste0("Log_fdev_", fleetname[f])
    } else {
      for(s in seasf){
        if(any(!is.na(fhit[, s, f]))){
          for (y in yearf) {
            if(!is.na(fhit[rownames(fhit)==y, s, f]) && fhit[rownames(fhit)==y, s, f]>0){
              anystring <-
                c(anystring,
                  paste0("Log_fdev_", fleetname[f], "_year_", y, "_season_", s))
            }
          }
        }else {
          anystring <- paste0("Log_fdev_", fleetname[f])
        }
      }
    }
    out[[f]] <- anystring
  }

  names(out) <- fleetname
  nameFdev <- out

  # Extract the female F offset to male fishing mortality parameters
  anystring <- NULL
  for (f in 1:N_fleet)
    anystring <- c(anystring, paste0("Log_foff_", fleetname[f]))
  nameFoff <- anystring













  # Extract the female F deviation offset parameters
  out <- list()
  nameFdov <- NULL
  for (f in 1:N_fleet) {
    anystring <- NULL
    yearf <- Sum_fleet[Sum_fleet$fleet == f, "year"]
    seasf <- unique(Sum_fleet[Sum_fleet$fleet == f, "seas"])
      # for (y in yearf) {
      #   for (s in seasf) {
      #     if(!is.na(yhit[rownames(yhit)==y, s, f]) && yhit[rownames(yhit)==y, s, f]>0){
      #       anystring <-
      #         c(anystring,
      #           paste0("Log_fdov_", fleetname[f], "_year_", y, "_season_", s))
      #     } else {
      #       anystring <- c(anystring, paste0("Log_fdov_", fleetname[f]))
      #     }
      #   }
      # }
    if (!f %in% Sum_fleet$fleet){
      anystring <- paste0("Log_fdov_", fleetname[f])
    } else {
      for(s in seasf){
        if(any(!is.na(yhit[, s, f]))){
          for (y in yearf) {
            if(!is.na(yhit[rownames(yhit)==y, s, f]) && yhit[rownames(yhit)==y, s, f]>0){
              anystring <-
                c(anystring,
                  paste0("Log_fdov_", fleetname[f], "_year_", y, "_season_", s))
            }
          }
        }else {
          anystring <- paste0("Log_fdov_", fleetname[f])
        }
      }
    }
    out[[f]] <- anystring
  }
  names(out) <- fleetname
  nameFdov <- out
  name1 <- c(name1, nameFbar, nameFdev, nameFoff, nameFdov)
  # -------------------------------------------------------------------------

























  # Extract Recruitment parameters ----
  # Extract the initial values of recruitment
  nameRecrInit <- NULL
  for (c in 1:nclass) {
    nameRecrInit <- c(nameRecrInit, paste0("Rec_ini_size_class_", c))
  }
  # Extract the recruitment deviation estimates
  nameRecrDev <- NULL
  for (y in CtlFile$rdv_syr:CtlFile$rdv_eyr)
    nameRecrDev <- c(nameRecrDev, paste0("Rec_dev_est_", y))
  # Extract the sex-ratio recruitment deviation estimates
  nameRecrLogitProp <- NULL
  for (y in CtlFile$rdv_syr:CtlFile$rdv_eyr)
    nameRecrLogitProp <-
    c(nameRecrLogitProp, paste0("Logit_rec_prop_est_", y))
  name1 <- c(name1, nameRecrInit, nameRecrDev, nameRecrLogitProp)

  # -------------------------------------------------------------------------

  # Extract the natural mortality deviation parameters ----
  nameMortality <- NULL
  if (CtlFile$nMdev > 0) {
    for (d in 1:CtlFile$nMdev)
      nameMortality <- c(nameMortality, paste0("M_dev_est_par_", d))
    name1 <- c(name1, nameMortality)
  }
  # -------------------------------------------------------------------------

  # Extract the maturity specific natural mortality ----
  nameMat_NatMort <- NULL
  for (s in 1:nsex) {
    nameMat_NatMort <-
      c(nameMat_NatMort, paste0("m_mat_mult_", nam_sex[s]))
  }
  name1 <- c(name1, nameMat_NatMort)
  # -------------------------------------------------------------------------

  # Extract the effective sample size parameters ----
  nameEffSamp <- NULL
  for (n in 1:max(CtlFile$iCompAggregator))
    nameEffSamp <- c(nameEffSamp, paste0("Log_vn_comp_", n))
  name1 <- c(name1, nameEffSamp)
  # -------------------------------------------------------------------------

  #Extract the catchability coefficient parameters ----
  nameCatchability <- NULL
  for (n in 1:DatFile$N_SurveyDF)
    nameCatchability <-
    c(nameCatchability, paste0("Log_vn_comp_", n))
  name1 <- c(name1, nameCatchability)
  # -------------------------------------------------------------------------

  # Extract the addtional CV for surveys/indices parameters ----
  nameAddCV <- NULL
  for (n in 1:DatFile$N_SurveyDF)
    nameAddCV <- c(nameAddCV, paste0("Log_add_cvt_survey_", n))
  name1 <- c(name1, nameAddCV)
  # -------------------------------------------------------------------------

  return(
    list(
      nameTot = unlist(name1),
      nameTheta = parname1,
      nameGrwth = nameGrwth,
      selname1 = selname1,
      Asymptname = Asymptname,
      selenvnames1 = selenvnames1,
      seldevnames1 = seldevnames1,
      nameFbar = nameFbar,
      nameFdev = nameFdev,
      nameFoff = nameFoff,
      nameFdov = nameFdov,
      nameRecrInit = nameRecrInit,
      nameRecrDev = nameRecrDev,
      nameRecrLogitProp = nameRecrLogitProp,
      nameMortality = nameMortality,
      nameMat_NatMort = nameMat_NatMort,
      nameEffSamp = nameEffSamp,
      nameCatchability = nameCatchability,
      nameAddCV = nameAddCV
    )
  )
}
