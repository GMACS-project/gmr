#' @title Write the control file
#'
#' @description Write a new Spc.ctl file. This function is used to modify (within
#' R) a pre-existent Spc.ctl file.
#'
#' @param Dir (character string)- path where to save the new Spc.ctl file
#' @param FileName (character string)- name of the new Spc.ctl file
#' @param CtlFile (character string)- Object (list) containing the ex Spc.ctl file - The list is
#' created using the [readGMACSctl()] function.
#' @param DatFile  (list; optional)- Object containing the .dat file - This can either be the output
#' of the [readGMACSdat()] function. If \code{DatFile} is provided, then you don't need to provide
#' the following: \code{nsex; Start_Y; End_Y}
#' @param stock (character string)- name of the stock of interest
#' @param model_name (character string)- name of the model currently considered (e.g., "model 22.A")
#' @param Ass_Year (character string)- Year of this assessment
#' @param nsex (integer; optional)- Number of sexes considered in the model. This is required if you
#' don't provide the \code{DatFile}.
#' @param Start_Y;End_Y (integer; optional)- First and last year of the assessment period.
#' This is required if you don't provide the \code{DatFile}.
#'
#' @return create a new .ctl file.
#'
#' @seealso \code{\link{readGMACSctl}}
#'
#' @export
#' @md
#
writeGmacsctlfile <- function(Dir = NULL,
                              FileName = NULL,
                              CtlFile = NULL,
                              DatFile = NULL,
                              stock = "",
                              model_name = "",
                              Ass_Year = "",
                              nsex = NULL,
                              Start_Y = NULL,
                              End_Y = NULL) {


  # Check for arguments
  if(is.null(DatFile)){
    if(is.null(nsex))
      stop("You must specify the number of sexes in the model or fill the 'DatFile' argument of the function.")
    if(is.null(Start_Y))
      stop("You must specify the start year of the assessment or fill the 'DatFile' argument of the function.")
    if(is.null(End_Y))
      stop("You must specify the last year of the assessment or fill the 'DatFile' argument of the function.")
  } else {
    nsex <- DatFile$N_sexes
    Start_Y <- DatFile$Start_Y
    End_Y <- DatFile$End_Y
    Fleet_names <- c(DatFile$F_Fleet_names,DatFile$Survey_names)
  }


  FileName <- file.path(Dir, FileName)
  fs::file_create(FileName)

  # Get GMACS version number and compilation date
  tmp <- GMACSversion(Dir = Dir)
  Ver <- tmp$ver
  Comp <- tmp$Comp

  obj <- CtlFile

  base::sink(FileName)


  cat("# ============================================================ #\n")
  cat("#                  GMACS main control file \n")
  cat("# \n")
  cat("#_*** \n")
  cat("#_", Ver, "\n")
  cat("#_Last GMACS mofification made by: ", Comp, "\n")
  cat("#_Date of writing the control file:", .ac(Sys.time()), "\n")
  cat("#_*** \n")
  cat("# \n")
  cat("#_Stock of interest: ", stock, "\n")
  cat("#_Model name: ", model_name, "\n")
  cat("#_Year of assessment: ", Ass_Year, "\n")
  cat("# ============================================================ #\n")
  cat("\n")
  cat("# -------------------------------------- #\n")
  cat("##_Key parameter controls\n")
  cat("# -------------------------------------- #\n")
  cat("#_ntheta - Number of leading parameters (guestimated)\n")
  cat(obj$ntheta, "\n")
  cat("#\n")
  cat("#_Core parameters\n")
  cat("# ************************************** #\n")
  cat("#_For each parameter columns are:\n")
  cat("#_Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("#_Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("#_Phase: Set equal to a negative number not to estimate\n")
  cat("#_Available prior types:\n")
  cat("#_-> 0 = Uniform   - parameters are the range of the uniform prior\n")
  cat("#_-> 1 = Normal    - parameters are the mean and sd\n")
  cat("#_-> 2 = Lognormal - parameters are the mean and sd of the log\n")
  cat("#_-> 3 = Beta      - parameters are the two beta parameters [see dbeta]\n")
  cat("#_-> 4 = Gamma     - parameters are the two gamma parameters [see dgamma]\n")
  cat("#_p1; p2: priors\n")
  cat("# ************************************** #\n")
  cat("# \n")
  cat("#_Init_val_| Lower_Bd_| Upper_Bd_| Phase_| Prior_type_| p1_| p2\n")
  utils::write.table(obj$theta_control,
                     row.names = FALSE,
                     col.names = FALSE)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_Allometry\n")
  cat("# -------------------------------------- #\n")
  cat("#_Length-weight type/method\n")
  cat("#_1 = Length-weight relationship (vector of sex specific parameters: w_l = a[s]*l^b[s])\n")
  cat("#_2 = Input vector of mean weight-at-size by sex (dim=[1:nclass])\n")
  cat("#_3 = Input matrix of mean weight-at-size by sex and year (dim=[nsex*Nyear; nclass])\n")
  cat(obj$lw_type, "\n")

  if(obj$lw_type == 1){
    cat("#_lw_alfa\n")
    cat(obj$lw_alfa, "\n")
    cat("#_lw_beta\n")
    cat(obj$lw_beta, "\n")

  } else if(obj$lw_type == 2){

    if(nsex == 0 || nsex == 1){
      base::switch(.ac(nsex),
                   "0" = cat("#_Vector of combined (males & females) mean weight-at-size\n"),
                   "1" = cat("#_vector of male mean weight-at-size\n"))
      utils::write.table(obj$mean_wt_in, col.names = FALSE, row.names = FALSE)

    } else {
      cat("#_vector of male mean weight-at-size\n")
      utils::write.table(obj$mean_wt_in[1,], col.names = FALSE, row.names = FALSE)
      cat("#_vector of female mean weight-at-size\n")
      utils::write.table(obj$mean_wt_in[2,], col.names = FALSE, row.names = FALSE)
    }

  } else if (obj$lw_type == 3){

    if(nsex == 0 || nsex == 1){
      base::switch(.ac(nsex),
                   "0" = cat("#_Matrix of combined (males & females) mean weight-at-size\n"),
                   "1" = cat("#_Matrix of male mean weight-at-size\n"))
      utils::write.table(obj$mean_wt_in, col.names = FALSE, row.names = FALSE)
    } else {
      tmp <- End_Y + 1 - Start_Y + 1
      cat("#_Matrix of male mean weight-at-size\n")
      utils::write.table(obj$mean_wt_in[1:tmp,], col.names = FALSE, row.names = FALSE)
      cat("\n")
      cat("#_Matrix of female mean weight-at-size\n")
      utils::write.table(obj$mean_wt_in[(tmp+1):dim(obj$mean_wt_in)[1],], col.names = FALSE, row.names = FALSE)
    }
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_Fecundity for MMB/MMA calculation\n")
  cat("# -------------------------------------- #\n")
  cat("#_Maturity definition: Proportion of mature at size by sex\n")
  utils::write.table(obj$maturity, col.names = FALSE, row.names = FALSE)
  cat("#_Legal definition of the proportion of mature at size by sex\n")
  utils::write.table(obj$legal_maturity, col.names = FALSE, row.names = FALSE)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_Growth parameter controls\n")
  cat("# -------------------------------------- #\n")
  cat(" \n")
  cat("#_Two lines for each parameter are required if the model considers two sexes, one line is not\n")
  cat(" \n")

  cat("#_Growth transition matrix definition\n")
  cat("# ************************************** #\n")
  cat("#_1 = Fixed growth transition matrix (requires molt probability)\n")
  cat("#_2 = Fixed size transition matrix (molt probability is ignored)\n")
  cat("#_3 = Growth increment is gamma distributed\n")
  cat("#_4 = Size after growth is gamma distributed\n")
  cat("#_5 = kappa varies among individuals\n")
  cat("#_6 = Linf varies among individuals\n")
  cat("#_7 = kappa and Ling varies among individuals\n")
  cat("#_8 = Growth increment is normally distributed\n")
  cat("# ************************************** #\n")
  cat(obj$bUseCustomGrowthMatrix, "\n")
  cat(" \n")

  cat("#_Growth increment model matrix\n")
  cat("# ************************************** #\n")
  cat("#_0 = Pre-specified growth increment\n")
  cat("#_1 = linear (alpha; beta parameters)\n")
  cat("#_2 = Estimated by size-class\n")
  cat("#_3 = Pre-specified by size-class (empirical approach)\n")
  cat("# ************************************** #\n")
  cat(obj$bUseGrowthIncrementModel,"\n")
  cat(" \n")

  cat("#_Molt probability function\n")
  cat("# ************************************** #\n")
  cat("#_0 = Pre-specified probability of molting\n")
  cat("#_1 = Constant probability of molting (flat approach)\n")
  cat("#_2 = Logistic function\n")
  cat("#_3 = Free estimated parameters\n")
  cat("# ************************************** #\n")
  cat("#_If the custom growth model option = 1 then the molt probability function must be 1 \n")
  cat(obj$bUseCustomMoltProbability, "\n")
  # cat(" \n")

  cat("#_Maximum of size-classes to which recruitment must occur (males then females)\n")
  # if(nsex == 1){
  cat(obj$nSizeClassRec, "\n")
  # } else {
  # utils::write.table(obj$nSizeClassRec, col.names = FALSE, row.names = FALSE)
  # }
  cat("#_Number of blocks of growth matrix parameters (i.e., number of size-increment period)\n")
  cat(obj$nSizeIncVaries, "\n")
  cat("#_Year(s) with changes in the growth matrix\n")
  cat("#_-> 1 line per sex - blank if no change (i.e., if the number of blocks of growth matrix parameters = 1)\n")
  tmp <- NULL
  for(s in 1:nsex){
    if((obj$nSizeIncVaries[s]-1)>0){
      tmp <- rbind(tmp,obj$iYrsSizeIncChanges[s,])
    }
  }
  cat(unlist(tmp), "\n")

  cat("#_Number of blocks of molt probability\n")
  cat(obj$nMoltVaries, "\n")
  cat("#_Year(s) with changes in molt probability\n")
  cat("#_-> 1 line per sex - blank if no change (i.e., if the number of blocks of growth matrix parameters = 1)\n")
  tmp <- NULL
  for(s in 1:nsex){
    if((obj$nMoltVaries[s]-1)>0){
      tmp <- rbind(tmp,obj$iYrsMoltChanges[s,])
    }
  }
  # cat(tmp, "\n")
  utils::write.table(tmp, col.names = FALSE, row.names = FALSE)


  cat("#_Are the beta parameters relative to a base level?\n")
  cat(obj$BetaParRelative, "\n")
  cat("\n")

  cat("#_Growth increment model controls\n")
  cat("# ************************************** #\n")
  cat("#_For each parameter columns are:\n")
  cat("#_Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("#_Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("#_Phase: Set equal to a negative number not to estimate\n")
  cat("#_Available prior types:\n")
  cat("#_-> 0 = Uniform   - parameters are the range of the uniform prior\n")
  cat("#_-> 1 = Normal    - parameters are the mean and sd\n")
  cat("#_-> 2 = Lognormal - parameters are the mean and sd of the log\n")
  cat("#_-> 3 = Beta      - parameters are the two beta parameters [see dbeta]\n")
  cat("#_-> 4 = Gamma     - parameters are the two gamma parameters [see dgamma]\n")
  cat("#_p1; p2: priors\n")
  cat("# ************************************** #\n")
  cat("# \n")
  cat("#_Init_val_| Lower_Bd_| Upper_Bd_| Phase_| Prior_type_| p1_| p2\n")
  utils::write.table(obj$Grwth_control[1:obj$nGrwth,], col.names = FALSE, row.names = FALSE)
  cat("\n")

  cat("#_Molt probability controls\n")
  cat("# ************************************** #\n")
  cat("#_For each parameter columns are:\n")
  cat("#_Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("#_Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("#_Phase: Set equal to a negative number not to estimate\n")
  cat("#_Available prior types:\n")
  cat("#_-> 0 = Uniform   - parameters are the range of the uniform prior\n")
  cat("#_-> 1 = Normal    - parameters are the mean and sd\n")
  cat("#_-> 2 = Lognormal - parameters are the mean and sd of the log\n")
  cat("#_-> 3 = Beta      - parameters are the two beta parameters [see dbeta]\n")
  cat("#_-> 4 = Gamma     - parameters are the two gamma parameters [see dgamma]\n")
  cat("#_p1; p2: priors\n")
  cat("# ************************************** #\n")
  cat(" \n")
  cat("#_Init_val_| Lower_Bd_| Upper_Bd_| Phase_| Prior_type_| p1_| p2\n")
  utils::write.table(obj$MoltProb_control, col.names = FALSE, row.names = FALSE)
  cat("\n")
  cat("#_Custom growth-increment matrix or size-transition matrix (if any)\n")
  if(obj$bUseCustomGrowthMatrix == 1 || obj$bUseCustomGrowthMatrix == 2){
    utils::write.table(obj$CustomGrowthMatrix, col.names = FALSE, row.names = FALSE)
  } else {
    cat("# \n")
  }
  cat("\n")
  cat("#_Custom molt probability matrix  (if any)\n")
  if(obj$bUseCustomMoltProbability == 0){
    utils::write.table(obj$CustomMoltProbabilityMatrix, col.names = FALSE, row.names = FALSE)
  } else {
    cat("# \n")
  }
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##_Vulnerability parameter controls\n")
  cat("# \n")
  cat("#_Vulnerability is the combination of selectivity and retention selectivity.\n")
  cat("#_Gmacs requires that each gear has a vulnerability.\n")
  cat("# \n")
  cat("# -------------------------------------- #\n")
  cat("# \n")
  cat("#_For each of the vulnerability component (selectivity and retention), the following need to be specified:\n")
  cat("# ************************************** #\n")
  cat("#_Component periods: Number of component time periods\n")
  cat("#_Sex specific component: 0 = No; 1 = Yes\n")
  cat("#_Vulnerability types\n")
  cat("#_-> <0 = Mirror vulnerability component\n")
  cat("#_-> 0 = Nonparameric component (one parameter per class)\n")
  cat("#_-> 1 = Nonparameric component (one parameter per class, constant from last specified class)\n")
  cat("#_-> 2 = Logistic component (inflection point and slope)\n")
  cat("#_-> 3 = Logistic component (50% and 95% selection)\n")
  cat("#_-> 4 = Double normal component (3 parameters)\n")
  cat("#_-> 5 = Flat equal to one (1 parameter; phase must be negative)\n")
  cat("#_-> 6 = Flat equal to zero (1 parameter; phase must be negative) \n")
  cat("#_-> 7 = Flat-topped double normal component (4 parameters) \n")
  cat("#_-> 8 = Declining logistic component with initial values (50% and 95% selection plus extra) \n")
  cat("#_-> 9 = Cubic-spline (specified with knots and values at knots) \n")
  cat("#_-> 10 = One parameter logistic component (inflection point and slope) \n")
  cat("#_Is the fleet within another? (0 = No; 1 = Yes)\n")
  cat("#_Extra parameters for each pattern - 1 line per sex\n")
  cat("# \n")
  cat("#_Is the maximum selectivity at size forced to equal 1 or not ?\n")
  cat("# ************************************** #\n")
  cat(" \n")
  cat("#_The number of columns corresponds to the number of fleets (fisheries and surveys)\n")

  cat("# Selectivity\n")
  N_index <- length(obj$slx_nsel_period_in)

  cat("# ", paste("Gear-", 1: N_index, sep= "", collapse = " | "))
  if(!is.null(Fleet_names)){
    cat("# ",paste0(Fleet_names, collapse = " | "), "\n")
  }
  cat(obj$slx_nsel_period_in, "#_Number of selectivity time period per fleet\n")
  cat(obj$slx_bsex_in, "#_Sex specific selectivity\n")
  if(nsex==1){
    cat(obj$slx_type_in, "#_Selectivity type\n")
  } else {
    tmp1 <- obj$slx_type_in[1,]
    cat(unlist(tmp1), "#_Male selectivity type\n")
    tmp2 <- obj$slx_type_in[2,]
    cat(unlist(tmp2), "#_Female selectivity type\n")
  }
  cat(obj$slx_include_in, "#_Insertion of fleet in another\n")
  if(nsex==1){
    cat(obj$slx_extra_in, "#_Extra parameter for each pattern\n")
  } else {
    tmp1 <- obj$slx_extra_in[1,]
    cat(unlist(tmp1), "#_Extra parameters for each pattern by fleet (males)\n")
    tmp2 <- obj$slx_extra_in[2,]
    cat(unlist(tmp2), "#_Extra parameters for each pattern by fleet (females)\n")
  }
  cat("# \n")
  cat("#_Retention\n")
  cat("#_", paste("Gear-", 1: length(obj$slx_nsel_period_in), sep= "", collapse = " | "), sep="")
  if(!is.null(Fleet_names)){
    cat("#_",paste0(Fleet_names, collapse = "_| "), "\n", sep="")
  }
  cat(obj$ret_nret_period_in, "#_Number of Retention time period per fleet\n")
  cat(obj$ret_bsex_in, "#_Sex specific Retention\n")
  if(nsex==1){
    cat(obj$ret_type_in, "#_Selectivity type\n")
  } else {
    tmp1 <- obj$ret_type_in[1,]
    cat(unlist(tmp1), "#_Male Retention type\n")
    tmp2 <- obj$ret_type_in[2,]
    cat(unlist(tmp2), "#_Female Retention type\n")
  }
  if(nsex==1){
    cat(obj$slx_nret, "#_retention flag (0 = No, 1 = Yes)\n")
  } else {
    tmp1 <- obj$slx_nret[1,]
    cat(unlist(tmp1), "#_Male retention flag (0 = No, 1 = Yes)\n")
    tmp2 <- obj$slx_nret[2,]
    cat(unlist(tmp2), "#_Female retention flag (0 = No, 1 = Yes)\n")
  }
  if(nsex==1){
    cat(obj$ret_extra_in, "#_Extra parameter for each pattern\n")
  } else {
    tmp1 <- obj$ret_extra_in[1,]
    cat(unlist(tmp1), "#_Extra parameters for each pattern by fleet (males)\n")
    tmp2 <- obj$ret_extra_in[2,]
    cat(unlist(tmp2), "#_Extra parameters for each pattern by fleet (females)\n")
  }
  cat(obj$slx_max_at_1_in, "#_Selectivity for the maximum size class if forced to be 1?\n")
  cat(" \n")
  cat("# ====================================== #\n")
  cat("# ====================================== #\n")

  cat("\n")
  cat("#_Selectivity parameter controls\n")
  cat("# ************************************** #\n")
  cat("#_For each parameter (for each gear) columns are:\n")
  cat("#_Fleet: The index of the fleet (positive for capture selectivity)\n")
  cat("#_Index: Parameter count\n")
  cat("#_Par_no: Parameter count within the current pattern\n")
  cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
  cat("#_Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("#_Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("#_Available prior types:\n")
  cat("#_-> 0 = Uniform   - parameters are the range of the uniform prior\n")
  cat("#_-> 1 = Normal    - parameters are the mean and sd\n")
  cat("#_-> 2 = Lognormal - parameters are the mean and sd of the log\n")
  cat("#_-> 3 = Beta      - parameters are the two beta parameters [see dbeta]\n")
  cat("#_-> 4 = Gamma     - parameters are the two gamma parameters [see dgamma]\n")
  cat("#_p1; p2: priors\n")
  cat("#_Phase: Set equal to a negative number not to estimate\n")
  cat("#_Start / End block: years to define the current block structure\n")
  cat("#_Env_Link: Is there any environmental link for this parameter (0 = no; 1 = yes)\n")
  cat("#_Link_Par: If 'Env_Link'=1; indicate the link to the environmental parameter\n")
  cat("#_(i.e., which parameter (column) in the Envdata matrix)\n")
  cat("#_Rand_Walk: Is there a random walk (0/1/2)- If so (1/2), which type :\n")
  cat("#_1 = First order autoregressive process; 2 = gaussian white noise\n")
  cat("#_Start_RdWalk / End_RdWalk: years (start/end) to define the period for random walk deviations\n")
  cat("#_Sigma_RdWalk: sigma for the random walk\n")
  cat("# ************************************** #\n")
  cat(" \n")
  cat("#_Fleet_| Index_| Par_no_| Sex_| Init_val_| Lower_Bd_| Upper_Bd_| Prior_type_| p1_| p2_| Phase_| Start_Block_| End_Block_| Env_Link_| Link_Par_| Rand_Walk_| Start_RdWalk_| End_RdWalk_| Sigma_RdWalk\n", sep = "")
  for(g in 1:N_index){
    tmp <- obj$Selex_control[which(obj$Selex_control$Fleet==g),]
    if(!is.null(Fleet_names)){
      cat("#", Fleet_names[g]," \n")
    } else {
      cat("#", paste0("Gear-",g)," \n")
    }
    for(i in 1:dim(tmp)[1]){
      # tmp2 <- tmp[i,]
      # cat(unlist(tmp2)," \n")
      cat(unlist(tmp[i,])," \n")
    }
  }
  cat("\n")
  cat("#_Retention parameter controls\n")
  cat("# ************************************** #\n")
  cat("#_For each parameter (for each gear) columns are:\n")
  cat("#_Fleet: The index of the fleet (negative for retention)\n")
  cat("#_Index: Parameter count\n")
  cat("#_Par_no: Parameter count within the current pattern\n")
  cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
  cat("#_Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("#_Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("#_Available prior types:\n")
  cat("#_-> 0 = Uniform   - parameters are the range of the uniform prior\n")
  cat("#_-> 1 = Normal    - parameters are the mean and sd\n")
  cat("#_-> 2 = Lognormal - parameters are the mean and sd of the log\n")
  cat("#_-> 3 = Beta      - parameters are the two beta parameters [see dbeta]\n")
  cat("#_-> 4 = Gamma     - parameters are the two gamma parameters [see dgamma]\n")
  cat("#_p1; p2: priors\n")
  cat("#_Phase: Set equal to a negative number not to estimate\n")
  cat("#_Start / End block: years to define the current block structure\n")
  cat("#_Env_Link: Is there any environmental link for this parameter (0 = no; 1 = yes)\n")
  cat("#_Link_Par: If 'Env_Link'=1; indicate the link to the environmental parameter\n")
  cat("#_(i.e., which parameter (column) in the Envdata matrix)\n")
  cat("#_Rand_Walk: Is there a random walk (0/1/2)- If so (1/2), which type :\n")
  cat("#_1 = First order autoregressive process; 2 = gaussian white noise\n")
  cat("#_Start_RdWalk / End_RdWalk: years (start/end) to define the period for random walk deviations\n")
  cat("#_Sigma_RdWalk: sigma for the random walk\n")
  cat("# ************************************** #\n")
  cat(" \n")
  cat("#_Fleet_| Index_| Par_no_| Sex_| Init_val_| Lower_Bd_| Upper_Bd_| Prior_type_| p1_| p2_| Phase_| Start_Block_| End_Block_| Env_Link_| Link_Par_| Rand_Walk_| Start_RdWalk_| End_RdWalk_| Sigma_RdWalk\n", sep = "")
  for(g in 1:N_index){
    tmp <- obj$Ret_control[which(obj$Ret_control$Fleet==(-1*g)),]
    if(!is.null(Fleet_names)){
      cat("#", Fleet_names[g]," \n")
    } else {
      cat("#", paste0("Gear-",g)," \n")
    }
    for(i in 1:dim(tmp)[1]){
      # tmp2 <- tmp[i,]
      # cat(unlist(tmp2)," \n")
      cat(unlist(tmp[i,])," \n")
    }
  }
  cat("\n")
  cat("#_Number of asymptotic retention parameter\n")
  cat(obj$NumAsympRet, "\n")
  cat("#_Asymptotic parameter controls\n")
  cat("# ************************************** #\n")
  cat("#_Fleet: The index of the fleet (negative for retention)\n")
  cat("#_Sex: 0 = both; 1 = male; 2 = female\n")
  cat("#_Year: year of interest \n")
  cat("#_Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("#_Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("#_Phase: Set equal to a negative number not to estimate\n")
  cat("# ************************************** #\n")
  cat("#_Fleet_| Sex_| Year_| Init_val_| Lower_Bd_| Upper_Bd_| Phase \n")
  utils::write.table(obj$AsympSel_control, col.names = FALSE, row.names = FALSE)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("\n")
  cat("#_Environmental parameters Control\n")
  cat("# ************************************** #\n")
  cat("#_Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("#_Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("#_Phase: Set equal to a negative number not to estimate\n")
  cat("#_Init_val_| Lower_Bd_| Upper_Bd_| Phase \n")
  cat("# \n")
  cat("#_One line for each parameter ordered as the parameters are in the\n")
  cat("#_control matrices\n")
  cat("# ************************************** #\n")
  cat("\n")
  cat("#_Vulnerability impact")
  cat("#_Init_val_| Lower_Bd_| Upper_Bd_| Phase \n")
  if(obj$nslx_envpars > 0)
    utils::write.table(obj$SlxEnvPar, col.names = FALSE, row.names = FALSE)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("#_Deviation parameter phase for the random walk in vulnerability parameters\n")
  cat("#_Need to be defined\n")
  if(!is.null(obj$devParPhase)){
    cat(obj$devParPhase, "\n")
  } else {
    cat("-1")
  }
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Priors for catchability\n")
  cat("# -------------------------------------- #\n")
  cat(" \n")
  cat("# ************************************** #\n")
  cat("# Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("# Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("# Phase: Set equal to a negative number not to estimate\n")
  cat("# Available prior types:\n")
  cat("# -> 0 = Uniform   - parameters are the range of the uniform prior\n")
  cat("# -> 1 = Normal    - parameters are the mean and sd\n")
  cat("# -> 2 = Lognormal - parameters are the mean and sd of the log\n")
  cat("# -> 3 = Beta      - parameters are the two beta parameters [see dbeta]\n")
  cat("# -> 4 = Gamma     - parameters are the two gamma parameters [see dgamma]\n")
  cat("# p1; p2: priors\n")
  cat("# Q_anal: Do we need to solve analytically Q? (0 = No; 1 = Yes)\n")
  cat("# CV_mult: multiplier ofr the input survey CV\n")
  cat("# Loglik_mult: weight for the likelihood\n")
  cat("# ************************************** #\n")
  cat("# Init_val | Lower_Bd | Upper_Bd | Phase | Prior_type | p1 | p2 | Q_anal | CV_mult | Loglik_mult\n")
  utils::write.table(obj$q_controls, col.names = FALSE, row.names = FALSE)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Additional CV controls\n")
  cat("# -------------------------------------- #\n")
  cat(" \n")
  cat("# ************************************** #\n")
  cat("# Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("# Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("# Phase: Set equal to a negative number not to estimate\n")
  cat("# Available prior types:\n")
  cat("# -> 0 = Uniform   - parameters are the range of the uniform prior\n")
  cat("# -> 1 = Normal    - parameters are the mean and sd\n")
  cat("# -> 2 = Lognormal - parameters are the mean and sd of the log\n")
  cat("# -> 3 = Beta      - parameters are the two beta parameters [see dbeta]\n")
  cat("# -> 4 = Gamma     - parameters are the two gamma parameters [see dgamma]\n")
  cat("# p1; p2: priors\n")
  cat("# ************************************** #\n")
  cat("# Init_val | Lower_Bd | Upper_Bd | Phase | Prior_type| p1 | p2\n")
  utils::write.table(obj$add_cv_controls, col.names = FALSE, row.names = FALSE)
  cat(" \n")
  cat("# Additional variance control for each survey (0 = ignore; >0 = use)\n")
  cat(obj$add_cv_links, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")


  cat("# -------------------------------------- #\n")
  cat("## Penalties for the average fishing mortality rate\n")
  cat("# -------------------------------------- #\n")
  cat(" \n")
  cat("# ************************************** #\n")
  cat("# Fishing mortality controls\n")
  cat("# ************************************** #\n")
  cat("# Mean_F_male: mean male fishing mortality (base value for the fully-selected F) #\n")
  cat("# Female_Offset: Offset between female and male fully-selected F  #\n")
  cat("# Pen_std_Ph1 & Pen_std_Ph2: penalties on the fully-selected F during the early and later phase, respectively  #\n")
  cat("# Ph_Mean_F_male & Ph_Mean_F_female: Phases to estimate the fishing mortality for males and females, respectively #\n")
  cat("# Low_bd_mean_F & Up_bd_mean_F: Range for the mean fishing mortality (lower and upper bounds, respectivly) #\n")
  cat("# Low_bd_Y_male_F & Up_bd_Y_male_F: Range for the male fishing mortality (lower and upper bounds, respectivly) #\n")
  cat("# Low_bd_Y_female_F & Up_bd_Y_female_F: Range for the female fishing mortality (lower and upper bounds, respectivly)#\n")
  cat("# ************************************** #\n")
  cat("# ", paste(c(
    "Mean_F_male",
    "Female_Offset",
    "Pen_std_Ph1",
    "Pen_std_Ph2",
    "Ph_Mean_F_male",
    "Ph_Mean_F_female",
    "Low_bd_mean_F",
    "Up_bd_mean_F",
    "Low_bd_Y_male_F",
    "Up_bd_Y_male_F",
    "Low_bd_Y_female_F",
    "Up_bd_Y_female_F"
  ), sep = "", collapse = " | "), "\n")
  utils::write.table(obj$f_controls, col.names = FALSE, row.names = FALSE)
  cat("# -------------------------------------- #\n")
  cat("\n")


  cat("# -------------------------------------- #\n")
  cat("## Size composition data control\n")
  cat("# -------------------------------------- #\n")
  cat(" \n")
  cat("# ************************************** #\n")
  cat("# Available types of likelihood:\n")
  cat("# -> 0 = Ignore size-composition data in model fitting\n")
  cat("# -> 1 = Multinomial with estimated/fixed sample size\n")
  cat("# -> 2 = Robust approximation to multinomial\n")
  cat("# -> 5 = Dirichlet\n")
  cat("# Auto tail compression (pmin):\n")
  cat("# -> pmin is the cumulative proportion used in tail compression\n")
  cat("# Type-like prediction (1 = catch-like predictions; 2 = survey-like predictions)\n")
  cat("# Lambda: multiplier for the effective sample size\n")
  cat("# Emphasis: multiplier for weighting the overall likelihood\n")
  cat("# ************************************** #\n")
  cat(" \n")
  cat("# The number of columns corresponds to the number size-composition data frames\n")
  # cat("# Type of likelihood for the size-composition\n")
  cat(obj$nAgeCompType, "# Type of likelihood for the size-composition\n")
  # cat("# Option for the auto tail compression\n")
  cat(obj$bTailCompression, "# Option for the auto tail compression\n")
  # cat("# Initial value for effective sample size multiplier\n")
  cat(obj$nvn_ival, "# Initial value for effective sample size multiplier\n")
  # cat("# Phase for estimating the effective sample size\n")
  cat(obj$nvn_phz, "# Phase for estimating the effective sample size\n")
  # cat("# Composition appender (Should data be aggregated?)\n")
  cat(obj$iCompAggregator, "# Composition appender (Should data be aggregated?)\n")
  # cat("# Type-like predictions\n")
  cat(obj$lf_catch, "# Type-like predictions\n")
  # cat("# Lambda: multiplier for the effective sample size\n")
  cat(obj$lf_lambda, "# Lambda: multiplier for the effective sample size\n")
  # cat("# Emphasis: multiplier for weighting the overall likelihood\n")
  cat(obj$lf_emphasis, "# Emphasis: multiplier for weighting the overall likelihood\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Time-varying Natural mortality controls\n")
  cat("# -------------------------------------- #\n")
  cat(" \n")
  cat("# ************************************** #\n")
  cat("# Available types of M specification:\n")
  cat("# -> 0 = Constant natural mortality\n")
  cat("# -> 1 = Random walk (deviates constrained by variance in M)\n")
  cat("# -> 2 = Cubic Spline (deviates constrained by nodes & node-placement)\n")
  cat("# -> 3 = Blocked changes (deviates constrained by variance at specific knots)\n")
  cat("# -> 4 = Natural mortality is estimated as an annual deviation\n")
  cat("# -> 5 = Deviations in M are estimated for specific periods relatively to the M estimated in the first year of the assessment\n")
  cat("# -> 6 = Deviation in M are estimated for specific periods relatively to M during the current year\n")
  cat("# ************************************** #\n")
  cat("# Type of natural mortality\n")
  cat(obj$m_type, "\n")
  cat("# Is female M relative to M male?\n")
  cat("# 0: No (absolute); 1: Yes (relative) \n")
  if(nsex>1){
    if(is.numeric(obj$MrelFem)){
      cat(obj$MrelFem, "\n")
    } else {
      cat(0, "\n")
    }
  } else {
    cat("\n")
  }
  cat("# Phase of estimation\n")
  cat(obj$Mdev_phz_def, "\n")
  cat("# Standard deviation in M deviations\n")
  cat(obj$m_stdev, "\n")
  cat("# Number of nodes for cubic spline or number of step-changes for option 3\n")
  cat("# -> One line per sex\n")
  utils::write.table(obj$m_nNodes_sex, col.names = FALSE, row.names = FALSE)
  cat("# Year position of the knots for each sex (vector must be equal to the number of nodes)\n")
  cat("# -> One line per sex\n")
  if(obj$m_nNodes_sex != "" && is.vector(obj$m_nNodes_sex)){
    cat(obj$m_nodeyear_sex, "\n")
  } else if(is.data.frame(obj$m_nNodes_sex)){
    for(s in 1:nsex){
      cat(unlist(obj$m_nodeyear_sex[s,]), "\n")
    }
  }
  cat("# number of breakpoints in M by size\n")
  cat(obj$nSizeDevs, "\n")
  cat("# Size positions of breakpoints in M by size class\n")
  cat(obj$m_size_nodeyear, "\n")
  cat("# Specific initial value for natural mortality deviations\n")
  cat(obj$Init_Mdev, "\n")
  cat("# Natural mortality deviation controls\n")
  cat("# ************************************** #\n")
  cat("# Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("# Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("# Phase: Set equal to a negative number not to estimate\n")
  cat("# Size_spec: Are the deviations size-specific ? (integer that specifies which size-class (negative to be considered))\n")
  cat("# ************************************** #\n")
  cat("# Init_val | Lower_Bd | Upper_Bd | Phase | Size_spec\n")
  utils::write.table(obj$Mdev_controls, col.names = FALSE, row.names = FALSE)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Tagging controls\n")
  cat("# -------------------------------------- #\n")
  cat("# Emphasis (likelihood weight) on tagging\n")
  cat(obj$tag_emphasis, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("##  Immature/mature natural mortality \n")
  cat("# -------------------------------------- #\n")
  cat("# maturity specific natural mortality? ( 0 = No; 1 = Yes - only for use if nmature > 1)\n")
  cat(obj$m_maturity, "\n")
  cat("# immature/mature natural mortality controls\n")
  cat("# ************************************** #\n")
  cat("# Init_val: Initial value for the parameter (must lie between lower and upper bounds)\n")
  cat("# Lower_Bd & Upper_Bd: Range for the parameter\n")
  cat("# Phase: Set equal to a negative number not to estimate\n")
  cat("# Available prior types:\n")
  cat("# -> 0 = Uniform   - parameters are the range of the uniform prior\n")
  cat("# -> 1 = Normal    - parameters are the mean and sd\n")
  cat("# -> 2 = Lognormal - parameters are the mean and sd of the log\n")
  cat("# -> 3 = Beta      - parameters are the two beta parameters [see dbeta]\n")
  cat("# -> 4 = Gamma     - parameters are the two gamma parameters [see dgamma]\n")
  cat("# p1; p2: priors\n")
  cat("# ************************************** #\n")
  cat("# Init_val | Lower_Bd | Upper_Bd | Phase | Prior_type| p1 | p2\n")
  utils::write.table(obj$m_mat_controls, col.names = FALSE, row.names = FALSE)
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Other (additional) controls\n")
  cat("# -------------------------------------- #\n")
  cat("# First year of recruitment estimation deviations\n")
  cat(obj$rdv_syr, "\n")
  cat("# Last year of recruitment estimation deviations\n")
  cat(obj$rdv_eyr, "\n")
  cat("# Consider terminal molting? (0 = No; 1 = Yes\n")
  cat(obj$Term_molt, "\n")
  cat("# Phase for recruitment estimation\n")
  cat(obj$rdv_phz, "\n")
  cat("# Phase for recruitment sex-ratio estimation\n")
  cat(obj$rec_prop_phz, "\n")
  cat("# Initial value for expected sex-ratio\n")
  cat(obj$init_sex_ratio, "\n")
  cat("# Phase for initial recruitment estimation\n")
  cat(obj$rec_ini_phz, "\n")
  cat("# Verbose flag (0 = off; 1 = on; 2 = objective function; 3 = diagnostics)\n")
  cat(obj$verbose, "\n")
  cat("# Initial conditions (1 = unfished, 2 = steady-state, 3 = free params, 4 = free params revised)\n")
  cat(obj$bInitializeUnfished, "\n")
  cat("# Proportion of mature male biomass for SPR reference points\n")
  cat(obj$spr_lambda, "\n")
  cat("# Stock-Recruit-Relationship (0 = none, 1 = Beverton-Holt) \n")
  cat(obj$nSRR_flag, "\n")
  cat("# Maximum phase (stop the estimation after this phase)\n")
  cat(obj$TurnOffPhase, "\n")
  cat("# Maximum number of function calls\n")
  cat(obj$StopAfterFnCall, "\n")
  cat("# Calculate reference points (0 = No, 1 = Yes)\n")
  cat(obj$CalcRefPoints, "\n")
  cat("# Use years specified to computed average sex ratio in the calculation of average recruitment for reference points\n")
  cat("# -> 0 = No, i.e. Rec based on End year; 1 = Yes \n")
  cat(obj$BRP_rec_sexR, "\n")
  cat("# Years to compute equilibrium\n")
  cat(obj$NyrEquil, "\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## Emphasis factor (weights for likelihood) controls\n")
  cat("# -------------------------------------- #\n")
  cat("# Weights on catches for the likelihood component\n")
  cat(obj$catch_emphasis, "\n")
  cat("\n")
  cat("# Penalties on deviations\n")
  cat("# ************************************** #\n")
  cat("# ",paste(c("Fdev_total", "Fdov_total", "Fdev_year", "Fdov_year"), sep = "", collapse = " | "), "\n")
  utils::write.table(obj$Penalty_fdevs, col.names = FALSE, row.names = FALSE)
  cat("\n")
  cat("# Account for priors (penalties)\n")
  cat("# ************************************** #\n")
  # cat("# ", paste(c(
  #   "Log_fdevs",
  #   "meanF",
  #   "Mdevs",
  #   "Rec_devs",
  #   "Initial_devs",
  #   "Fst_dif_dev",
  #   "Mean_sex-Ratio",
  #   "Molt_prob",
  #   "Free_selectivity",
  #   "Init_n_at_len",
  #   "Fvecs",
  #   "Fdovs",
  #   "Vul_devs"
  # ), sep = "", collapse = " | "), "\n")
  # utils::write.table(obj$Penalty_emphasis, col.names = FALSE, row.names = FALSE)
  namPenal <- c(
    "Log_fdevs",
    "meanF",
    "Mdevs",
    "Rec_devs",
    "Initial_devs",
    "Fst_dif_dev",
    "Mean_sex-Ratio",
    "Molt_prob",
    "Free_selectivity",
    "Init_n_at_len",
    "Fvecs",
    "Fdovs",
    "Vul_devs"
  )
  for(i in 1:length(namPenal)){
    cat(obj$Penalty_emphasis[[i]], "\t#_",namPenal[i],"\n")
  }
  cat("\n")
  cat("# -------------------------------------- #\n")
  cat("\n")

  cat("# -------------------------------------- #\n")
  cat("## End of control file\n")
  cat("# -------------------------------------- #\n")
  cat(9999)
  cat("\n")

  base::sink()
}
