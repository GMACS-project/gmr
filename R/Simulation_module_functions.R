#' @title Get the original input files for the simulation approach
#'
#' @description Prepares the simulations:
#' 1. Creates a specific folder (\code{Simulations}) in the designated repertory of
#' the stock of interest
#' 2. Creates two sub folders (\code{True} and \code{Runs}) in the
#' \code{Simulations} folder to store the outputs of the simulations
#' 3. Reads the original input files that will be modified to simulate the data
#'
#' @param Dir (character string)- path to the repertory that holds the \code{"build"} folder.
#' @param Spc (character string)- the name of the stock of interest for the simulations
#' @param verbose (logical)- flag to print processing information
#'
#' @return A named list with the following arguments:
#' - \code{Dirlist}: named list of the working directory used and/or created,
#' - \code{GmacsFile}: the gmacs.dat file as a named list,
#' - \code{DatFile}: the Spc.dat file as a named list,
#' - \code{CtlFile}: the Spc.ctl file as a named list,
#' - \code{PrjFile}: the Spc.pj file as a named list,
#' - \code{ParFile}: the gmacs.par file as a named list,
#' - \code{OrigSimData}: the simdata.out file (the original one) as a named list.
#'
#' @seealso \code{\link{readGMACSfiles}}, \code{\link{readGMACSdat}},
#' \code{\link{readGMACSctl}}, \code{\link{readGMACSprj}}, \code{\link{readGMACSpar}},
#' \code{\link{readGMACSsimdat}}
#'
#' @export
#' @md
prepSim <- function(Dir = NULL,
                    Spc = NULL,
                    verbose = NULL){

  # options("scipen" = 100, "digits" = 14)


  oldWD = getwd();
  on.exit(setwd(oldWD));

  Dirlist <- list()

  Dir <- file.path(Dir, "build")
  Dirlist$Dir <- Dir
  DirStock <- file.path(Dir, Spc)
  Dirlist$DirStock <- DirStock

  # 1- build directory if applicable
  # ===================================== # ----
  if(verbose){
    cat("-- Checking the existence of the working directories\n")
    cat("\n")
  }

  Dirsim <- file.path(Dir, Spc, "Simulations")
  Dirlist$Dirsim <- Dirsim
  if (!dir.exists(Dirsim)){
    if(verbose)
      cat("\n")
    cat("\t->Creating the 'Simulations' folder in :\n", DirStock, "\n",sep="")
    dir.create(file.path(Dirsim), recursive = TRUE)
  }

  DirTrue <- file.path(Dirsim, 'True')
  Dirlist$DirTrue <- DirTrue
  DirRuns <- file.path(Dirsim, 'Runs')
  Dirlist$DirRuns <- DirRuns

  if (!dir.exists(DirTrue)){
    if(verbose)
      cat("\n")
    cat("\t->Creating the 'True' folder in :\n", Dirsim, "\n",sep="")
    dir.create(DirTrue, recursive = TRUE)
  }

  if (!dir.exists(DirRuns)){
    if(verbose)
      cat("\n")
    cat("\t->Creating the 'Runs' folder in :\n", Dirsim, "\n",sep="")
    dir.create(DirRuns, recursive = TRUE)
  }
  #  ------------------------------------

  # 2. Read the original input files
  # ===================================== # ----
  if(verbose){
    cat("\n")
    cat("-- Reading the original input files for :", Spc, "in :\n", DirStock, "\n")
  }

  Totfiles <- readGMACSfiles(Dir = DirStock, verbose = verbose)
  GmacsFile <- Totfiles$GmacsFile
  DatFile <- Totfiles$DatFile
  CtlFile <- Totfiles$CtlFile
  PrjFile <- Totfiles$PrjFile

  if(verbose){
    cat("\n")
    cat("\t-> OK after reading input files (gmacs.dat; .dat; .ctl; .prj) ...\n")
    cat("\n")
  }

  if(verbose)
    cat("-- Reading the gmacs.par and simdata.out files for :", Spc, "in :\n", DirStock,"\n")
  cat("\n")

  # Read the .par file
  ParFile <- readGMACSpar(
    Dir = DirStock,
    verbose = TRUE,
    DatFile = DatFile,
    CtlFile = CtlFile
  )
  if(verbose){
    cat("\n")
    cat("\t-> OK after reading gmacs.par file ...\n")
    cat("\n")
  }
  # Read the simdata.out file
  OrigSimData <-
    readGMACSsimdat(
      Dir = DirStock,
      verbose = TRUE,
      DatFile = DatFile,
      CtlFile = CtlFile
    )
  if(verbose){
    cat("\n")
    cat("\t-> OK after reading simdata.out file ...\n")
    cat("\n")
  }
  #  ------------------------------------

  Out <- NULL
  Out$Dirlist <- Dirlist
  Out$GmacsFile <- GmacsFile
  Out$DatFile <- DatFile
  Out$CtlFile <- CtlFile
  Out$PrjFile <- PrjFile
  Out$ParFile <- ParFile
  Out$OrigSimData <- OrigSimData

  return(Out)
}


#' @title Generate the input files to get the simulated data for each simulation
#'
#' @description Generates the input files for each of the Nsim simulations.
#' Specifically, this function re-write the original input files in the \code{True} folder
#' and:
#' 1. allows for considering random variation in recruitment (either by adding random
#' variations (normally distributed) to the original recruitment deviations or by
#' generating "new deviations",
#' 2. Set the maximum number of function calls to 1,
#' 3. call the gmacs.exe (with no estimation) to generate a new gmacsall.out file
#' that will be used for the simulations.
#'
#' The function is called for each simulation so it will generate Nsim gmacsall.out
#' which are copy and paste in the \code{Run} folder.
#'
#' @param Isim (integer)- simulation number (1 to Nsim)
#' @param OrigFiles (named list)- Output of the [prepSim()] function.
#' @param Ndev_Rec (integer)- Number of year to which apply a deviation in recruitment
#' @param SigmaR (numeric)- Standard deviation used in the [stats::rnorm()] function
#' @param DoRecDev (logical)- if \code{FALSE} : add deviation to the original ones,
#' if \code{TRUE} generate new deviations.
#' @param FunCall (logical)- Run the executable (with no estimation) ?
#' @param verbose (logical)- flag to print processing information.
#' to generate random deviations that will be added to the recruitment deviation.
#'
#' @return the SimData.out file as a named list for each simulation.
#'
#' @seealso \code{\link{readGMACSfiles}}, \code{\link{writeGmacs.dat}},
#'  \code{\link{readGMACSdat}},\code{\link{writeGmacsdatfile}},
#' \code{\link{readGMACSctl}}, \code{\link{writeGmacsctlfile}},
#' \code{\link{readGMACSprj}}, \code{\link{writeGmacsprjfile}},
#' \code{\link{readGMACSpar}},\code{\link{writeGmacsPAR}},
#' \code{\link{readGMACSsimdat}},
#'
#' @export
#' @md
#
GenSimFiles <- function(Isim = NULL,
                        OrigFiles = NULL,
                        Ndev_Rec = NULL,
                        SigmaR = NULL,
                        DoRecDev = NULL,
                        verbose = NULL,
                        FunCall = TRUE){

  gmacs_exe <- ifelse(isWindowsOS(),"gmacs.exe","gmacs")


  DirStock <- OrigFiles$Dirlist$DirStock
  DirTrue <- OrigFiles$Dirlist$DirTrue
  DirRuns <- OrigFiles$Dirlist$DirRuns

  # 1- Copy and paste the gmacs.exe in the True folder
  # ===================================== # ----
  if(Isim == 1)
    file.copy(file.path(DirStock, gmacs_exe), to = DirTrue, overwrite = TRUE, recursive = TRUE)
  # --------------------------------------

  # 2- Add random noise to the recruitment deviation estimates and save the .PIN file
  # ===================================== # ----
  parPIN <- OrigFiles$ParFile

  for (i in 1:Ndev_Rec){
    if(DoRecDev){
      parPIN$rec_dev_est[i] <- rnorm(1,0,SigmaR)
    }else {
      parPIN$rec_dev_est[i] <- parPIN$rec_dev_est[i]+rnorm(1,0,SigmaR)
    }
  }

  writeGmacsPAR(Dir = DirTrue,
                FileName = "gmacs.pin",
                gmacsPar = parPIN)
  # --------------------------------------

  # 3- Write and save the gmacs.dat in the TRUE folder
  # ===================================== # ----
  writeGmacs.dat(Dir = DirTrue,
                 FileName = "gmacs.dat",
                 gmacsDat = OrigFiles$GmacsFile)
  # --------------------------------------

  # 4- Write and save the Spc.dat in the TRUE folder
  # ===================================== # ----
  writeGmacsdatfile(Dir = DirTrue,
                    FileName = OrigFiles$GmacsFile$DatFileName,
                    DatFile = OrigFiles$DatFile)
  # --------------------------------------

  # 5- Change the Maximum number of function calls to 1 and
  # write and save the Spc.ctl in the TRUE folder
  # ===================================== # ----
  OrigFiles$CtlFile$StopAfterFnCall <- 1
  writeGmacsctlfile(Dir = DirTrue,
                    FileName = OrigFiles$GmacsFile$CtlFileName,
                    CtlFile = OrigFiles$CtlFile,
                    DatFile = OrigFiles$DatFile)
  # --------------------------------------

  # 6- Write and save the Spc.prj file in the TRUE folder
  # ===================================== # ----
  writeGmacsprjfile(Dir = DirTrue,
                    FileName = OrigFiles$GmacsFile$PrjFileName,
                    PrjFile = OrigFiles$PrjFile
  )
  # --------------------------------------

  # 7- Run the executable without estimation and copy the output to the RunFolder
  # ===================================== # ----

  command <- paste(ifelse(.Platform$OS.type=="windows",gmacs_exe,paste0("./",gmacs_exe)), " -est", sep = "")
  if(FunCall){
    tmp <- .CallTerm(command = command,
                     .Dir = DirTrue,
                     verbose = FALSE)

    while(is.null(rstudioapi::terminalExitCode(tmp))){
      Sys.sleep(0.1)
    }
  }


  namOut <- paste0("gmacsAll_True_", Isim, ".out")
  file.copy(file.path(DirTrue, "gmacsall.out"), to = file.path(DirRuns, namOut), overwrite = TRUE)
  # --------------------------------------

  # 8- Read the simdata.out generated for the simulation Isim and return it
  # ===================================== # ----
  SimFile <- readGMACSsimdat(Dir = DirTrue,
                             FileName = "simdata.out",
                             verbose = verbose,
                             CtlFile = OrigFiles$CtlFile,
                             DatFile = OrigFiles$DatFile)
  # --------------------------------------
  return(SimFile)
}


#' @title Generate the simulated data
#'
#' @description Generates the data for the simulation. The functions generate:
#' 1. catch data,
#' 2. Index/cpue data,
#' 3. length frequency data.
#'
#' All these data can either be considered as deterministic or random generation.
#' When noise is added, the function generates normally distributed deviations for
#' catch and cpue data while the deviations for the size composition data
#' can be generated either from a a multinormal or a dirichlet distribution.
#' The distribution considered is based on the specifications in the control file.
#'
#' @param Outres (named list)- list that holds the outputs of the [prepSim()] and
#' [GenSimFiles()] functions.
#' @param Rand (logical)- If true, random deviations are generated.
#'
#' @return a list that hold the simulated data.
#'
#' @export
#' @md
#
SimData <- function(Outres = NULL,
                    Rand = NULL){

  DatFile <- Outres$DatFile
  CtlFile <- Outres$CtlFile
  simIData <- Outres$simIData

  # 1. Internal functions ----
  # =========================================================================== #
  makeIframe <- function(Iyear,
                         outDat = NULL,
                         OldDat = NULL,
                         Random = NULL,
                         type = NULL
  ){
    if(type == "catch"){
      Obs <- "obs"
    } else {Obs <- "Abundance"}

    cv <- OldDat[Iyear,"CV"]
    eval(parse(text = paste0("OldObs <- OldDat[Iyear,'",Obs,"']")))

    gen <-
      sim <- outDat[Iyear]

    # Generate random noise?
    if(Random)
      gen <- gen*exp(rnorm(1,0,1)*cv-cv^2/2.0)

    if(OldObs >0){
      eval(parse(text = paste0("OldDat[Iyear, '",Obs,"'] <- gen")))

    } else {
      gen <- 0
      eval(parse(text = paste0("OldDat[Iyear, '",Obs,"'] <- gen")))

    }
  }
  # ===========================================================================


  # 2. Catch generation ----
  # =========================================================================== #

  # Old catch data frame
  OldCatchDat <- DatFile$Catch
  # Simulated catch
  CatchOut <- simIData$pre_catch
  # get new data frame
  for(Iframe in 1:DatFile$N_CatchDF){
    tmp <- NULL
    tmp <- sapply(X = 1:DatFile$Nrows_CatchDF[[Iframe]], makeIframe,outDat = CatchOut[[Iframe]], OldDat = OldCatchDat[[Iframe]],
                  Random = Rand,
                  type = "catch")
    Outres$DatFile$Catch[[Iframe]][,"obs"] <- tmp
  }
  # ===========================================================================


  # 3. CPUE/Index generation ----
  # =========================================================================== #

  # Old CPUE data frame
  OldCpueDat <- DatFile$Surveys
  # Simulated catch
  CpueOut_Iinit <- simIData$pre_cpue

  # Temporary rows survey
  Nrows_SvDF <- NULL
  for(Iframe in 1:DatFile$N_SurveyDF)
    Nrows_SvDF[[Iframe]] <- dim(OldCpueDat[[Iframe]])[1]

  # Transform the CpueOut to match the oldCpue data frame
  CpueOut <- list()

  for(Iframe in 1:DatFile$N_SurveyDF){
    CpueOut[[Iframe]] <- CpueOut_Iinit[1:Nrows_SvDF[[Iframe]]]
    CpueOut_Iinit <- CpueOut_Iinit[-c(1:Nrows_SvDF[[Iframe]])]
  }
  # get new data frame
  for(Iframe in 1:DatFile$N_SurveyDF){
    tmp <- NULL
    tmp <- sapply(X = 1:Nrows_SvDF[[Iframe]], makeIframe,
                  outDat = CpueOut[[Iframe]],
                  OldDat = OldCpueDat[[Iframe]],
                  Random = Rand,
                  type = "surveys")
    Outres$DatFile$Surveys[[Iframe]][,"Abundance"] <- tmp
  }
  # ===========================================================================





  # 4. Size-composition generation ----
  # =========================================================================== #
  GenVals <- list()
  nSizeDataS1 <- length(simIData$SizeDataS1)

  Nrows_SiseFreqDF <- DatFile$Nrows_SiseFreqDF
  N_SizeFreq_df <- DatFile$N_SizeFreq_df
  Nbins_SiseFreq <- DatFile$Nbins_SiseFreq

  iCompAggregator <- CtlFile$iCompAggregator
  nAgeCompType <- CtlFile$nAgeCompType


  # Compute the number of of effective length
  # ---------------------------------- #
  Nlen <- data.frame(cbind(iCompAggregator,
                           Nbins_SiseFreq,
                           Nrows_SiseFreqDF))
  Nlen$Nlen <- -1
  oldk <- 0

  for (n in 1:N_SizeFreq_df) {
    k <- iCompAggregator[n]

    if (oldk != k) {
      Nlen$Nlen[n] <- Nlen$Nbins_SiseFreq[n]
    } else {
      Nlen$Nlen[n-1] <- Nlen$Nbins_SiseFreq[n-1] + Nlen$Nbins_SiseFreq[n]
      Nlen$Nlen[n] <- Nlen$Nlen[n-1]
    }
    oldk <- k
  }
  NlenGen <- unique(Nlen[,!colnames(Nlen)%in%"Nbins_SiseFreq"])

  # Extract column names from the size-composition data
  Nam <- names(DatFile$SizeFreq[[1]])
  Nam <- Nam[which(Nam != "")]
  Nam <- c(Nam,  "sim_X", "sim_Nsamp")
  nNam <- length(Nam)
  # ----------------------------------


  # 4.2. Generated new values from the simulated data----
  # ---------------------------------- #

  for(d in 1:nSizeDataS1){
    type <- .ac(nAgeCompType[d])
    NlenDf <- NlenGen$Nlen[[d]]
    tmpDat <- simIData$SizeDataS1[[d]]
    newVals <- cbind(tmpDat[,colnames(tmpDat)%in%Nam], tmpDat[, nNam+NlenDf+1:NlenDf])

    Neff <- .an(tmpDat[,"sim_Nsamp"])

    if(Rand){
      vals <- NULL
      for(row in 1:NlenGen$Nrows_SiseFreqDF[d]){
        vals <- newVals[row, (nNam+1):dim(newVals)[2]]

        # if(type == "1" || type == "2"){
        #   newVals[row, (nNam+1):dim(newVals)[2]] <- .an(stats::rmultinom(1,Neff[row],prob=vals))
        # } else if (type == "5"){
        newVals[row, (nNam+1):dim(newVals)[2]] <- .an(gtools::rdirichlet(1,alpha=Neff[row]*vals/sum(vals)))
        # }
      }
    }
    newVals <- newVals[,!colnames(newVals)%in%c("sex", "Nsamp","sim_X", "sim_Nsamp")]
    # colnames(newVals)[which(colnames(newVals)=="sim_Nsamp")] <- "Nsamp"
    GenVals[[d]] <- newVals
  }
  # ----------------------------------



  # 4.3. Fill the data file with the new values----
  # ---------------------------------- #
  Nam <- Nam[!Nam%in%c("sim_X", "sim_Nsamp")]

  SizeFreq <- DatFile$SizeFreq
  N_sizeC <- DatFile$N_sizeC


  GenNam <- Nam[!Nam%in%c("sex", "Nsamp")]
  mergeNam <- paste0("'",Nam[!Nam%in%c("sex", "Nsamp")],"'", collapse = ",")

  tmp <- NULL


  for(d in 1:N_SizeFreq_df){

    tmp <- SizeFreq[[d]]
    tmp$ID <- d
    sex <- unique(tmp[,"sex"])
    fleet <- unique(tmp[,"fleet"])
    Nsamp <- unique(tmp[,"Nsamp"])
    IDgen <- Nlen$iCompAggregator[d]
    nbin <- Nlen$Nbins_SiseFreq[d]


    if(sex == 1 || (fleet ==1 && min(Nsamp)>0)){

      Ilast <- length(GenNam) + nbin

      eval(parse(text = paste(
        "tmp <- merge(tmp[,colnames(tmp)%in%Nam], GenVals[[IDgen]][,1:Ilast], by.y = c(",mergeNam, "))", sep=""
      )))
    } else {

      Istart <- length(GenNam) + N_sizeC + 1
      Ilast <- length(GenNam)+ N_sizeC + nbin


      tmpGen <- cbind(GenVals[[IDgen]][,colnames(GenVals[[IDgen]])%in%Nam], GenVals[[IDgen]][,Istart:Ilast])

      eval(parse(text = paste(
        "tmp <-merge(tmp[,colnames(tmp)%in%Nam], tmpGen, by.y = c(",mergeNam, "))", sep=""
      )))
    }
    tmp$rows <- 1:nrow(tmp)
    Want <- c(Nam, "rows")
    tmp1 <- tmp[Want]
    tmp2 <- tmp[,!colnames(tmp)%in%Nam]
    tmp <- merge(tmp1, tmp2, by.x = "rows")
    tmp <- tmp[,!colnames(tmp)%in%"rows"]

    SizeFreq[[d]] <- tmp
  }
  # ----------------------------------

  Outres$DatFile$SizeFreq <- SizeFreq

  # ===========================================================================
  return(Outres)
}


#' @title Save the outputs of each simulation-estimation run
#'
#' @description Save the outputs of each simulation
#'
#' For each simulation the function writes the gmacs.dat, data, control, and
#' projection files in the \code{Runs} folder created by the [prepSim()] functions.
#' The function also writes a batch command that will be called to when running
#' Gmacs (name of the file \code{RunSim.bat}).
#'
#' @param simDat (list)- the output of the [SimData()] function for each simulation
#' @param UsePar (logical)- Do the simulations use a .PIN file to import parameters?
#' @param NoEst (logical)- Set the maximum function call to 1? i.e., run estimation?
#' @param Isim (integer)- the current simulation
#' @param Nsim (integer)- the number of simulation to run
#'
#'
#' @return Nothing
#'
#' @export
#' @md
#
SaveSimFiles <- function(simDat = NULL,
                         UsePar = NULL,
                         NoEst = NULL,
                         Isim = NULL,
                         Nsim = NULL){

  gmacs_exe <- ifelse(isWindowsOS(),"gmacs.exe","gmacs")


  DirRuns <- simDat$Dirlist$DirRuns
  DirStock <- simDat$Dirlist$DirStock

  # 1. Write the new input files ----

  GmacsFileNames <- paste("Gmacs_",Isim,".dat",sep="")
  DatFileName <- paste("Gmacs_Sim",Isim,".dat",sep="")
  CtlFileName <- paste("Gmacs_Sim",Isim,".ctl",sep="")
  PrjFileName <- paste("Gmacs_Sim",Isim,".proj",sep="")


  simDat$GmacsFile$DatFileName <- DatFileName
  simDat$GmacsFile$CtlFileName <- CtlFileName
  simDat$GmacsFile$PrjFileName <- PrjFileName

  # Write gmacs.dat
  writeGmacs.dat(Dir = DirRuns,
                 FileName = GmacsFileNames,
                 gmacsDat = simDat$GmacsFile)

  # Write the Spc.dat
  writeGmacsdatfile(Dir = DirRuns,
                    FileName = simDat$GmacsFile$DatFileName,
                    DatFile = simDat$DatFile)

  # Change the Maximum number of function calls to 1 and ?
  # write the Spc.ctl
  if (NoEst==T)
    simDat$CtlFile$StopAfterFnCall <- 1
  writeGmacsctlfile(Dir = DirRuns,
                    FileName = simDat$GmacsFile$CtlFileName,
                    CtlFile = simDat$CtlFile,
                    DatFile = simDat$DatFile)


  # Write the Spc.prj
  writeGmacsprjfile(Dir = DirRuns,
                    FileName = simDat$GmacsFile$PrjFileName,
                    PrjFile = simDat$PrjFile)

  # Write the Spc.pin ?
  if(UsePar)
    writeGmacsPAR(Dir = DirRuns,
                  FileName = "gmacs.pin",
                  gmacsPar = simDat$ParFile)
  #  ----------------------------------------------



  # 2. Copy the gmacs.exe from the Stock directory to DirRuns ----

  file.copy(from=file.path(DirStock, gmacs_exe), to=DirRuns, overwrite = TRUE)

  #  ----------------------------------------------


}



#' @title Generates the simulated data for a set of simulation
#'
#' @description Function that generates the simulated data for a set of simulation
#' with Gmacs.
#'
#' @param path (character string)- file path representing root i.e., the folder where
#' the \code{gmacsbase.tpl} file is stored.
#' @param Stock (character string)- Name of the stock of interest. This will be used
#' to set the working directories: this is the name of the folder in the
#' \code{build} directory where the basic gmacs input files are stored.
#' @param RandomRec (logical)- if \code{FALSE} : add deviations to the original ones,
#' if \code{TRUE} generate new deviations.
#' @param Ndev_Rec (integer)- Number of year to which apply a deviation in recruitment.
#' @param SigmaR (numeric)- Standard deviation used in the [stats::rnorm()] function.
#' @param RandSim (logical)- If true, random deviations are generated.
#' @param UseParam (logical)- Do the simulations use a .PIN file to import parameters?
#' @param No_Est (logical)- Set the maximum function call to 1? i.e., run estimation?
#' @param Nsim (integer)- the number of simulation to run.
#' @param seed_val (integer) - a random number generation (single value; see [base::set.seed()]).
#' @param verbose (logical)- flag to print processing information
#'
#'
#' @return Nothing
#'
#' @export
#' @md
#
Gen_GmacsSim <- function(path = NULL,
                         Stock = NULL,
                         RandomRec = NULL,
                         Ndev_Rec = NULL,
                         SigmaR = NULL,
                         RandSim = NULL,
                         UseParam = NULL,
                         No_Est = NULL,
                         Nsim = NULL,
                         seed_val = NULL,
                         verbose = NULL) {

  # 1. Create the progress bar ----
  # ===================================================================== #
  pb <-
    tcltk::tkProgressBar(
      title = "Preparing the simulation files",
      min = 0,
      max = Nsim,
      width = 500,
    )
  # =====================================================================

  # 2. Set the seed and extract the data from the original files ----
  # ===================================================================== #
  set.seed(seed_val)
  OrigFiles <- prepSim(Dir = path,
                       Spc = Stock,
                       verbose = verbose)
  # =====================================================================



  # 3. Simulate the data ----
  # ===================================================================== #

  for (Isim in 1:Nsim) {
    # Print the progress bar
    Sys.sleep(0.1)
    tcltk::setTkProgressBar(pb, Isim,
                            label = paste("Generating simulation :",
                                          round(Isim / Nsim * 100, 0), "% done"))
    if (verbose)
      cat("-- Generating simulation ", Isim, " of ", Nsim, "\n")

    # 3.1 Generate the simulation files
    # ------------------------------------ #
    simIData <-
      GenSimFiles(
        Isim = Isim,
        OrigFiles = OrigFiles,
        Ndev_Rec = Ndev_Rec,
        SigmaR = SigmaR,
        DoRecDev = RandomRec,
        verbose = verbose
      )

    # 3.2 Store the data that have been simulated for simulation Isim
    OrigFiles$simIData <- simIData

    # 3.3 Generate the simulated data for Isim
    ResSim <- OrigFiles
    ResSim <- SimData(Outres = ResSim, Rand = RandSim)
    # ------------------------------------

    # 3.4 Save the files for the simulation ----
    # ------------------------------------ #
    SaveSimFiles(
      simDat = ResSim,
      UsePar = UseParam,
      NoEst = No_Est,
      Isim = Isim,
      Nsim = Nsim
    )
    if (verbose)
      cat("\t -> All files saved for simulation: ", Isim, "\n\n")
    # ------------------------------------

    if(Isim == Nsim)
      close(pb)
  } # end Nsim
  # ===================================================================== #
}



#' @title Run the GMACS simulation-estimation
#'
#' @description Executes the simulations with Gmacs using the data generated with
#' the [Gen_GmacsSim()] function.
#'
#' @param path (character string)- file path representing root i.e., the folder where
#' the \code{gmacsbase.tpl} file is stored.
#' @param Stock (character string)- Name of the stock of interest. This will be used
#' to set the working directories: this is the name of the folder in the
#' \code{build} directory where the basic gmacs input files are stored.
#' @param Nsim (integer)- the number of simulation that have been considered in
#' the analysis (see [Gen_GmacsSim()]).
#' @param verbose (logical)- flag to print processing information
#'
#'
#' @return Nothing
#'
#' @export
#' @md
#
RunGmacsSim <- function(path = NULL,
                        Stock = NULL,
                        Nsim = NULL,
                        verbose = NULL){

  gmacs_exe <- ifelse(isWindowsOS(),"gmacs.exe","gmacs")

  # 1. Create the progress bar ----
  # ===================================================================== #
  pb <-
    tcltk::tkProgressBar(
      title = "Execution of Gmacs for the simulation/estimation approach",
      min = 0,
      max = Nsim,
      width = 500,
    )
  # =====================================================================

  # 2. internal function ----
  # ===================================================================== #

  ExeCommand <- function(Com, dir, verbose){
    tmp <- .CallTerm(command = Com,
                     .Dir = dir,
                     verbose = verbose)

    while (is.null(rstudioapi::terminalExitCode(tmp))) {
      Sys.sleep(0.1)
    }
  }

  # =====================================================================

  # 3. Execute gmacs ----
  # ===================================================================== #

  DirRuns <- file.path(path,"build", Stock,"simulations","Runs")


  for (Isim in 1:Nsim) {
    # Print the progress bar
    Sys.sleep(0.01)
    tcltk::setTkProgressBar(pb, Isim,
                            label = paste(round(Isim / Nsim * 100, 0), "% done"))

    # 3.1 Copy the Gmacs_Isim.dat towards Gmacs.dat
    GmacsFileNames <- paste("Gmacs_",Isim,".dat",sep="")
    cpy_cmd = ifelse(.Platform$OS.type=="windows","Copy ","cp ");
    command1 <- paste0(cpy_cmd, GmacsFileNames," Gmacs.dat")
    ExeCommand(Com = command1, dir = DirRuns, verbose = verbose)

    # 3.2 Run Gmacs and store the execution windows in the a.a file
    command2 <- paste(ifelse(.Platform$OS.type=="windows",gmacs_exe,paste0("./",gmacs_exe)), " -est >a.a", sep = "")
    ExeCommand(Com = command2, dir = DirRuns, verbose = verbose)

    # 3.3 Save the results of the estimation
    # Copy the GmacsAll.out for simulation Isim towards GmacsAll_Sim_Isim.out
    namOut <- paste0("gmacsAll_Sim_", Isim, ".out")
    cpy_cmd = ifelse(.Platform$OS.type=="windows","Copy ","cp ");
    command3 <- paste0(cpy_cmd, "gmacsall.out ", namOut)
    ExeCommand(Com = command3, dir = DirRuns, verbose = verbose)
  } # end Nsim

  close(pb)

}
