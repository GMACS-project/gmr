#' @title Compare Gmacs version
#'
#' @description Function to run test of comparison between two code versions of
#' Gmacs.
#'
#' @param stock (character; vector)- the of the stock to be tested. This specified
#' the input file folder names in the \code{dir_test} repertory.
#' @param dir_test (path)- path to the repertory where the output of Gmacs
#' ran for each \code{stock} using the new version will be saved saved. This
#' repertory will be used to run tests and make comparison between Gmacs code
#' version.
#' @param dir_old_version (path)- path to the repertory where the output of Gmacs
#' ran for each \code{stock} using the version to be compared with are saved. The
#' names of the folders in that repertory have to match the ones in the \code{stock}
#' argument.
#' @param dir_OLD_Gmacs_Exe (path)- path to the repertory where the .TPL file of the
#' old Gmacs version is saved. If empty, this is set equal to
#' \code{dirname(dir_old_version)}.
#' @param dir_Gmacs_Exe (path)- path to the repertory where the .TPL file of the
#' new Gmacs version is saved. If empty, this is set equal to \code{dir_test}.
#' @param dir_InputFiles (path)- path to the repertory where the input files for
#' each stock for the newly developed version of Gmacs are saved.
#' @param usePin (logical)- flag to use the `gmacs.pin` as pin file to initialize
#' parameter values when running the new version. This assumes that the `gmacs.par`
#' file structure remained the same between the two versions.
#' @param Run_old_version (logical)- flag to re run the old version of Gmacs using
#' the estimated values for the parameters as a pin file. This will allow to get
#' a new `gmacs.par` file that will be then used for comparison. Default,
#' \code{Run_old_version = TRUE}.
#' @param compareWithPin (logical)- flag to make comparison with the .pin file
#' instead of the gmacs.par file obtained with the version to be compared with.
#' @param verbose;verbose_shell (logical)- flag to print processing information
#' and shell output. Default, \code{verbose;verbose_shell = TRUE}.
#' @param Threshold (numeric)- Threshold to test if the comparison between the
#' two versions passed. It is used to compared parameter values between the two
#' version. by default, \code{Threshold = 1.0e-5}.
#' @param Clean_Files (logical)- flag to delete Gmacs files in stock specific folder
#' after running the new version. See the [clean_bat()] function for more details.
#'
#' @details
#'
#' When \code{Run_old_version = TRUE}, the function assumes that the `gmacs.exe`
#' for the old version is saved in the relative folder of the \code{dir_old_version}
#' folder. If that is not the case, the `gmacs.exe` will be build based on the
#' Gmacs codes available in that folder. See the [createGmacsExe()] function.
#'
#' Using the pin file to initialize a model run (\code{usePin=TRUE}) also sets
#' the starting phase to 10. If all is well, the model should converge almost
#' immediately if the pin file is a copy of a par file from a previous converged
#' model run. Ideally, the model should converge immediately, with the resulting
#' par file identical to the pin file. However, this is frequently not the case
#' if parameter values are near a bound in the pin file and thus the resulting par
#' file values may not be identical to those in the pin file (i.e., the model
#' converged to a slightly different place in parameter space). For the purpose of
#' quickly testing the impact of changes to the code on previous model results, it
#' is probably best to test the par file obtained using the new code run with the
#' pin file against a par file from the old code that was the result of using the
#' same pin file to initialize that model run.
#'
#' @return A list with the results of the comparison. This list is also saved
#' as a `.Rdata` file under the name `Results_testing_version_XXX_[runDate]` in
#' the relative folder of the `dir_test` repertory, i.e., in the `Testing_Versions`
#' folder if the workflow to update/upgrade Gmacs has been followed.
#'
#' @author W. Stockhausen; M. Veron
#'
#' @examples
#' \dontrun{
#' # Load package ----
#' library(gmr)
#'
#' # Set directories ----
#' Dir_Dvpt_Vers <- file.path(here::here(), "Dvpt_Version", fsep = fsep)
#' Dir_Last_Vers <- file.path(here::here(), "Latest_Version", fsep = fsep)
#' dir_test <- file.path(here::here(), "Testing_Versions")
#'
#' # Testing new version of Gmacs ----
#' Res <- CompareCodeVersion(stock = "all",
#' dir_test = dir_test,
#' dir_old_version = file.path(Dir_Last_Vers, 'build', fsep = fsep),
#' dir_new_version = file.path(Dir_Dvpt_Vers, 'build', fsep = fsep),
#' dir_Gmacs_Exe = dir_Gmacs_Exe,
#' dir_InputFiles = dir_InputFiles,
#' dir_OLD_Gmacs_Exe = NULL,
#' Run_old_version = TRUE,
#' usePin = TRUE,
#' compareWithPin = FALSE,
#' verbose = TRUE,
#' Threshold = 1.0e-5,
#' Clean_Files = TRUE
#' )
#' }
#'
#' @export
#' @md
#
CompareCodeVersion <- function(stock = NULL,
                               dir_test = NULL,
                               dir_old_version = NULL,
                               dir_OLD_Gmacs_Exe = NULL,
                               dir_Gmacs_Exe = NULL,
                               dir_InputFiles = NULL,
                               usePin = NULL,
                               Run_old_version = TRUE,
                               compareWithPin = NULL,
                               Threshold = 1.0e-5 ,
                               verbose = TRUE,
                               verbose_shell = TRUE,
                               Clean_Files = NULL) {
  Out <- NULL

  # Internal functions ----

  # Execute a command
  ExeCommand <- function(Com, dir, verbose) {
    tmp <- .CallTerm(command = Com,
                     .Dir = dir,
                     verbose = verbose)

    while (is.null(rstudioapi::terminalExitCode(tmp))) {
      Sys.sleep(0.1)
    }
  }
  # ----------------------------------------------------------------

  # Get the par file as a data frame
  getPar_dataFrame <- function(parData = NULL) {
    Out_par <- NULL
    for (X in 1:length(parData)) {
      if (!is.data.frame(parData[[X]]) && !is.list(parData[[X]])) {
        Out_par <- rbind(Out_par,
                         data.frame(Param_name = names(parData)[X], value = parData[[X]]))
      } else if (is.data.frame(parData[[X]])) {
        Out_par <- rbind(Out_par,
                         data.frame(Param_name = parData[[X]]["Param_name"], value = parData[[X]]["value"]))
      } else if (is.list(parData[[X]])) {
        for (a in 1:length(parData[[X]])) {
          Out_par <- rbind(Out_par,
                           data.frame(Param_name = parData[[X]][[a]]["Param_name"], value = parData[[X]][[a]]["value"]))
        }
      }
    }
    return(Out_par)
  }
  # ----------------------------------------------------------------
  # Function to print warning over a loop
  war <- function(text) {
    suppressWarnings(w <- warning(text))
    message(w)
    # return(text)
  }
  # ----------------------------------------------------------------
  # ----------------------------------------------------------------

  # Check input and set useful stuff
  # verbose <- .an(verbose)

  # Check if the test have to be ran over all stocks
  if (length(stock) == 1 && stock == "all") {
    stock <- list.files(dir_InputFiles)
  }

  # Define the gmacs.exe file depending on the OS
  gmacs_exe <- ifelse(isWindowsOS(), "gmacs.exe", "gmacs")

  # Get the Gmacs exe from dir_test/dir_Gmacs_Exe ----
  # This Exe is the one from the new version
  dir_Gmacs_Exe <- ifelse(is.null(dir_Gmacs_Exe), yes = dir_test, no = dir_Gmacs_Exe)
  # Check if the Exe exists
  if (!file.exists(file.path(dir_Gmacs_Exe, gmacs_exe, fsep = fsep))) {
    if (verbose)
      cat(
        "-- Building the exe for the new version of Gmacs in the following repertory:\n\t",
        dir_Gmacs_Exe,
        "\n"
      )
    ADMBpaths <- ifelse(isWindowsOS(),
                        "ADpaths_Windows.txt",
                        "ADpaths_MacOS.txt")
    createGmacsExe(
      vv = 1,
      Dir = dir_Gmacs_Exe,
      ADMBpaths = file.path(dir_Gmacs_Exe, ADMBpaths, fsep = fsep),
      verbose = verbose_shell
    )

  }

  # Run Gmacs for each stock ----
  # The input files for the run are those that are saved in the "dir_InputFiles/stock"
  # folder. Gmacs output are compared with the estimated parameters from the last
  # available version of Gmacs whose TPL is saved in the dir_old_version.
  # Results of comparison are saved as a .Rdata file in the dir_test folder.
  for (s in 1:length(stock)) {
    if (verbose)
      cat("\n-- Currently testing stock:", stock[s], "\n\n")
    # Set directories
    dirTest_Stock <- file.path(dir_test, stock[s], fsep = fsep)
    dir_oldver_stock <- file.path(dir_old_version, stock[s], fsep = fsep)
    dir_InputFiles_stock <- file.path(dir_InputFiles, stock[s], fsep = fsep)

    # Create repertory for the current stock
    if (!dir.exists(dirTest_Stock)) {
      dir.create(dirTest_Stock, recursive = TRUE)
      if (verbose)
        cat(paste0(
          "-- The following repertory has been created:\n\t",
          dirTest_Stock,
          "\n"
        ))
    }

    # Copy the gmacs input files from the dir_InputFiles folder to the dirTest_Stock folder
    if (verbose)
      cat(
        paste0(
          '\t--> Copying input files from : \n\t',
          dir_InputFiles_stock,
          "\nto:\n\t",
          dirTest_Stock,
          "\n"
        )
      )
    copy_GMACSinputs(
      fromDir = dir_InputFiles_stock,
      toDir = dirTest_Stock,
      GMACS_files = "all",
      overwrite = TRUE,
      verbose = verbose
    )

    # Read all input files from the new version ----
    # Used in the following to write the .par/pin file
    Stock_files <- getInpOutFiles(Dir = dirTest_Stock, verbose = verbose_shell)

    # Check if the pin file created using the old version is used ----
    # This .pin file corresponds to the parameter file and is used to run the two
    # version of Gmacs. The .pin file is created based on the .par file obtained
    # with the old version, assuming that the model converged.
    if (usePin) {
      # Run the old version using the estimated parameters as a pin file?
      if (Run_old_version) {
        # Check if the gmacs.exe exists in the relative folder of dir_old_version
        if (is.null(dir_OLD_Gmacs_Exe)) {
          dir_OLD_Gmacs_Exe <- dirname(dir_old_version)
        }
        if (!file.exists(file.path(dir_OLD_Gmacs_Exe, gmacs_exe, fsep = fsep))) {
          if (verbose)
            cat(
              "-- Building the exe for the old version of Gmacs in the following repertory:\n\t",
              dir_OLD_Gmacs_Exe,
              "\n"

            )
          ADMBpaths <- ifelse(isWindowsOS(),
                              "ADpaths_Windows.txt",
                              "ADpaths_MacOS.txt")
          createGmacsExe(
            vv = 1,
            Dir = dir_OLD_Gmacs_Exe,
            ADMBpaths = file.path(dir_Gmacs_Exe, ADMBpaths, fsep = fsep),
            verbose = verbose_shell
          )
        } # End file.exists gmacs_exe

        # Copy the exe in the stock folder
        test0 <- file.copy(
          file.path(dir_OLD_Gmacs_Exe, gmacs_exe),
          to = dir_oldver_stock,
          overwrite = TRUE,
          recursive = TRUE
        )
        if (!test0) {
          txt0 <- paste0(
            "The gmacs.exe from the old version was not copied to:\n",
            dir_oldver_stock,
            "\n\t-> the old Gmacs version won't be run for :",
            stock[s]
          )
          war(text = txt0)
        }
        # Create the .pin file from the .par file that is assumed
        # to hold the parameter estimates from the old version

        # Test if the .par file exist for the old version
        if (!file.exists(file.path(dir_oldver_stock, "gmacs.par", fsep = fsep))) {
          test1 <- paste(
            "The .par file does not exist in:\n",
            dir_oldver_stock,
            "\n\t-> Can't create the .pin file.\n"
          )
          stop(test1)
        }
        gmacs_par <- readGMACSpar(
          Dir = dir_oldver_stock,
          FileName = "gmacs.par",
          verbose = FALSE,
          DatFile = Stock_files$datFile,
          CtlFile = Stock_files$ctlFile,
          GMACSdat = Stock_files$GMACSdat
        )
        # 3. Write the .pin file in the old version folder
        if (verbose)
          cat(
            "\t--> Using the parameter estimates to write the 'gmacs.pin' file in the following directory:\n\t",
            dir_oldver_stock,
            "\n"
          )
        writeGmacsPAR(Dir = dir_oldver_stock,
                      FileName = "gmacs.pin",
                      gmacsPar = gmacs_par)
        # Run Gmacs without estimation using the gmacs.pin file
        # Set the command to run the model
        # This accounts for the OS system, the verbose and if the .pin file is used
        command <- paste(
          ifelse(
            .Platform$OS.type == "windows",
            gmacs_exe,
            paste0("./", gmacs_exe)
          ),
          "-rs -nox -nohess"
        )
        if (usePin) {
          command <- paste(command, "-phase 10 -pin gmacs.pin ")
        }
        if (verbose > 0) {
          command <- paste(command, "-verbose", verbose)
        }
        if (verbose)
          cat("Running the old Gmacs version using the gmacs.pin file\n")
        ExeCommand(Com = command,
                   dir = dir_oldver_stock,
                   verbose = verbose_shell)
        clean_bat(path = dir_oldver_stock, verbose = verbose)

      } # End if(Run_old_version)

      # Test if the .par file exist for the old version
      if (!file.exists(file.path(dir_oldver_stock, "gmacs.par", fsep = fsep))) {
        test1 <- paste(
          "The .par file does not exist in:\n",
          dir_oldver_stock,
          "\n\t-> Can't create the .pin file.\n"
        )
        stop(test1)
      }

      # Create the .pin file from the .par file obtained after running the old
      # Gmacs version WITHOUT estimation using the parameter estimates.
      # Caution we use the datFile and CtlFile from the new version to read the
      # old .par file, since the new version may have implied changes in these
      # input file. This assumes that the structure of the .par has not been
      # chnaged between the two Gmacs versions
      # read the .par file obtained from the old version without estimation
      gmacs_par <- readGMACSpar(
        Dir = dir_oldver_stock,
        FileName = "gmacs.par",
        verbose = FALSE,
        DatFile = Stock_files$datFile,
        CtlFile = Stock_files$ctlFile,
        GMACSdat = Stock_files$GMACSdat
      )
      # 3. Write the .pin file in the old version folder
      if (verbose)
        cat(
          "\n\t--> Writing the 'gmacs.pin' file in the following directory:\n\t",
          dir_oldver_stock,
          "\n"
        )
      writeGmacsPAR(Dir = dir_oldver_stock,
                    FileName = "gmacs.pin",
                    gmacsPar = gmacs_par)

      # 4. Copy the .pin file to the Test folder
      if (verbose)
        cat(
          "\t--> Copying the 'gmacs.pin' file from:\n\t",
          dir_oldver_stock,
          "\nto\n\t",
          dirTest_Stock,
          "\n"
        )
      copy_GMACSinputs(
        fromDir = dir_oldver_stock,
        toDir = dirTest_Stock,
        GMACS_files = "gmacs.pin",
        overwrite = TRUE,
        verbose = FALSE
      )
    } # End usePin

    # Get the Gmacs exe from dir_test/dir_Gmacs_Exe ----
    # This Exe is the one from the new version
    file.copy(
      file.path(dir_Gmacs_Exe, gmacs_exe),
      to = dirTest_Stock,
      overwrite = TRUE,
      recursive = TRUE
    )

    # Set the command to run the model ----
    # This accounts for the OS system, the verbose and if the .pin file is used
    command <- paste(ifelse(
      .Platform$OS.type == "windows",
      gmacs_exe,
      paste0("./", gmacs_exe)
    ),
    "-rs -nox -nohess")
    if (usePin) {
      command <- paste(command, "-phase 10 -pin gmacs.pin ")
    }
    if (verbose > 0) {
      command <- paste(command, "-verbose", verbose)
    }
    ExeCommand(Com = command,
               dir = dirTest_Stock,
               verbose = verbose_shell)

    # Compare the output with either the .pin file or the .par file from the
    # old version
    if (compareWithPin) {
      old_par <- readGMACSpar(
        Dir = dirTest_Stock,
        FileName = "gmacs.pin",
        verbose = FALSE,
        DatFile = Stock_files$datFile,
        CtlFile = Stock_files$ctlFile,
        GMACSdat = Stock_files$GMACSdat
      )
    } else {
      # Test if the .par file exist for the old version
      if (!file.exists(file.path(dir_oldver_stock, "gmacs.par", fsep = fsep))) {
        test2 <- paste(
          "The .par file does not exist in:\n",
          dir_oldver_stock,
          "\n\t-> Can't make the comparison.\n\n"
        )
        # warning(test2, flush=TRUE)
        war(text = test2)
      } else {
        old_par <- readGMACSpar(
          Dir = dir_oldver_stock,
          FileName = "gmacs.par",
          verbose = FALSE,
          DatFile = Stock_files$datFile,
          CtlFile = Stock_files$ctlFile,
          GMACSdat = Stock_files$GMACSdat
        )
      }
    }
    old_par <- getPar_dataFrame(parData = old_par)

    # Read the parameters from the new version
    new_par <- readGMACSpar(
      Dir = dirTest_Stock,
      FileName = "gmacs.par",
      verbose = FALSE,
      DatFile = Stock_files$datFile,
      CtlFile = Stock_files$ctlFile,
      GMACSdat = Stock_files$GMACSdat
    )
    new_par <- getPar_dataFrame(parData = new_par)

    # Make comparison
    if (nrow(new_par) != nrow(old_par)) {
      test3 <- paste0(
        "Comparing new, old par files: number of rows differ! ",
        nrow(new_par),
        " ",
        nrow(old_par)
      )
      tmp_Out <- list(new_Param = new_par, old_Parm = old_par)

      if (length(which(!new_par$Param_name %in% old_par$Param_name)) > 0) {
        if (verbose)
          cat("The 'diff_par' object holds the names of the parameters that are not in 'old_par'")
        tmp_Out[["diff_par"]] <-
          new_par[which(!new_par$Param_name %in% old_par$Param_name), "Param_name"]
      }
      # warning(test3, flush=TRUE)
      war(text = test3)
    } else {
      tmp_Out <- merge(
        x = new_par,
        y = old_par,
        by = 'Param_name',
        all = TRUE
      )
      colnames(tmp_Out) <- c("Param_name", "new_Param", "old_Param")
      tmp_Out <- tmp_Out %>%
        dplyr::mutate(abs_dif = abs(.an(new_Param) - .an(old_Param))) %>%
        dplyr::mutate(Over = ifelse(.an(abs_dif) > Threshold, TRUE, FALSE))
      if (any(as.logical(tmp_Out$Over))) {
        test4 <- paste0(
          "Detected differences for: \n\t-",
          paste(new_par$Param_name[which(tmp_Out$Over)], collapse = "\n\t-")
        )
        # warning(test4, flush=TRUE)
        war(text = test4)
        cat(
          "\n\nRange of differences:\n",
          range(
            tmp_Out %>% dplyr::filter(Over == TRUE) %>% dplyr::select(abs_dif)
          ),
          "\n"
        )
      } else {
        test4 = paste0("'", stock[s], "' passed!\n")
        cat(test4)
      }
    }
    Out[[stock[s]]] <- tmp_Out

    # Check if cleaning after running
    if (Clean_Files) {
      clean_bat(path = dirTest_Stock, verbose = verbose_shell)
    }
  } # End loop on stocks

  # Save an Rdata and specification of the run
  RunDate <- base::gsub("[[:punct:]*|[:blank:]*]", "-", base::Sys.time(), fixed =
                          FALSE)
  save(Out, file = file.path(
    dirname(dir_test),
    paste0(
      "Results_testing_version_",
      basename(dir_test),
      RunDate,
      ".Rdata"
    ),
    fsep = fsep
  ))
  return(Out)
}
