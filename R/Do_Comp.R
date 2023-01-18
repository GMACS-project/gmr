
#' @title Do_Comp
#'
#' @description For each stock considered in the analysis, this function
#' establishes comparison tables of management quantities between different
#' versions of GMACS.
#'
#' @param Spc vector of strings specifying the name(s) of the stock
#' considered in the analysis.
#' @param GMACS_version vector of strings holding the name(s) of the
#' GMACS versions that is/are used in the analysis.
#' @param ASS Logical. If TRUE, the outputs of the last assessment will be compared
#' to the GMACS version(s) currently used in this analysis.
#' @param AssMod_names vector of strings specifying the names of the models used
#' in the last assessment (e.g., model_16_0)
#' @param Dir vector of strings containing the directories for all
#' \code{GMACS_version} used in this analysis
#' @param compile (0/1). If 0, GMACS is not compiled. This assumes that an
#' executable already exists in the directory of the version(s) used in the analysis.
#' If 1, the code will be compiled before a new run.
#' @param run Logical. If TRUE the model will be executed.
#' @param LastAssDat Logical. If TRUE, the latest available data will be used for
#' the analysis i.e. the model will be executed using the data stored in the
#' the "Assessment_data" folder.
#' @param ADMBpaths string name of 2-column text file that details the relevant
#' paths for the R variables admbpath, gccpath, and editor.
#' @param make.comp Logical. If TRUE, comparisons will be made between the various
#' \code{GMACS_version} considered in the analysis.
#' @param out.pdf Logical. Do you want to get the table of comparison in a pdf format?
#' @inheritParams PBSadmb::convAD
#'
#' @seealso \code{\link{Do_GMACS}}, \code{.buildGMACS} for
#' building the executable.
#'
#' @return For each stock considered in the analysis, this function returns a
#' table that compares management quantities between different version of GMACS.
#'
#' @export
#'
#'
# Do_Comp <-
#   function(Spc = NULL,
#            GMACS_version = NULL,
#            ASS = NULL,
#            AssMod_names  = NULL,
#            Dir = NULL,
#            compile = NULL,
#            run = NULL,
#            LastAssDat = NULL,
#            ADMBpaths = NULL,
#            make.comp = NULL,
#            verbose = NULL) {
#
#     # .MODELDIR <- Dir
#     addDir <- matrix(data = "",
#                      nrow = length(Dir),
#                      ncol = length(Spc))
#     base.file <-
#       matrix(data = "",
#              nrow = length(Dir),
#              ncol = length(Spc))
#     if (ASS) {
#       pat <- Dir[which(grep(pattern = "Assessments", x = Dir) == 1)]
#       Dir[!Dir == pat] <- file.path(Dir[!Dir == pat], "build")
#       for (vv in 1:length(Dir))
#         if (Dir[vv] == pat) {
#           addDir[vv, ] <- AssMod_names
#         }
#     } else {
#       Dir <- file.path(Dir, "build")
#       # addDir <- rep("", length(Spc))
#     }
#
#     ScenarioNames <- GMACS_version
#
#     # Check existence files
#     ex.rep <- matrix(NA, nrow = length(Dir), ncol = length(Spc))
#     Need.run <- 0
#     for (vv in 1:length(Dir)){
#       for (nm in 1:length(Spc)) {
#         pth = file.path(Dir[vv], Spc[nm], addDir[vv, nm]);
#         cat("checking path: ",pth,"\n")
#         if (!file.exists(file.path(pth, "gmacs.rep")) &&
#             !file.exists(file.path(pth, "Gmacsall.out"))) {
#           cat("Did not find either gmacs.rep or Gmacsall.out\n")
#           ex.rep[vv, nm] <- 0
#           Need.run <- 1
#         }
#         if (file.exists(file.path(pth, "Gmacsall.out"))){
#           cat("\tFound Gmacsall.out\n")
#           base.file[vv, nm] <- 0
#         } else {
#             if (file.exists(file.path(pth, "gmacs.rep"))){
#               cat("\tFound gmacs.rep\n");
#               base.file[vv, nm] <- 1
#             }
#         }
#       }
#     }
#
#     if (Need.run == 1 && !ASS) {
#       for (vv in 1:length(Dir)){
#         if (length(which(ex.rep[vv, ] == 0)) > 0)
#           cat("gmacs.rep is missing for ",
#               paste0(Spc[which(ex.rep[vv, ] == 0)], collapse = ", "),
#              " for the ",GMACS_version[vv]," of GMACS.\n",sep="");
#       }
#       do.run <- NA
#
#       while (is.na(do.run)) {
#         text = "gmacs.rep is missing for one or serveral version of GMACS for one or several species.
#       \nPlease consider running GMACS for this/these version(s) and species so you can make comparison between versions.
#       \nDo you want to run it now? (Y/N)\n"
#         do.run <- svDialogs::dlgInput(text, Sys.info())$res
#         # do.run <-readline(prompt="gmacs.rep is missing one or serveral vaersion of GMACS for one or several species.\n
#         #          Please consider running GMACS for this/these version(s) and species so you can make a comparison.\n
#         #         Do you want to run it now? (Y/N)\n")
#         Sys.sleep(0.1)
#       }
#
#       if (do.run == "Y") {
#         GMACS_OUT <-
#           gmr::Do_GMACS(
#             Spc = Spc,
#             GMACS_version = GMACS_version,
#             ASS = FALSE,
#             AssMod_names  = NULL,
#             Dir = Dir,
#             compile = compile,
#             run = run,
#             LastAssDat = LastAssDat,
#             ADMBpaths = ADMBpaths,
#             make.comp = make.comp,
#             verbose = verbose
#           )
#         gmr::Do_Comp(                      #<-TODO: this is a recursive call: shiould it be recursive??
#           Spc = Spc,
#           GMACS_version = GMACS_version,
#           ASS = ASS,
#           AssMod_names  = AssMod_names,
#           Dir = Dir,
#           compile = compile,
#           run = run,
#           LastAssDat = LastAssDat,
#           ADMBpaths = ADMBpaths,
#           make.comp = make.comp,
#           verbose = verbose
#         )
#         if (!is.null(GMACS_OUT))
#           return(GMACS_OUT)
#         break()
#       }
#       stop("No comparison is made. The execution has been halted.")
#     }
#
#
#     if (Need.run == 1 && ASS) {
#       for (vv in 1:length(Dir))
#         if (length(which(ex.rep[vv, ] == 0)) > 0)
#           cat(
#             "gmacs.rep is missing for ",
#             paste0(Spc[which(ex.rep[vv, ] == 0)], collapse = ", "),
#             " for the ",
#             GMACS_version[vv],
#             " of GMACS.\n"
#           )
#       stop("No comparison is possible.")
#     }
#
#     # Build comparison tables
#     for (nm in 1:length(Spc)) {
#       # nm = 1
#
#       cat("\n\n\\pagebreak\n")
#       cat("\n\n\\# Comparison of ",
#           Spc[nm],
#           " for ",
#           length(Dir),
#           " version",ifelse(length(Dir)==1,"","s")," of GMACS. \n",sep="")
#       cat("\n\n\\                                                                 \n")
#       cat("\n\n\\                                                                 \n")
#
#
#       # cat("\n\n\\# This is the summary of management quantities for: ",Spc,"\n")
#
#       Mfile <- unique(.an(base.file[, nm]))
#       PlotTab <- data.frame(
#         Model = ScenarioNames,
#         MMB = rep(0, length(ScenarioNames)),
#         B35 = rep(0, length(ScenarioNames)),
#         F35 = rep(0, length(ScenarioNames)),
#         FOFL = rep(0, length(ScenarioNames)),
#         OFL = rep(0, length(ScenarioNames)),
#         Status = rep(0, length(ScenarioNames)),
#         M = rep(0, length(ScenarioNames)),
#         Av_Recr = rep(0, length(ScenarioNames))
#       )
#
#       if (Mfile == 0 || length(Mfile) > 1) {
#         # fn       <- ifelse(test = base.file[,nm]==0, paste0(Dir, Spc[nm], addDir[,nm], "/gmacsall.OUT"), paste0(Dir, Spc[nm], addDir[,nm],"/gmacs"))
#
#         for (vv in 1:length(Dir)) {
#           #--vv=1;
#           M <- NULL;
#
#           if (base.file[vv, nm] == 0) {
#             fn       <- file.path(Dir[vv], Spc[nm], addDir[vv, nm], "Gmacsall.out");
#             M[[vv]] <- read.OUT(fn)
#           } else {
#             tmp <- NULL
#             fn <- file.path(Dir[vv], Spc[nm], addDir[vv, nm], "gmacs")
#             tmp[[vv]] <- gmr::read_admb(fn)
#             M[[vv]]$MMB <- tmp[[vv]]$ssb[length(tmp[[vv]]$ssb)]
#             M[[vv]]$B35 <- tmp[[vv]]$spr_bmsy
#             M[[vv]]$F35 <- tmp[[vv]]$sd_fmsy[1]
#             M[[vv]]$FOFL <- tmp[[vv]]$sd_fofl[1]
#             M[[vv]]$OFL <- tmp[[vv]]$spr_cofl
#             M[[vv]]$Status <- M[[vv]]$MMB / M[[vv]]$B35
#             M[[vv]]$M <- mean(tmp[[vv]]$M[1,])
#             M[[vv]]$Av_Recr <- mean(tmp[[vv]]$recruits[1,]) / 10000
#           }
#
#           PlotTab$MMB[vv] <- M[[vv]]$MMB
#           PlotTab$B35[vv] <- M[[vv]]$B35
#           PlotTab$F35[vv] <- M[[vv]]$F35
#           PlotTab$FOFL[vv] <- M[[vv]]$FOFL
#           PlotTab$OFL[vv] <- M[[vv]]$OFL
#           PlotTab$Status[vv] <- M[[vv]]$Status
#           PlotTab$M[vv] <- M[[vv]]$M
#           PlotTab$Av_Recr[vv] <- M[[vv]]$Av_Recr
#         }#--vv
#       }#--if (Mfile == 0 || length(Mfile) > 1)
#
#       if (unique(.an(base.file)) == 1) {
#         fn       <- file.path(Dir, Spc[nm], "gmacs")
#         M        <-
#           lapply(fn, gmr::read_admb) #need .prj file to run gmacs and need .rep file here
#         names(M) <- ScenarioNames
#         for (x in 1:length(M))
#         {
#           PlotTab$MMB[x] <- M[[x]]$ssb[length(M[[x]]$ssb)]
#           PlotTab$B35[x] <- M[[x]]$spr_bmsy
#           PlotTab$F35[x] <- M[[x]]$sd_fmsy[1]
#           PlotTab$FOFL[x] <- M[[x]]$sd_fofl[1]
#           PlotTab$OFL[x] <- M[[x]]$spr_cofl
#           PlotTab$Status[x] <- PlotTab$MMB[x] / PlotTab$B35[x]
#           PlotTab$Nat.M[x] <- mean(M[[x]]$M)
#           PlotTab$Av_Recr[x] <- mean(M[[x]]$recruits) / 10000
#         }
#       }
#
#       rownames(PlotTab) <- NULL
#       PlotTab[, c(2:dim(PlotTab)[2])] <-
#         round(PlotTab[, c(2:dim(PlotTab)[2])], 3)
#       print(
#         knitr::kable(
#           PlotTab[, 1:dim(PlotTab)[2]],
#           split.cells = c(25, rep(7, 5)),
#           justify = c("left", rep("center", 5)),
#           caption = "\\label{stepchange}Changes in management quantities for each scenario considered.
#         Reported management quantities are derived from maximum likelihood estimates."
#         )
#       )
#     }
#     return("Comparison done.")
#   }
Do_Comp <-
  function(Spc = NULL,
           GMACS_version = NULL,
           ASS = NULL,
           AssMod_names  = NULL,
           Dir = NULL,
           compile = NULL,
           run = NULL,
           LastAssDat = NULL,
           ADMBpaths = NULL,
           make.comp = NULL,
           verbose = NULL,
           out.pdf = FALSE) {
    # .MODELDIR <- Dir
    addDir <- matrix(data = "",
                     nrow = length(Dir),
                     ncol = length(Spc))
    base.file <-
      matrix(data = "",
             nrow = length(Dir),
             ncol = length(Spc))
    if (ASS) {
      pat <- Dir[which(grep(pattern = "Assessments", x = Dir) == 1)]
      Dir[!Dir == pat] <- file.path(Dir[!Dir == pat], "build")
      for (vv in 1:length(Dir))
        if (Dir[vv] == pat) {
          addDir[vv,] <- AssMod_names
        }
    } else {
      Dir <- file.path(Dir, "build")
      # addDir <- rep("", length(Spc))
    }

    ScenarioNames <- GMACS_version

    # Check existence files
    ex.rep <- matrix(NA, nrow = length(Dir), ncol = length(Spc))
    Need.run <- 0
    for (vv in 1:length(Dir)) {
      for (nm in 1:length(Spc)) {
        pth = file.path(Dir[vv], Spc[nm], addDir[vv, nm])

        cat("checking path: ", pth, "\n")
        if (!file.exists(file.path(pth, "gmacs.rep")) &&
            !file.exists(file.path(pth, "Gmacsall.out"))) {
          cat("Did not find either gmacs.rep or Gmacsall.out\n")
          ex.rep[vv, nm] <- 0
          Need.run <- 1
        }
        if (file.exists(file.path(pth, "Gmacsall.out"))) {
          cat("\tFound Gmacsall.out\n")
          base.file[vv, nm] <- 0
        } else {
          if (file.exists(file.path(pth, "gmacs.rep"))) {
            cat("\tFound gmacs.rep\n")

            base.file[vv, nm] <- 1
          }
        }
      }
    }

    if (Need.run == 1 && !ASS) {
      for (vv in 1:length(Dir)) {
        if (length(which(ex.rep[vv,] == 0)) > 0)
          cat(
            "gmacs.rep is missing for ",
            paste0(Spc[which(ex.rep[vv,] == 0)], collapse = ", "),
            " for the ",
            GMACS_version[vv],
            " of GMACS.\n",
            sep = ""
          )

      }
      do.run <- NA

      while (is.na(do.run)) {
        text = "gmacs.rep is missing for one or serveral version of GMACS for one or several species.
      \nPlease consider running GMACS for this/these version(s) and species so you can make comparison between versions.
      \nDo you want to run it now? (Y/N)\n"
        do.run <- svDialogs::dlgInput(text, Sys.info())$res
        # do.run <-readline(prompt="gmacs.rep is missing one or serveral vaersion of GMACS for one or several species.\n
        #          Please consider running GMACS for this/these version(s) and species so you can make a comparison.\n
        #         Do you want to run it now? (Y/N)\n")
        Sys.sleep(0.1)
      }

      if (do.run == "Y") {
        GMACS_OUT <-
          gmr::Do_GMACS(
            Spc = Spc,
            GMACS_version = GMACS_version,
            ASS = FALSE,
            AssMod_names  = NULL,
            Dir = Dir,
            compile = compile,
            run = run,
            LastAssDat = LastAssDat,
            ADMBpaths = ADMBpaths,
            make.comp = make.comp,
            verbose = verbose
          )
        gmr::Do_Comp(
          #<-TODO: this is a recursive call: shiould it be recursive??
          Spc = Spc,
          GMACS_version = GMACS_version,
          ASS = ASS,
          AssMod_names  = AssMod_names,
          Dir = Dir,
          compile = compile,
          run = run,
          LastAssDat = LastAssDat,
          ADMBpaths = ADMBpaths,
          make.comp = make.comp,
          verbose = verbose
        )
        if (!is.null(GMACS_OUT))
          return(GMACS_OUT)
        break()
      }
      stop("No comparison is made. The execution has been halted.")
    }


    if (Need.run == 1 && ASS) {
      for (vv in 1:length(Dir))
        if (length(which(ex.rep[vv,] == 0)) > 0)
          cat(
            "gmacs.rep is missing for ",
            paste0(Spc[which(ex.rep[vv,] == 0)], collapse = ", "),
            " for the ",
            GMACS_version[vv],
            " of GMACS.\n"
          )
      stop("No comparison is possible.")
    }

    # Build comparison tables
    out <- NULL

    for (nm in 1:length(Spc)) {
      # nm = 1

      if (out.pdf) {
        cat("\n\n\\pagebreak\n")
        cat(
          "\n\n\\# Comparison of ",
          Spc[nm],
          " for ",
          length(Dir),
          " version",
          ifelse(length(Dir) == 1, "", "s"),
          " of GMACS. \n",
          sep = ""
        )
        cat("\n\n\\                                                                 \n")
        cat("\n\n\\                                                                 \n")
      }

      Mfile <- unique(.an(base.file[, nm]))
      PlotTab <- data.frame(
        Model = ScenarioNames,
        MMB = rep(0, length(ScenarioNames)),
        B35 = rep(0, length(ScenarioNames)),
        F35 = rep(0, length(ScenarioNames)),
        FOFL = rep(0, length(ScenarioNames)),
        OFL = rep(0, length(ScenarioNames)),
        Status = rep(0, length(ScenarioNames)),
        M = rep(0, length(ScenarioNames)),
        Av_Recr = rep(0, length(ScenarioNames))
      )

      if (Mfile == 0 || length(Mfile) > 1) {
        # fn       <- ifelse(test = base.file[,nm]==0, paste0(Dir, Spc[nm], addDir[,nm], "/gmacsall.OUT"), paste0(Dir, Spc[nm], addDir[,nm],"/gmacs"))

        for (vv in 1:length(Dir)) {
          #--vv=1;
          M <- NULL


          if (base.file[vv, nm] == 0) {
            fn       <-
              file.path(Dir[vv], Spc[nm], addDir[vv, nm], "Gmacsall.out")

            M[[vv]] <- read.OUT(fn)
          } else {
            tmp <- NULL
            fn <-
              file.path(Dir[vv], Spc[nm], addDir[vv, nm], "gmacs")
            tmp[[vv]] <- gmr::read_admb(fn)
            M[[vv]]$MMB <- tmp[[vv]]$ssb[length(tmp[[vv]]$ssb)]
            M[[vv]]$B35 <- tmp[[vv]]$spr_bmsy
            M[[vv]]$F35 <- tmp[[vv]]$sd_fmsy[1]
            M[[vv]]$FOFL <- tmp[[vv]]$sd_fofl[1]
            M[[vv]]$OFL <- tmp[[vv]]$spr_cofl
            M[[vv]]$Status <- M[[vv]]$MMB / M[[vv]]$B35
            M[[vv]]$M <- mean(tmp[[vv]]$M[1, ])
            M[[vv]]$Av_Recr <- mean(tmp[[vv]]$recruits[1, ]) / 10000
          }

          PlotTab$MMB[vv] <- M[[vv]]$MMB
          PlotTab$B35[vv] <- M[[vv]]$B35
          PlotTab$F35[vv] <- M[[vv]]$F35
          PlotTab$FOFL[vv] <- M[[vv]]$FOFL
          PlotTab$OFL[vv] <- M[[vv]]$OFL
          PlotTab$Status[vv] <- M[[vv]]$Status
          PlotTab$M[vv] <- M[[vv]]$M
          PlotTab$Av_Recr[vv] <- M[[vv]]$Av_Recr
        }#--vv
      }#--if (Mfile == 0 || length(Mfile) > 1)

      if (unique(.an(base.file)) == 1) {
        fn       <- file.path(Dir, Spc[nm], "gmacs")
        M        <-
          lapply(fn, gmr::read_admb) #need .prj file to run gmacs and need .rep file here
        names(M) <- ScenarioNames
        for (x in 1:length(M))
        {
          PlotTab$MMB[x] <- M[[x]]$ssb[length(M[[x]]$ssb)]
          PlotTab$B35[x] <- M[[x]]$spr_bmsy
          PlotTab$F35[x] <- M[[x]]$sd_fmsy[1]
          PlotTab$FOFL[x] <- M[[x]]$sd_fofl[1]
          PlotTab$OFL[x] <- M[[x]]$spr_cofl
          PlotTab$Status[x] <- PlotTab$MMB[x] / PlotTab$B35[x]
          PlotTab$Nat.M[x] <- mean(M[[x]]$M)
          PlotTab$Av_Recr[x] <- mean(M[[x]]$recruits) / 10000
        }
      }

      if (out.pdf) {
        rownames(PlotTab) <- NULL
        PlotTab[, c(2:dim(PlotTab)[2])] <-
          round(PlotTab[, c(2:dim(PlotTab)[2])], 3)
        print(
          knitr::kable(
            PlotTab[, 1:dim(PlotTab)[2]],
            split.cells = c(25, rep(7, 5)),
            justify = c("left", rep("center", 5)),
            caption = "\\label{stepchange}Changes in management quantities for each scenario considered.
        Reported management quantities are derived from maximum likelihood estimates."
          )
        )
      }
      if (length(unlist(stringr::str_split(
        string = Spc[nm], pattern = "/"
      ))) > 1) {
        tmpSpc <-
          unlist(stringr::str_split(string = Spc[nm], pattern = "/"))[2]
      } else {
        tmpSpc <- Spc[nm]
      }
      eval(parse(text = paste(tmpSpc, " <- NULL", sep = "")))

      for (v in 1:length(ScenarioNames)) {
        # v<-1
        tmpSc <- ScenarioNames[v]

        for (n in 2:length(names(PlotTab))) {
          # n <- 2

          eval(parse(
            text = paste(
              tmpSpc,
              "$",
              tmpSc,
              "$",
              names(PlotTab)[n],
              " <- PlotTab$",
              names(PlotTab)[n],
              "[",
              v,
              "]",
              sep = ""
            )
          ))

        }
      }
      eval(parse(text = paste("out$", tmpSpc, "<-", tmpSpc, sep = "")))
    }
    cat("Comparison done.")
    return(out)
  }
